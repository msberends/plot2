# ===================================================================== #
#  An R package for Fast 'ggplot2' Plotting:                            #
#  https://github.com/msberends/plot2                                   #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

# Single argument definition (F035) ------------------------------------
#
# The arguments of plot2() are defined once, in the generic in R/plot2.R. The methods in
# R/plot2-methods.R and plot2_exec() are written as `function(.data, ...)` and receive the
# generic's arguments here, with their method-specific defaults. Each method forwards all
# arguments with `eval(plot2_exec_call())`, which builds the call to plot2_exec() and is
# evaluated in the method's own frame, so that `{{ }}` and missing() refer to the user's call.
#
# This file is named zzz.R so that it is collated after all other files.

# arguments forwarded as quosures, since they may refer to columns
plot2_nse_args <- c("x", "y", "category", "facet",
                    "x.title", "y.title", "category.title", "legend.title",
                    "title", "subtitle", "caption", "tag",
                    "y_secondary", "y_secondary.title", "datalabels")

# arguments whose absence plot2_exec() needs to know
plot2_misses_args <- c("x", "y", "category", "facet", "datalabels", "colour_fill",
                       "x.title", "y.title", "title", "subtitle", "tag", "caption",
                       "y.percent", "y.percent_break", "x.zoom", "x.max_items",
                       "facet.fixed_x", "summarise_function")

# arguments whose default title is derived from the user's code
plot2_label_args <- c("x", "y", "category", "facet", "y_secondary")

plot2_arg_names <- function() {
  setdiff(names(formals(plot2)), c(".data", "data", "..."))
}

#' Formals of a plot2() method
#'
#' The generic's formals, with method-specific defaults in `...` (as expressions, e.g. `x = quote(.data$item)`) and method-only arguments in `.extra`, placed before `...`.
#' @noRd
plot2_method_formals <- function(..., .extra = list(), .data_default = quote(expr = )) {
  f <- formals(plot2)
  f$data <- NULL
  f$`...` <- NULL
  f[".data"] <- list(.data_default)
  overrides <- list(...)
  unknown <- setdiff(names(overrides), names(f))
  if (length(unknown) > 0) {
    stop("unknown arguments in method defaults: ", toString(unknown), call. = FALSE)
  }
  for (n in names(overrides)) {
    f[n] <- list(overrides[[n]])
  }
  c(f, .extra, alist(... = ))
}

#' Call from a plot2() method to plot2_exec()
#'
#' Returns the call that forwards every argument of the method to `plot2_exec()`. Evaluate it in the method's frame with `eval(plot2_exec_call())`. Named arguments in `...` replace parts of the call (as expressions or values), e.g. `.data = quote(new_df)` or `` `_label.x` = "item" ``.
#' @noRd
plot2_exec_call <- function(...) {
  cl <- plot2_session$exec_call
  if (is.null(cl)) {
    cl <- list(quote(plot2_exec), .data = quote(.data))
    for (n in plot2_arg_names()) {
      cl[[n]] <- if (n %in% plot2_nse_args) call("{", call("{", as.name(n))) else as.name(n)
    }
    for (n in plot2_misses_args) {
      cl[[paste0("_misses.", n)]] <- call("missing", as.name(n))
    }
    cl$`_misses.y_secondary.colour` <- quote(missing(y_secondary.colour) && missing(y_secondary.colour_fill))
    for (n in plot2_label_args) {
      cl[[paste0("_label.", n)]] <- bquote(plot2_label(rlang::enquo(.(as.name(n)))))
    }
    cl$`_summarise_fn_name` <- quote(plot2_label(rlang::enquo(summarise_function)))
    cl <- c(cl, list(quote(...)))
    plot2_session$exec_call <- cl
  }
  overrides <- list(...)
  for (n in names(overrides)) {
    cl[n] <- list(overrides[[n]])
  }
  as.call(cl)
}

#' Default title of an argument
#'
#' Derives a title from the code the user wrote, after resolving `{{ }}`, `!!` and `.data` pronouns, so that `{{ col }}`, `.data[[v]]` and `!!sym(v)` in functions and loops give the column name (F105).
#' @noRd
plot2_label <- function(quo) {
  # a missing argument gives the code of its default, e.g. "NULL"
  expr <- resolve_data_pronoun(rlang::quo_get_expr(quo), env = rlang::quo_get_env(quo))
  if (rlang::is_quosure(expr)) {
    expr <- rlang::quo_squash(expr)
  }
  if (is.character(expr) && length(expr) == 1) {
    return(expr)
  }
  paste(trimws(deparse(rlang::quo_squash(expr), width.cutoff = 500L)), collapse = " ")
}

resolve_data_pronoun <- function(expr, env) {
  if (rlang::is_quosure(expr)) {
    return(resolve_data_pronoun(rlang::quo_get_expr(expr), rlang::quo_get_env(expr)))
  }
  if (!is.call(expr)) {
    return(expr)
  }
  fn <- expr[[1]]
  if ((identical(fn, as.name("[[")) || identical(fn, as.name("$"))) &&
      length(expr) == 3 && identical(expr[[2]], as.name(".data"))) {
    nm <- expr[[3]]
    if (identical(fn, as.name("[["))) {
      nm <- tryCatch(eval(nm, envir = env), error = function(e) NULL)
    }
    if (is.name(nm)) {
      return(nm)
    }
    if (is.character(nm) && length(nm) == 1) {
      return(as.name(nm))
    }
    return(expr)
  }
  for (i in seq_along(expr)[-1]) {
    if (!is.null(expr[[i]]) && !identical(expr[[i]], quote(expr = ))) {
      expr[[i]] <- resolve_data_pronoun(expr[[i]], env)
    }
  }
  expr
}

#' Formals of the sf method
#'
#' A map has no axes, grid or axis titles by default.
#' @importFrom ggplot2 element_blank unit
#' @noRd
plot2_sf_formals <- function() {
  plot2_method_formals(x.title = FALSE,
                       y.title = FALSE,
                       colour = quote(getOption("plot2.colour_sf", "grey50")),
                       colour_fill = quote(getOption("plot2.colour_sf_fill", getOption("plot2.colour", "ggplot2"))),
                       x.expand = 0,
                       y.expand = 0,
                       datalabels.format = NULL,
                       datalabels.colour = "black",
                       legend.position = "right",
                       theme = quote(theme_minimal2(panel.grid.major = element_blank(),
                                                    panel.grid.minor = element_blank(),
                                                    panel.border = element_blank(),
                                                    plot.margin = unit(c(5, 5, 0, 0), units = "pt"),
                                                    axis.title = element_blank(),
                                                    axis.text = element_blank(),
                                                    axis.line = element_blank(),
                                                    axis.ticks = element_blank())),
                       .extra = alist(crs = NULL, datalabels.centroid = NULL))
}

# formals of the methods and of plot2_exec() ----
formals(plot2.default) <- plot2_method_formals()
formals(plot2.formula) <- plot2_method_formals(.data_default = NULL, .extra = alist(data = NULL))
formals(plot2.freq) <- plot2_method_formals(x = quote(.data$item),
                                            y = quote(.data$count),
                                            x.title = "Item",
                                            y.title = "Count",
                                            category.title = TRUE,
                                            x.sort = "freq-desc")
formals(plot2.sf) <- plot2_sf_formals()
formals(plot2.data.frame) <- plot2_method_formals()
formals(plot2.matrix) <- plot2_method_formals(x.title = FALSE,
                                              y.title = FALSE)
formals(plot2_exec) <- plot2_method_formals()

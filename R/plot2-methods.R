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

#' Methods for [plot2()]
#' 
#' These are the implemented methods for different S3 classes to be used in [plot2()]. Since they have an extensive list of arguments, they are placed here on a separate manual page.
#' @details
#' The methods differ from the data frame method as follows:
#' 
#' - **Vectors and other objects** (`plot2.default()`): the input is converted to a data frame first. Character and factor values are counted. A single character value that is the name of a data frame is replaced by that data frame.
#' - **Formulas** (`plot2.formula()`): `y ~ x` takes the variables from `data`, e.g. `plot2(Sepal.Length ~ Species, data = iris)`.
#' - **Frequency tables** (`plot2.freq()`, from [cleaner::freq()]): `x` and `y` default to the items and their counts, the axis titles to "Item" and "Count", and `x.sort` to `"freq-desc"`.
#' - **Spatial data** (`plot2.sf()`, from the `sf` package): plots a map, without axis titles, axis expansion, grid or axes; the borders use the `plot2.colour_sf` option (default `"grey50"`), the fill uses `plot2.colour_sf_fill`, and the legend is on the right. The extra arguments `crs` and `datalabels.centroid` apply only here.
#' - **Matrices** (`plot2.matrix()`): the row names (or row numbers) become `x` and every column becomes a `category`. A matrix with identical row and column names, such as the outcome of [cor()], is plotted as a heatmap (`type = "tile"`). The axis titles are left out, unless the dimensions are named (e.g. `dimnames = list(region = ..., year = ...)`); these names are then used as titles of x and of the category.
#' @return A [`ggplot`][ggplot2::ggplot()] object.
#' @rdname plot2-methods
#' @name plot2-methods
#' @inheritParams plot2
#' @importFrom dplyr count filter
#' @export
#' @examples
#' # vectors: character values are counted
#' plot2(c("a", "b", "b", "c", "c", "c"))
#' 
#' # formulas
#' plot2(Sepal.Length ~ Species, data = iris)
#' 
#' # frequency tables
#' if (requireNamespace("cleaner", quietly = TRUE)) {
#'   plot2(cleaner::freq(iris$Species))
#' }
#' 
#' # correlation matrices become a heatmap
#' plot2(cor(mtcars[, 1:5]))
#' 
#' # spatial data
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   plot2(netherlands)
#' }
plot2.default <- function(.data, ...) {
  # arguments: see plot2_method_formals() in R/zzz.R (F035)
  # per-call state and diagnostics, restored or emitted on exit (F001, F014)
  call_state_enter()
  on.exit(call_state_exit(), add = TRUE)
  if (is.function(.data)) {
    stop("`plot2()` does not yet support functions to be plotted", call. = FALSE)
  }
  
  # the name of a data frame in the caller's environment, e.g. plot2("df"), as used by
  # tool calling (ellmer::tool()); only the caller's own environment is searched, not the
  # search path or plot2's namespace (F030)
  if (is.character(.data) && length(.data) == 1 && !is.na(.data) &&
      exists(.data, envir = parent.frame(), inherits = FALSE) &&
      is.data.frame(get(.data, envir = parent.frame(), inherits = FALSE))) {
    .data <- get(.data, envir = parent.frame(), inherits = FALSE)
  }
  
  if (tryCatch(suppressWarnings(is.atomic(.data) && !is.table(.data) && !is.matrix(.data)), error = function(e) FALSE)) {
    # an atomic vector, such as numeric, character, factor
    y_deparse <- paste0(trimws(deparse(substitute(.data))), collapse = " ")
    if (nchar(y_deparse) > 30) {
      # fall back to "x", never to the name of the count column "y" (F072)
      y_deparse <- "x"
    }
    y_given <- !missing(y)
    if (missing(.data)) {
      .data <- y
      y_deparse <- "y"
      y_given <- FALSE
    }
    if (is.character(.data) || is.factor(.data)) {
      new_df <- as.data.frame(table(.data))
      colnames(new_df) <- c(ifelse(y_deparse == "y", "x", y_deparse), "y")
      new_df <- new_df |> filter(y != 0)
    } else if (is.numeric(.data) && !y_given) {
      new_df <- data.frame(x = seq_len(length(.data)),
                           y = .data, stringsAsFactors = FALSE)
    } else if (is.numeric(.data) && missing(x)) {
      # the vector is x, the values of y are given (F030)
      new_df <- data.frame(x = .data, stringsAsFactors = FALSE)
    } else if (!is.numeric(.data) && missing(x)) {
      new_df <- data.frame(x = .data, stringsAsFactors = FALSE) |> count(x, name = "y")
    } else {
      stop("For a vector, `x` cannot be set; put the values in a data frame instead", call. = FALSE)
    }
    if (is.null(type)) {
      type <- getOption("plot2.default_type", "geom_col")
    }
    
  } else {
    fortified <- fortify_df(.data, rlang::enquo(y))
    new_df <- fortified$new_df
    y <- fortified$y
  }
  
  eval(plot2_exec_call(.data = quote(new_df)))
}

#' @rdname plot2-methods
#' @export
plot2.formula <- function(.data, ...) {
  # arguments: see plot2_method_formals() in R/zzz.R (F035)
  # per-call state and diagnostics, restored or emitted on exit (F001, F014)
  call_state_enter()
  on.exit(call_state_exit(), add = TRUE)
  if (is.null(data)) {
    stop("Formula notation requires the `data` argument to be set, e.g. `plot2(mpg ~ wt, data = mtcars)` or `mtcars |> plot2(mpg ~ wt)`", call. = FALSE)
  }
  form <- .data
  .data <- data
  # walk the formula as a call tree, so that terms such as log(y) or I(x + 1) are kept (F019)
  terms <- formula_terms(form)
  as_term <- function(e) if (is.null(e)) NULL else rlang::new_quosure(e, env = rlang::f_env(form))
  x <- as_term(terms$x)
  y <- as_term(terms$y)
  category <- as_term(terms$category)
  facet <- as_term(terms$facet)
  term_label <- function(e) if (is.null(e)) "NULL" else paste(deparse(e), collapse = " ")
  
  eval(plot2_exec_call(`_label.x` = quote(term_label(terms$x)),
                       `_label.y` = quote(term_label(terms$y)),
                       `_label.category` = quote(term_label(terms$category)),
                       `_label.facet` = quote(term_label(terms$facet))))
  
}

#' @rdname plot2-methods
#' @export
plot2.freq <- function(.data, ...) {
  # arguments: see plot2_method_formals() in R/zzz.R (F035)
  # per-call state and diagnostics, restored or emitted on exit (F001, F014)
  call_state_enter()
  on.exit(call_state_exit(), add = TRUE)
  # legend.title replaces the method's default category.title (D001.12)
  if (missing(category.title) && !missing(legend.title)) {
    category.title <- NULL
  }
  eval(plot2_exec_call(.data = quote(as.data.frame(.data, stringsAsFactors = FALSE)[, 1:2, drop = FALSE]),
                       x = quote(x),
                       y = quote(y),
                       `_label.x` = "item",
                       `_label.y` = "count"))
}

#' @rdname plot2-methods
#' @details For geographic information system (GIS) analysis, use the `sf` package with a data set containing geometries. The result can be used as input for [plot2()].
#' @param crs The coordinate reference system (CRS) to use. If this is not left blank, [sf::st_transform()] will be used to transform the geometric data to the new CRS.
#' @param datalabels.centroid A [logical] to indicate whether datalabels must be centred on the polygon (using [sf::st_centroid()], the default), or be placed on the 'best' spot on the surface (using [sf::st_point_on_surface()]).
#' @export
plot2.sf <- function(.data, ...) {
  # arguments: see plot2_method_formals() in R/zzz.R (F035)
  # per-call state and diagnostics, restored or emitted on exit (F001, F014)
  call_state_enter()
  on.exit(call_state_exit(), add = TRUE)
  
  rlang::check_installed("sf")
  loadNamespace("sf")
  
  if (!inherits(.data, "sf")) {
    plot2_caution("Transforming plot data to an sf model using ", font_blue("sf::st_as_sf()"))
    .data <- sf::st_as_sf(.data)
  }
  if (!is.null(crs)) {
    .data <- sf::st_transform(.data, crs = crs)
  }
  
  # use missing(), since x and y are column names that cannot be evaluated here (F080)
  if (!missing(x)) {
    plot2_caution("In 'sf' plots, ", font_blue("x"), " will be ignored - did you mean ", font_blue("category"), "?")
  }
  if (!missing(y)) {
    plot2_caution("In 'sf' plots, ", font_blue("y"), " will be ignored - did you mean ", font_blue("category"), "?")
  }
  colour_fill_default <- getOption("plot2.colour_sf_fill", getOption("plot2.colour", "ggplot2"))
  if (!missing(colour) && identical(colour_fill, colour_fill_default) && !identical(colour, "white")) {
    plot2_caution("In 'sf' plots, " , font_blue("colour"), " will set the borders, not the areas - did you mean ", font_blue(paste0("colour_fill = ", deparse(colour))), "?")
  }
  
  df <- .data
  df$x <- ""
  df$y <- 0
  
  eval(plot2_exec_call(.data = quote(df),
                       x = quote(x),
                       y = quote(y),
                       type = "sf",
                       y_secondary = NULL,
                       y_secondary.type = NULL,
                       y_secondary.title = TRUE,
                       y_secondary.colour = NULL,
                       y_secondary.colour_fill = NULL,
                       y_secondary.scientific = NULL,
                       y_secondary.percent = NULL,
                       y_secondary.labels = NULL,
                       `_label.y_secondary` = NULL,
                       `_sf.column` = quote(attributes(.data)$sf_column),
                       `_datalabels.centroid` = quote(datalabels.centroid)))
}

#' @rdname plot2-methods
#' @importFrom dplyr is_grouped_df ungroup
#' @export
plot2.data.frame <- function(.data, ...) {
  # arguments: see plot2_method_formals() in R/zzz.R (F035)
  # per-call state and diagnostics, restored or emitted on exit (F001, F014)
  call_state_enter()
  on.exit(call_state_exit(), add = TRUE)
  
  if (isTRUE(is_grouped_df(.data))) {
    .data <- ungroup(.data)
  }
  
  # a formula after the data, e.g. mtcars |> plot2(mpg ~ wt), goes to the formula method (F081)
  if (rlang::quo_is_call(rlang::enquo(x), "~")) {
    args <- as.list(match.call())[-1]
    args$.data <- rlang::eval_tidy(rlang::enquo(x))
    args$x <- NULL
    args$data <- .data
    return(do.call(plot2.formula, args, envir = parent.frame()))
  }
  
  eval(plot2_exec_call()) 
}

#' @rdname plot2-methods
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename
#' @export
plot2.matrix <- function(.data, ...) {
  # arguments: see plot2_method_formals() in R/zzz.R (F035)
  # per-call state and diagnostics, restored or emitted on exit (F001, F014)
  call_state_enter()
  on.exit(call_state_exit(), add = TRUE)
  # x, y and category follow from the matrix (F034)
  ignored <- c(x = !missing(x), y = !missing(y), category = !missing(category))
  if (any(ignored)) {
    plot2_caution("In matrix plots, ", paste0(font_blue(names(ignored)[ignored], collapse = NULL), collapse = font_black(" and ")),
                  " will be ignored, since the rows and columns of the matrix determine x, y and category")
  }
  
  df <- .data |>
    as.data.frame(stringsAsFactors = FALSE)
  
  row_to_col <- function(df, var) {
    df[[var]] <- rownames(df)
    rownames(df) <- NULL
    df
  }
  
  if (is.null(type) && identical(rownames(df), colnames(df))) {
    # rows and columns are identical, while the data are numeric
    # this is the outcome of cor(), a correlation matrix
    # assume type tile
    plot2_message("Assuming ", font_blue("type = \"tile\""),
                  " since the matrix contains identical row and column names")
    type <- "tile"
    # make long format
    df <- df |> 
      row_to_col(var = "rowname") |> 
      pivot_longer(-rowname, names_to = "y", values_to = "category") |> 
      rename(x = rowname)
  } else {
    # a plain matrix: rows on x, every column as a category (F089)
    if (identical(rownames(df), as.character(seq_len(nrow(df))))) {
      df$x <- seq_len(nrow(df))
      rownames(df) <- NULL
    } else {
      df <- df |> row_to_col(var = "x")
    }
    df <- df |>
      pivot_longer(-x, names_to = "category", values_to = "y")
    df$category <- factor(df$category, levels = unique(df$category))
  }
  # axis titles from the names of the dimensions, if any
  dim_names <- names(dimnames(.data))
  label_of <- function(i, default) {
    if (length(dim_names) >= i && !is.na(dim_names[i]) && nzchar(dim_names[i])) dim_names[i] else default
  }
  if (missing(x.title) && !identical(label_of(1, ""), "")) {
    x.title <- label_of(1, "")
  }
  
  eval(plot2_exec_call(.data = quote(df),
                       x = quote(x),
                       y = quote(y),
                       category = quote(category),
                       facet = NULL,
                       `_label.x` = quote(label_of(1, "x")),
                       `_label.y` = "y",
                       `_label.category` = quote(label_of(2, "category")))) 
}

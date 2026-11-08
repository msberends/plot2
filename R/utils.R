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

plot2_env <- new.env(hash = FALSE)
# state that lives for the whole R session (never cleared by clear_plot2_env())
plot2_session <- new.env(hash = FALSE)
plot2_env$reg_cols <- list()

globalVariables(c(".",
                  "_new_title",
                  "_row_index",
                  "_sankey_id",
                  "_sankey_split",
                  "_sankey_x",
                  "_var_category",
                  "_var_datalabels",
                  "_var_facet",
                  "_var_x",
                  "_var_y",
                  "_var_y_secondary",
                  "ab",
                  "antibiotic", 
                  "cases",
                  "cluster",
                  "count",
                  "day_in_period",
                  "geom",
                  "group",
                  "in_scope",
                  "interpretation",
                  "isolates",
                  "ma_5c",
                  "ma_5c_pct_outscope",
                  "max_ma_5c",
                  "mo",
                  "month_day",
                  "moving_avg",
                  "moving_avg_limit",
                  "moving_avg_max",
                  "n",
                  "n_cases",
                  "name",
                  "out",
                  "period",
                  "period_date",
                  "period_txt",
                  "R",
                  "rowname",
                  "SI",
                  "syndromic_group",
                  "total",
                  "value",
                  "where",
                  "x",
                  "x_axis",
                  "xmax",
                  "xmin",
                  "y_max",
                  "y_max_label",
                  "y_min",
                  "y_min_label",
                  "year",
                  "ymax",
                  "ymin"))

like <- function(x, pattern) {
  x <- tolower(x)
  pattern <- tolower(pattern)
  if (length(pattern) == 1) {
    # only one pattern
    grepl(pattern, x, ignore.case = FALSE, fixed = FALSE,  perl = TRUE)
  } else {
    # multiple patterns
    if (length(x) == 1) {
      x <- rep(x, length(pattern))
    } else {
      stop("'x' and 'pattern' must be of equal length", call. = FALSE)
    }
    unlist(mapply(FUN = grepl, x = x, pattern = pattern, 
                  fixed = FALSE, perl = TRUE, MoreArgs = list(ignore.case = FALSE), 
                  SIMPLIFY = FALSE, USE.NAMES = FALSE))
  }
}
`%like%` <- function(x, pattern) {
  like(x = x, pattern = pattern)
}
`%unlike%` <- function(x, pattern) {
  !like(x = x, pattern = pattern)
}

font_black <- function(..., collapse = " ") {
  if (isTRUE(tryCatch(rstudioapi::getThemeInfo()$dark, error = function(e) FALSE))) {
    as.character(cli::col_white(paste0(c(...), collapse = collapse)))
  } else {
    as.character(cli::col_black(paste0(c(...), collapse = collapse)))
  }
}
font_blue <- function(..., collapse = " ") {
  as.character(cli::col_blue(paste0(c(...), collapse = collapse)))
}
font_bold <- function(..., collapse = " ") {
  as.character(cli::style_bold(paste0(c(...), collapse = collapse)))
}
font_italic <- function(..., collapse = " ") {
  as.character(cli::style_italic(paste0(c(...), collapse = collapse)))
}
font_magenta <- function(..., collapse = " ") {
  as.character(cli::col_magenta(paste0(c(...), collapse = collapse)))
}
font_stripstyle <- function(..., collapse = " ") {
  as.character(cli::ansi_strip(paste0(c(...), collapse = collapse)))
}
font_url <- function(url, txt = url) {
  if (tryCatch(cli::ansi_has_hyperlink_support(), error = function(e) FALSE)) {
    paste0("\033]8;;", url, "\a", txt, "\033]8;;\a")
  } else {
    url
  }
}
font_white <- function(..., collapse = " ") {
  if (isTRUE(tryCatch(rstudioapi::getThemeInfo()$dark, error = function(e) FALSE))) {
    as.character(cli::col_black(paste0(c(...), collapse = collapse)))
  } else {
    as.character(cli::col_white(paste0(c(...), collapse = collapse)))
  }
}

# Per-call state and diagnostics (F001, F014) ---------------------------------
#
# plot2_env holds the state of the plot2() call that is running. Every plot2()
# method calls call_state_enter() first: it saves and clears the state of an
# outer call (nested calls happen in back-to-back and UpSet plots, or when a
# method calls plot2() again), and call_state_exit() restores it. Only the
# registered colours ("reg_cols") are shared by all calls.
#
# plot2_message(), plot2_caution() and plot2_warning() collect diagnostics in
# order of occurrence. When the outermost call ends, they are emitted grouped:
# messages, then cautions, then warnings (maintainer's decision, 2026-11-08).
# Outside a call (e.g. at render time, or get_colour() used directly) they are
# emitted at once, so that they never surface with a later, unrelated call.

call_depth <- function() {
  length(plot2_session$call_stack)
}

call_state_enter <- function() {
  vars <- setdiff(ls(plot2_env, all.names = TRUE), "reg_cols")
  saved <- mget(vars, envir = plot2_env)
  rm(list = vars, envir = plot2_env)
  plot2_session$call_stack <- c(plot2_session$call_stack, list(saved))
  invisible(NULL)
}

call_state_exit <- function() {
  diagnostics <- list(infos = plot2_env$infos,
                      cautions = plot2_env$cautions,
                      warnings = plot2_env$warnings)
  rm(list = setdiff(ls(plot2_env, all.names = TRUE), "reg_cols"), envir = plot2_env)
  n <- call_depth()
  if (n == 0) {
    return(invisible(NULL))
  }
  saved <- plot2_session$call_stack[[n]]
  plot2_session$call_stack <- plot2_session$call_stack[-n]
  for (nm in names(saved)) {
    assign(nm, saved[[nm]], envir = plot2_env)
  }
  if (n > 1) {
    # a nested call: hand its diagnostics to the outer call
    for (type in names(diagnostics)) {
      plot2_env[[type]] <- c(plot2_env[[type]], diagnostics[[type]])
    }
  } else {
    throw_messages(diagnostics)
  }
  invisible(NULL)
}

plot2_message <- function(...) {
  plot2_diagnostic(paste0(c(...), collapse = ""), "infos")
}
plot2_caution <- function(...) {
  plot2_diagnostic(paste0(c(...), collapse = ""), "cautions")
}
plot2_warning <- function(...) {
  plot2_diagnostic(paste0(c(...), collapse = ""), "warnings")
}
plot2_diagnostic <- function(txt, type) {
  if (call_depth() > 0) {
    plot2_env[[type]] <- c(plot2_env[[type]], txt)
  } else {
    emit_diagnostic(txt, type)
  }
  invisible(NULL)
}

plot2_shows_messages <- function() {
  (interactive() || Sys.getenv("IN_PKGDOWN") != "") &&
    !isTRUE(as.logical(getOption("plot2.silent", default = FALSE)))
}

# Emit one diagnostic as an R condition. Messages and cautions are message
# conditions (classes "plot2_message", and "plot2_caution" for cautions):
# they can be caught or muffled (suppressMessages()), and are printed only in
# interactive sessions and pkgdown builds unless plot2.silent = TRUE. Warnings
# are R warnings (class "plot2_warning"), always visible. Returns whether a
# message was printed.
emit_diagnostic <- function(txt, type) {
  plain <- cli::ansi_strip(txt)
  if (type == "warnings") {
    warning(warningCondition(plain, class = "plot2_warning", call = NULL))
    return(invisible(FALSE))
  }
  cls <- c(if (type == "cautions") "plot2_caution", "plot2_message")
  printed <- FALSE
  withRestarts({
    signalCondition(messageCondition(paste0(plain, "\n"), class = cls, call = NULL))
    if (plot2_shows_messages()) {
      if (type == "cautions") {
        cli::cli_alert_warning(txt, wrap = TRUE)
      } else {
        cli::cli_alert_info(txt, wrap = TRUE)
      }
      printed <- TRUE
    }
  }, muffleMessage = function() NULL)
  invisible(printed)
}

throw_messages <- function(diagnostics) {
  printed <- FALSE
  for (type in c("infos", "cautions", "warnings")) {
    # in order of occurrence, each text once
    for (txt in unique(diagnostics[[type]])) {
      printed <- isTRUE(emit_diagnostic(txt, type)) || printed
    }
  }
  # show the tip about plot2.silent once per session, without touching the random-number stream
  if (isTRUE(printed) && !isTRUE(plot2_session$silent_tip_shown) && Sys.getenv("IN_PKGDOWN") == "") {
    if (is.null(getOption("plot2.silent"))) {
      message("NOTE: Use ", font_blue("options(plot2.silent = TRUE)"), " to silence plot2 messages.")
    }
    plot2_session$silent_tip_shown <- TRUE
  }
}

# whether an unevaluated direction (e.g. the y argument) is a column
# specification: a name, a string, c() of those, a `:` range of names, or a
# tidyselect helper; evaluated values (numeric vectors) are not
is_column_selection <- function(expr) {
  helpers <- c("where", "starts_with", "ends_with", "contains", "matches", "num_range",
               "everything", "last_col", "all_of", "any_of", "c", ":", "-", "!")
  if (is.symbol(expr)) return(TRUE)
  if (is.character(expr)) return(TRUE)
  if (!is.call(expr)) return(FALSE)
  fn <- expr[[1]]
  if (is.call(fn) && identical(fn[[1]], as.name("::"))) fn <- fn[[3]]
  if (!is.symbol(fn) || !as.character(fn) %in% helpers) return(FALSE)
  if (as.character(fn) %in% c("c", ":", "-", "!")) {
    return(all(vapply(as.list(expr)[-1], is_column_selection, logical(1))))
  }
  TRUE
}

# whether a materialised direction column is the switch TRUE or FALSE (e.g.
# `category = FALSE`, `datalabels = TRUE`) rather than data: only logical
# columns qualify, so numeric 0/1 data and NA values are never mistaken for it
is_switch <- function(x, value) {
  is.logical(x) && length(x) > 0 && !anyNA(x) && all(x == value)
}

# name of a transformation given as a string or as a transformation object
# (e.g. scales::transform_log10()), for comparisons such as == "identity"
transform_name <- function(x) {
  if (is.null(x)) return("identity")
  if (is.character(x)) return(x[1L])
  if (inherits(x, "transform") || is.list(x)) return(as.character(x$name %or% "custom")[1L])
  "custom"
}

# a sequence of breaks that are multiples of `by` (anchored at zero), also when
# `from` is negative and not a multiple of `by`
seq_anchored <- function(from, to, by) {
  out <- seq(floor(from / by + 1e-8) * by, to, by = by)
  round(out / by) * by
}

# breaks for whole numbers with a small range: pretty breaks without decimals,
# also below zero (used for the x, y and category scales)
breaks_whole_numbers <- function(x, ...) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(numeric(0))
  b <- scales::breaks_pretty()(x)
  b <- b[abs(b - round(b)) < 1e-8]
  if (length(b) < 2) {
    b <- seq(floor(min(x)), ceiling(max(x)), by = 1)
  }
  b
}

requires_numeric_coercion <- function(x) {
  !is.null(x) && mode(x) == "numeric" && !is.numeric(x) && !inherits(x, c("factor", "Date", "POSIXt"))
}

summarise_variable <- function(df, var, sep) {
  # combined with add_direction(), this will add support for multiple vars in one direction:
  # e.g., `category = c(col1, col2)`
  cols <- colnames(df)
  old_vars <- cols[cols %like% paste0(var, "_")]
  if (length(old_vars) == 0) {
    return(df)
  } else if (length(old_vars) > 1) {
    new_var <- do.call(paste, c(df[old_vars], sep = sep))
  } else {
    new_var <- df[, old_vars, drop = TRUE]
  }
  df <- df[, cols[!cols %in% old_vars], drop = FALSE]
  df[, var] <- new_var
  df
}

#' @importFrom dplyr select mutate across
add_direction <- function(df, direction, var_name, var_label, sep) {
  tryCatch({
    # this for using tidyverse selectors, such as `facet = where(is.character)`
    selected_cols <- df |>
      as.data.frame(stringsAsFactors = FALSE) |> # for sf data
      select({{ direction }}) |> 
      colnames()
    selected_cols <- selected_cols[selected_cols %unlike% "^_var_"]
    if (length(selected_cols) > 1 && is.character(selected_cols) && !all(var_label %like% selected_cols)) {
      # replace e.g. `facet = where(is.character)` with `facet = c(var1, var2, var3)`
      # in labels for columns, but also in mapping
      new_var_name <- paste0("c(", paste0(selected_cols, collapse = ", "), ")")
      plot2_message("Using ", font_blue(paste0(var_name, " = ", new_var_name)))
      if (var_name == "x") plot2_env$mapping_x <- new_var_name
      if (var_name == "category") plot2_env$mapping_category <- new_var_name
      if (var_name == "facet") plot2_env$mapping_facet <- new_var_name
      var_label <- new_var_name
    }
  }, error = function(e) invisible())
  
  df <- tryCatch({
    out <- df |> 
      mutate(`_var_` = {{ direction }})
    colnames(out)[colnames(out) == "_var_"] <- paste0("_var_", var_name)
    out
  }, error = function(e) {
    # multiple columns selected
    df |> 
      mutate(across({{ direction }}, .names = paste0("_var_", var_name, "_{col}"))) |> 
      summarise_variable(paste0("_var_", var_name), sep = sep)
  })
  
  # this adds the column again with the right label
  var_label <- paste0(trimws(var_label), collapse = " ")
  var_label <- gsub("^\\{ \\{", "", var_label)
  var_label <- gsub("\\} \\}$", "", var_label)
  var_label <- trimws(var_label)
  if (var_label != "NULL" && !var_label %in% colnames(df) && paste0("_var_", var_name) %in% colnames(df)) {
    df$`_var_new` <- df[, paste0("_var_", var_name), drop = TRUE]
    colnames(df)[colnames(df) == "_var_new"] <- var_label
  }
  
  df
}

#' @importFrom dplyr pull
get_column_name <- function(df, column_var) {
  out <- vapply(FUN.VALUE = logical(1), df, function(col) {
    identical(col,
              df |> pull({{column_var}}))
  })
  if (all(out[names(out) %unlike% "^_var_"] == FALSE)) {
    # no column found, probably due to sorting (i.e., factors), try again with character comparison
    out <- vapply(FUN.VALUE = logical(1), df, function(col) {
      identical(col |> as.character(),
                df |> pull({{column_var}}) |> as.character())
    })
  }
  out <- names(out)[out & names(out) %unlike% "^_var_"][1L]
  if (is.na(out)) {
    return(NULL)
  }
  out
}

get_x <- function(df, na.rm = FALSE) {
  if (has_x(df)) {
    out <- df$`_var_x`
    if (isTRUE(na.rm)) {
      out <- out[!is.na(out)]
    }
    out
  } else {
    NULL
  }
}
get_x_name <- function(df) {
  if (has_x(df)) {
    if (!is.null(plot2_env$mapping_x) && plot2_env$mapping_x != "NULL" && plot2_env$mapping_x %in% colnames(df)) {
      plot2_env$mapping_x
    } else {
      get_column_name(df, `_var_x`)
    }
  } else {
    NULL
  }
}
has_x <- function(df) {
  "_var_x" %in% colnames(df)
}

get_y <- function(df) {
  if (has_y(df)) {
    df$`_var_y`
  } else {
    NULL
  }
}
get_y_name <- function(df) {
  if (has_y(df)) {
    if (!is.null(plot2_env$mapping_y) && plot2_env$mapping_y != "NULL" && plot2_env$mapping_y %in% colnames(df)) {
      plot2_env$mapping_y
    } else {
      get_column_name(df, `_var_y`)
    }
  } else {
    NULL
  }
}
has_y <- function(df) {
  "_var_y" %in% colnames(df)
}

get_category <- function(df, na.rm = FALSE) {
  if (has_category(df)) {
    out <- df$`_var_category`
    if (isTRUE(na.rm)) {
      out <- out[!is.na(out)]
    }
    out
  } else {
    NULL
  }
}
get_category_name <- function(df) {
  if (has_category(df)) {
    if (!is.null(plot2_env$mapping_category) && plot2_env$mapping_category != "NULL" && plot2_env$mapping_category %in% colnames(df)) {
      plot2_env$mapping_category
    } else {
      get_column_name(df, `_var_category`)
    }
  } else {
    NULL
  }
}
has_category <- function(df) {
  "_var_category" %in% colnames(df)
}

get_facet <- function(df) {
  if (has_facet(df)) {
    df$`_var_facet`
  } else {
    NULL
  }
}
get_facet_name <- function(df) {
  if (has_facet(df)) {
    if (!is.null(plot2_env$mapping_facet) && plot2_env$mapping_facet != "NULL" && plot2_env$mapping_facet %in% colnames(df)) {
      plot2_env$mapping_facet
    } else {
      get_column_name(df, `_var_facet`)
    }
  } else {
    NULL
  }
}
has_facet <- function(df) {
  "_var_facet" %in% colnames(df)
}

get_y_secondary <- function(df) {
  if (has_y_secondary(df)) {
    df$`_var_y_secondary`
  } else {
    NULL
  }
}
get_y_secondary_name <- function(df) {
  if (has_y_secondary(df)) {
    if (!is.null(plot2_env$mapping_y_secondary) && plot2_env$mapping_y_secondary != "NULL" && plot2_env$mapping_y_secondary %in% colnames(df)) {
      plot2_env$mapping_y_secondary
    } else {
      get_column_name(df, `_var_y_secondary`)
    }
  } else {
    NULL
  }
}
has_y_secondary <- function(df) {
  "_var_y_secondary" %in% colnames(df)
}

get_datalabels <- function(df) {
  if (has_datalabels(df)) {
    df$`_var_datalabels`
  } else {
    NULL
  }
}
has_datalabels <- function(df) {
  "_var_datalabels" %in% colnames(df)
}

determine_date_breaks_labels <- function(x) {
  rng <- range(x, na.rm = TRUE)
  range_in_days <- as.double(difftime(rng[2], rng[1], units = "days"))
  range_in_months <- diff(as.double(format(rng, "%m"))) + 1
  range_in_years <- diff(as.double(format(rng, "%Y"))) + 1
  if (range_in_days <= 2) {
    range_in_hours <- as.double(difftime(rng[2], rng[1], units = "hours"))
    range_in_minutes <- as.double(difftime(rng[2], rng[1], units = "mins"))
    if (range_in_hours <= 1) {
      if (range_in_minutes <= 10) {
        out <- list(breaks = "1 min",
                    labels = "HH:MM")
      } else if (range_in_minutes <= 30) {
        out <- list(breaks = "5 min",
                    labels = "HH:MM")
      } else {
        out <- list(breaks = "10 min",
                    labels = "HH:MM")
      }
    } else if (range_in_hours <= 2) {
      if (range_in_minutes <= 30) {
        out <- list(breaks = "10 min",
                    labels = "HH:MM")
      } else {
        out <- list(breaks = "15 mins",
                    labels = "HH:MM")
      }
    } else if (range_in_hours <= 4) {
      out <- list(breaks = "30 mins",
                  labels = "HH:MM")
    } else if (range_in_hours <= 6) {
      out <- list(breaks = "1 hour",
                  labels = "HH:MM")
    } else if (range_in_hours <= 12) {
      out <- list(breaks = "1 hour",
                  labels = "HH")
    } else {
      out <- list(breaks = "2 hours",
                  labels = "HH")
    }
  } else if (range_in_days <= 7) {
    # 1 week
    out <- list(breaks = "1 day",
                labels = "ddd")
  } else if (range_in_days <= 31) {
    # 1 month
    out <- list(breaks = "1 day",
                labels = "d mmm")
  } else if (range_in_days < 100 && range_in_months <= 3) {
    # quarter
    out <- list(breaks = "4 days",
                labels = "d mmm")
  } else if (range_in_days < 190 && range_in_months <= 6) {
    # half year
    out <- list(breaks = "2 weeks",
                labels = "d mmm")
  } else if (range_in_days <= 366 && range_in_years == 1) {
    # year within 1 year
    out <- list(breaks = "1 month",
                labels = "mmm")
  } else if (range_in_days <= 366 && range_in_years == 2) {
    # max 1 year, but crossing 1 Jan
    out <- list(breaks = "2 months",
                labels = "mmm yyyy")
  } else if (range_in_years == 2) {
    out <- list(breaks = "3 months",
                labels = "mmm yyyy")
  } else if (range_in_years == 3) {
    out <- list(breaks = "6 months",
                labels = "mmm yyyy")
  } else if (range_in_years <= 5) {
    out <- list(breaks = "1 year",
                labels = "mmm yyyy")
  } else if (range_in_years < 10) {
    out <- list(breaks = "1 year",
                labels = "yyyy")
  } else if (range_in_years < 25) {
    out <- list(breaks = "2 years",
                labels = "yyyy")
  } else {
    # even longer, all other cases
    out <- list(breaks = "5 years",
                labels = "yyyy")
  }
  out
}

unify_years <- function(x, as_leap_year = NULL) {
  if (is.null(as_leap_year)) {
    as_leap_year <- any(x |> format() |> substr(6, 10) == "02-29", na.rm = TRUE)
  }
  if (inherits(x, "Date")) {
    as.Date(paste0(ifelse(as_leap_year, "1972", "1970"), x |> format() |> substr(5, 10)))
  } else {
    # keep the time zone (F029)
    tz <- attr(as.POSIXct(x), "tzone")
    tz <- if (is.null(tz)) "" else tz[1]
    as.POSIXct(paste0(ifelse(as_leap_year, "1972", "1970"), format(x, tz = tz) |> substr(5, 99)), tz = tz)
  }
}

is_empty <- function(x) {
  is.null(x) || isFALSE(x) || identical(x, "") || tryCatch(all(is.na(as.character(x))), error = function(x) FALSE)
}

geom_is_continuous <- function(geom) {
  geom %in% c("geom_boxplot", "geom_violin", "geom_point", "geom_jitter", "geom_beeswarm", "geom_histogram", "geom_density", "geom_sf", "geom_line", "geom_area", "geom_ribbon", "geom_polygon", "geom_tile", "geom_raster", "geom_rect", "geom_upset", "geom_spider")
}
geom_is_continuous_x <- function(geom) {
  geom %in% c("geom_histogram", "geom_density")
}
geom_is_line_or_area <- function(geom) {
  geom %in% c("geom_line", "geom_hline", "geom_vline", "geom_path", "geom_qq_line", "geom_linerange", "geom_area", "geom_ribbon", "geom_tile", "geom_raster", "geom_rect", "geom_polygon")
}
geom_has_only_colour <- function(geom) {
  geom %in% c("geom_point", "geom_jitter", "geom_line", "geom_hline", "geom_vline",
              "geom_path", "geom_qq_line", "geom_linerange", "geom_pointrange")
}

#' @importFrom dplyr group_by across group_size
group_sizes <- function(df) {
  if (inherits(df, "sf")) {
    nrow(df)
  } else {
    df |> 
      group_by(across(c(get_x_name(df), get_category_name(df), get_facet_name(df))),
               .drop = FALSE) |>
      group_size()
  }
}

# this replaces ggplot2::aes_string(), which was deprecated in 3.4.0
#' @importFrom ggplot2 aes
setup_aes <- function(current = aes(), ..., as_symbol = FALSE) {
  mapping <- list(...)
  caller_env <- parent.frame()
  mapping <- lapply(mapping, function(x) {
    if (tryCatch(is.null(x) || identical(x, "") || identical(x, "NULL"), error = function(e) FALSE)) {
      # this will ultimately remove the aesthetic from the list, after running utils::modifyList()
      return(NULL)
    }
    if (rlang::is_quosure(x)) {
      # as regular text
      x <- rlang::as_label(x)
    }
    if (isTRUE(as_symbol)) {
      # this is required for restore_mapping()
      x <- as.symbol(x)
    } else {
      # use str2lang() to get a `call` type, but not if it's already in backticks or if it's a number:
      x <- as.character(x)
      if (x != make.names(x) && substr(x, 1, 1) != "`" && x %unlike% "^[0-9.]+$") {
        x <- paste0("`", x, "`")
      }
      x <- str2lang(x)
    }
    rlang::new_quosure(x, env = caller_env)
  })
  out <- structure(mapping, class = class(aes()))
  utils::modifyList(current, out)
}

restore_mapping <- function(p, df) {
  # helper function
  fn_new_mapping <- function(mapping, df) {
    if (is.null(mapping)) {
      return(mapping)
    }
    att <- attributes(mapping)
    new_mapping <- lapply(mapping,
                          function(map) {
                            # deparse(map) has a value such as "~`_var_y`"
                            if (any(deparse(map) %like% "_var_x")) {
                              setup_aes(x = get_x_name(df), as_symbol = TRUE)[[1]] 
                            } else if (any(deparse(map) %like% "_var_y_secondary")) {
                              setup_aes(x = get_y_secondary_name(df), as_symbol = TRUE)[[1]]
                            } else if (any(deparse(map) %like% "_var_y")) {
                              setup_aes(x = get_y_name(df), as_symbol = TRUE)[[1]]
                            } else if (any(deparse(map) %like% "_var_category")) {
                              setup_aes(x = get_category_name(df), as_symbol = TRUE)[[1]]
                            } else if (any(deparse(map) %like% "_var_facet")) {
                              setup_aes(x = get_facet_name(df), as_symbol = TRUE)[[1]]
                            } else {
                              map
                            }})
    attributes(new_mapping) <- att
    new_mapping
  }
  
  # general plot mapping
  p$mapping <- fn_new_mapping(mapping = p$mapping, df = df)
  # mapping for each extra layer, such as geom_smooth()
  for (i in seq_len(length(p$layers))) {
    p$layers[[i]]$mapping <- fn_new_mapping(mapping = p$layers[[i]]$mapping, df = df)
  }
  
  # facet mapping
  if (has_facet(df)) {
    p$facet$params$facets[[1]] <- setup_aes(x = get_facet_name(df), as_symbol = TRUE)[[1]]
    names(p$facet$params$facets)[1] <- get_facet_name(df)
    # required for ggplot2::facet_grid(), which is used when facet.relative = TRUE:
    if (length(p$facet$params$rows) > 0) {
      p$facet$params$rows[[1]] <- setup_aes(x = get_facet_name(df), as_symbol = TRUE)[[1]]
      names(p$facet$params$rows)[1] <- get_facet_name(df)
    }
    if (length(p$facet$params$cols) > 0) {
      p$facet$params$cols[[1]] <- setup_aes(x = get_facet_name(df), as_symbol = TRUE)[[1]]
      names(p$facet$params$cols)[1] <- get_facet_name(df)
    }
  }
  
  # now remove these anonymous`_var_*` columns from the data
  p$data <- p$data[, colnames(p$data)[colnames(p$data) %unlike% "^_var_(x|y|category|facet)$"], drop = FALSE]

  # give the remaining internal columns (such as `_var_datalabels` and `_sankey_x`) readable names,
  # or remove them if no layer uses them (F069)
  internal <- colnames(p$data)[startsWith(colnames(p$data), "_")]
  if (length(internal) > 0) {
    # mapping entries are quosures or, after the renaming above, plain symbols
    map_expr <- function(m) if (rlang::is_quosure(m)) rlang::quo_get_expr(m) else m
    used <- unique(unlist(c(lapply(p$mapping, function(m) all.vars(map_expr(m))),
                            lapply(p$layers, function(l) {
                              if (inherits(l$data, "waiver")) {
                                unlist(lapply(l$mapping, function(m) all.vars(map_expr(m))))
                              }
                            }))))
    internal_used <- internal[internal %in% used]
    p$data <- p$data[, colnames(p$data)[!colnames(p$data) %in% setdiff(internal, internal_used)], drop = FALSE]
    if (length(internal_used) > 0) {
      others <- setdiff(colnames(p$data), internal_used)
      readable <- make.unique(c(others, sub("^_(var_)?", "", internal_used)))[-seq_along(others)]
      colnames(p$data)[match(internal_used, colnames(p$data))] <- readable
      rename_symbols <- function(expr) {
        if (is.symbol(expr) && as.character(expr) %in% internal_used) {
          as.symbol(readable[match(as.character(expr), internal_used)])
        } else if (is.call(expr)) {
          as.call(lapply(as.list(expr), rename_symbols))
        } else {
          expr
        }
      }
      rename_mapping <- function(mapping) {
        if (is.null(mapping)) {
          return(mapping)
        }
        att <- attributes(mapping)
        out <- lapply(mapping, function(m) {
          if (rlang::is_quosure(m)) rlang::quo_set_expr(m, rename_symbols(rlang::quo_get_expr(m))) else rename_symbols(m)
        })
        attributes(out) <- att
        out
      }
      p$mapping <- rename_mapping(p$mapping)
      for (i in seq_along(p$layers)) {
        if (inherits(p$layers[[i]]$data, "waiver")) {
          p$layers[[i]]$mapping <- rename_mapping(p$layers[[i]]$mapping)
        }
      }
    }
  }
  
  # return the plot object
  p
}

set_plot2_env <- function(x = NULL, y = NULL, category = NULL, facet = NULL, y_secondary = NULL, x_variable_names = NULL) {
  x <- paste0(trimws(x), collapse = " ")
  y <- paste0(trimws(y), collapse = " ")
  category <- paste0(category, collapse = " ")
  facet <- paste0(facet, collapse = " ")
  y_secondary <- paste0(y_secondary, collapse = " ")
  if (!x %in% c("NULL", "") && is.null(plot2_env$mapping_x)) {
    plot2_env$mapping_x <- x
  }
  if (!y %in% c("NULL", "") && is.null(plot2_env$mapping_y)) {
    plot2_env$mapping_y <- y
  }
  if (!category %in% c("NULL", "") && is.null(plot2_env$mapping_category)) {
    plot2_env$mapping_category <- category
  }
  if (!facet %in% c("NULL", "") && is.null(plot2_env$mapping_facet)) {
    plot2_env$mapping_facet <- facet
  }
  if (!y_secondary %in% c("NULL", "") && is.null(plot2_env$mapping_y_secondary)) {
    plot2_env$mapping_y_secondary <- y_secondary
  }
  plot2_env$x_variable_names <- x_variable_names
}
clear_plot2_env <- function() {
  # clear everything from local environment, except for registered colours ("reg_cols"),
  # and forget any unfinished calls; used by the tests to recover from failures
  vars_in_env <- ls(envir = plot2_env, all.names = TRUE)
  vars_to_remove <- setdiff(vars_in_env, "reg_cols")
  rm(list = vars_to_remove, envir = plot2_env)
  plot2_session$call_stack <- list()
}

sigfigs <- function(x) {
  vapply(FUN.VALUE = double(1), x, function(val) {
    frm <- format(val, scientific = FALSE)
    if (frm %unlike% "[.]" | frm %like% "[.]0+$") {
      0
    } else if (frm %like% "[.]0") {
      nchar(gsub(".*[.](0+).*$", "\\1", frm)) + 1
    } else {
      nchar(gsub(".*[.]([0-9]+)$", "\\1", frm))
    }
  })
}

is_date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

data_is_numeric <- function(x) {
  all(x %like% "^[0-9.,-]+(e[+][0-9.,-]+)?$", na.rm = TRUE)
}

digit_to_text <- function(x) {
  out <- switch(x,
                "one",
                "two",
                "three",
                "four",
                "five",
                "six",
                "seven",
                "eight",
                "nine",
                "ten")
  if (is.null(out)) {
    out <- as.character(x)
  }
  out
}

format_error <- function(e, replace = character(0), by = character(0)) {
  txt <- conditionMessage(e)
  if (length(txt) == 0) {
    # return original error
    stop(e, call. = FALSE)
  }
  for (i in seq_len(length(replace))) {
    txt <- gsub(replace[i], by[i], txt)
  }
  if (all(txt == "")) {
    txt <- "Plot cannot be generated due to unknown error"
  }
  txt <- trimws(txt)
  paste0(txt, collapse = "\n")
}

`%or%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @importFrom ggplot2 fortify
#' @importFrom dplyr select mutate
fortify_df <- function(df, y = NULL) {
  # broom is in Suggests (F108): use broom::augment() for model objects when it is installed
  augment_classes <- function() {
    m <- suppressMessages(utils::methods(broom::augment))
    gsub("augment.", "", as.character(m), fixed = TRUE)
  }
  has_broom <- requireNamespace("broom", quietly = TRUE)
  
  if (is.null(tryCatch(rlang::eval_tidy(y), error = function(e) FALSE))) {
    y <- NULL
  }
  
  if (has_broom && inherits(df, augment_classes())) {
    # broom::augment() supports our data
    new_df <- tryCatch(broom::augment(df), error = function(e) NULL)
  } else {
    # try the ggplot2::fortify()
    new_df <- tryCatch(fortify(df), error = function(e) NULL)
    if (!is.data.frame(new_df) && !has_broom) {
      # e.g. a model object, which broom can turn into a data frame
      rlang::check_installed("broom", reason = paste0("to plot an object of class '", class(df)[1], "'"))
      new_df <- tryCatch(broom::augment(df), error = function(e) NULL)
    }
  }
  
  if (is.data.frame(new_df)) {
    # broom returns a tibble, so rownames became a column - return to original data
    new_df <- as.data.frame(new_df, stringsAsFactors = FALSE)
    if (".rownames" %in% colnames(new_df)) {
      rownames(new_df) <- new_df[[".rownames"]]
      new_df <- new_df |> select(-".rownames")
    }
    
  } else if (is.null(new_df)) {
    # try as.data.frame methods to make a regular data.frame (e.g., as.data.frame.table(), etc.)
    new_df <- tryCatch(as.data.frame(df, stringsAsFactors = FALSE),
                       error = function(e) NULL)
    if (!is.data.frame(new_df)) {
      stop("Unable to initialise plot2(): input class '", paste(class(df), collapse = "/"), "' is unsupported",
           call. = FALSE)
    }
    if (!inherits(df, "data.frame")) {
      plot2_caution("Input class ", paste0("'", class(df), "'", collapse = "/"), " was transformed using `as.data.frame()`")
    }
    if (inherits(df, "table")) {
      # if using `as.data.frame()` on a `table`, the resulting count column with be "Freq"
      if (tryCatch(is.null(y), error = function(e) FALSE)) {
        colnames(new_df)[colnames(new_df) == "Freq"] <- "count"
        y <- str2lang("count")
      } else {
        # this is when y is set to something - put that to the Y-axis and adjust the data set
        `:=` <- rlang::`:=`
        col_fn <- function(y = NULL) data.frame() |> mutate({{ y }} := 0)
        y <- colnames(col_fn({{ y }}))
        colnames(new_df)[colnames(new_df) == "Freq"] <- y
        y <- str2lang(y)
      }
    }
  }
  
  list(new_df = new_df, y = y)
}

natural_sort <- function(x, decreasing = FALSE) {
  # like stringr::str_sort(numeric = TRUE) (ICU collation): numbers within strings are
  # compared as numbers and come before text; text ignores case, with lower case first
  x <- as.character(x)
  nas <- x[is.na(x)]
  x <- x[!is.na(x)]
  if (length(x) < 2) return(c(x, nas))
  parts <- regmatches(x, gregexpr("[0-9]+|[^0-9]+", x))
  n <- max(lengths(parts))
  keys <- list()
  for (i in seq_len(n)) {
    tok <- vapply(parts, function(p) if (length(p) >= i) p[i] else NA_character_, character(1))
    is_num <- !is.na(tok) & grepl("^[0-9]+$", tok)
    # a string that has ended sorts first, then numbers, then text
    keys[[length(keys) + 1]] <- ifelse(is.na(tok), 0L, ifelse(is_num, 1L, 2L))
    keys[[length(keys) + 1]] <- ifelse(is_num, suppressWarnings(as.double(tok)), 0)
    keys[[length(keys) + 1]] <- xtfrm(ifelse(is_num | is.na(tok), "", tolower(tok)))
  }
  # ties (only case differs): lower case first
  # (a key of 0 for lower case and 1 for upper case per character, so independent of the locale)
  keys[[length(keys) + 1]] <- xtfrm(gsub("[^1]", "0", gsub("[[:upper:]]", "1", x)))
  ord <- do.call(order, keys)
  if (isTRUE(decreasing)) ord <- rev(ord)
  c(x[ord], nas)
}

# The language of the words that plot2 generates (titles, y.24h, y.age) and of
# date labels: option plot2.language, "en" (default) or "nl" (F003)
plot2_language <- function() {
  lang <- getOption("plot2.language", default = "en")
  lang <- tolower(substr(trimws(as.character(lang)[1L]), 1, 2))
  if (!isTRUE(lang %in% c("en", "nl"))) {
    plot2_warning("Option plot2.language must be \"en\" or \"nl\", using \"en\"")
    lang <- "en"
  }
  lang
}

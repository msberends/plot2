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
                  "y_min",
                  "ymax",
                  "ymin",
                  "year"))

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

#' @importFrom crayon black white
font_black <- function(..., collapse = " ") {
  if (isTRUE(tryCatch(rstudioapi::getThemeInfo()$dark, error = function(e) FALSE))) {
    paste0(white(...), collapse = collapse)
  } else {
    paste0(black(...), collapse = collapse)
  }
}
#' @importFrom crayon blue
font_blue <- function(..., collapse = " ") {
  paste0(blue(...), collapse = collapse)
}
#' @importFrom crayon bold
font_bold <- function(..., collapse = " ") {
  paste0(bold(...), collapse = collapse)
}
#' @importFrom crayon magenta
font_magenta <- function(..., collapse = " ") {
  paste0(magenta(...), collapse = collapse)
}
#' @importFrom crayon strip_style
font_stripstyle <- function(..., collapse = " ") {
  paste0(strip_style(...), collapse = collapse)
}
#' @importFrom crayon black white
font_white <- function(..., collapse = " ") {
  if (isTRUE(tryCatch(rstudioapi::getThemeInfo()$dark, error = function(e) FALSE))) {
    paste0(black(...), collapse = collapse)
  } else {
    paste0(white(...), collapse = collapse)
  }
}

plot2_message <- function(..., print = interactive() | Sys.getenv("IN_PKGDOWN") != "", type = "info") {
  # at default, only prints in interactive mode and for the website generation
  if (isTRUE(print)) {
    # get info icon
    if (isTRUE(base::l10n_info()$`UTF-8`) && interactive()) {
      # \u2139 is a symbol officially named 'information source'
      icon <- "\u2139"
    } else {
      icon <- "i"
    }
    if (type == "info") {
      fn <- font_black
      icon <- font_blue(icon)
    } else {
      fn <- font_magenta
      icon <- font_magenta("!")
    }
    msg <- paste0(fn(c(...), collapse = NULL), collapse = "")
    if (type %in% c("info", "caution")) {
      message(paste(icon, fn(msg)))
    } else if (type == "warning") {
      warning("\n", paste(icon, fn(msg)), call. = FALSE, immediate. = TRUE)
    }
  }
}

plot2_caution <- function(..., print = interactive() | Sys.getenv("IN_PKGDOWN") != "") {
  plot2_message(..., print = print, type = "caution")
}

plot2_warning <- function(..., print = interactive() | Sys.getenv("IN_PKGDOWN") != "") {
  plot2_message(..., print = print, type = "warning")
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
  if (var_label != "NULL" && !var_label %in% colnames(df)) {
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

#' @importFrom dplyr n_distinct
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
    as.POSIXct(paste0(ifelse(as_leap_year, "1972", "1970"), x |> format() |> substr(5, 99)))
  }
}

is_empty <- function(x) {
  is.null(x) || isFALSE(x) || identical(x, "") || all(is.na(as.character(x)))
}

geom_is_continuous <- function(geom) {
  geom %in% c("geom_boxplot", "geom_violin", "geom_point", "geom_jitter", "geom_histogram", "geom_density", "geom_sf", "geom_line", "geom_area", "geom_ribbon", "geom_tile", "geom_raster", "geom_rect")
}
geom_is_continuous_x <- function(geom) {
  geom %in% c("geom_histogram", "geom_density")
}
geom_is_line_or_area <- function(geom) {
  geom %in% c("geom_line", "geom_hline", "geom_vline", "geom_path", "geom_qq_line", "geom_linerange", "geom_area", "geom_ribbon", "geom_tile", "geom_raster", "geom_rect")
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
#' @importFrom rlang is_quosure as_label new_quosure
update_aes <- function(current = aes(), ..., as_symbol = FALSE) {
  mapping <- list(...)
  caller_env <- parent.frame()
  mapping <- lapply(mapping, function(x) {
    if (tryCatch(is.null(x) || identical(x, "") || identical(x, "NULL"), error = function(e) FALSE)) {
      # this will ultimately remove the aesthetic from the list, after running utils::modifyList()
      return(NULL)
    }
    if (is_quosure(x)) {
      # as regular text
      x <- as_label(x)
    }
    if (isTRUE(as_symbol)) {
      # this is required for restore_mapping()
      x <- as.symbol(x)
    } else {
      # use str2lang() to get a `call` type:
      x <- as.character(x)
      if (x %unlike% "^[A-Za-z0-9]") {
        x <- paste0("`", x, "`")
      }
      x <- str2lang(x)
    }
    new_quosure(x, env = caller_env)
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
                              update_aes(x = get_x_name(df), as_symbol = TRUE)[[1]] 
                            } else if (any(deparse(map) %like% "_var_y_secondary")) {
                              update_aes(x = get_y_secondary_name(df), as_symbol = TRUE)[[1]]
                            } else if (any(deparse(map) %like% "_var_y")) {
                              update_aes(x = get_y_name(df), as_symbol = TRUE)[[1]]
                            } else if (any(deparse(map) %like% "_var_category")) {
                              update_aes(x = get_category_name(df), as_symbol = TRUE)[[1]]
                            } else if (any(deparse(map) %like% "_var_facet")) {
                              update_aes(x = get_facet_name(df), as_symbol = TRUE)[[1]]
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
    p$facet$params$facets[[1]] <- update_aes(x = get_facet_name(df), as_symbol = TRUE)[[1]]
    names(p$facet$params$facets)[1] <- get_facet_name(df)
    # required for ggplot2::facet_grid(), which is used when facet.relative = TRUE:
    if (length(p$facet$params$rows) > 0) {
      p$facet$params$rows[[1]] <- update_aes(x = get_facet_name(df), as_symbol = TRUE)[[1]]
      names(p$facet$params$rows)[1] <- get_facet_name(df)
    }
    if (length(p$facet$params$cols) > 0) {
      p$facet$params$cols[[1]] <- update_aes(x = get_facet_name(df), as_symbol = TRUE)[[1]]
      names(p$facet$params$cols)[1] <- get_facet_name(df)
    }
  }
  
  # now remove these anonymous`_var_*` columns from the data
  p$data <- p$data[, colnames(p$data)[colnames(p$data) %unlike% "^_var_(x|y|category|facet)$"], drop = FALSE]
  
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
clean_plot2_env <- function() {
  plot2_env$mapping_x <- NULL
  plot2_env$mapping_y <- NULL
  plot2_env$mapping_category <- NULL
  plot2_env$mapping_facet <- NULL
  plot2_env$mapping_y_secondary <- NULL
  plot2_env$x_variable_names <- NULL
  plot2_env$y_secondary_factor <- NULL
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

#' @importFrom rlang cnd_message
format_error <- function(e, replace = character(0), by = character(0)) {
  if (inherits(e, "rlang_error")) {
    txt <- cnd_message(e)
    txt <- font_stripstyle(txt)
    txt <- gsub(".*Caused by error[:](\n!)?", "", txt)
  } else {
    txt <- c(e$message, e$parent$message, e$parent$parent$message, e$parent$parent$parent$message, e$call)
  }
  txt <- txt[txt %unlike% "^Problem while"]
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

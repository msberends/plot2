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

#' @importFrom dplyr group_by across summarise n_distinct n
validate_type <- function(type, df = NULL) {
  type.bak <- type
  type_unset <- (is.null(type) || identical(type, ""))
  if (type_unset && !is.null(df)) {
    if (!has_x(df)) {
      # only numeric values, make it a boxplot
      type <- "geom_boxplot"
      plot2_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL),
                    font_black(" since there is no x axis"))
    } else if (has_x(df) && is.numeric(get_x(df))) {
      if (identical(get_x(df), get_y(df))) {
        # both x and y are numeric - if they are equal then use histogram
        # see plot_message below 'since the variable ... is the only numeric variable'
        type <- "geom_histogram"
      } else {
        # make it points if x and y are both numeric
        type <- "geom_point"
        plot2_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL), 
                      font_black(" since both axes are numeric"))
      }
    } else {
      # check if y has multiple values across groups, then make it boxplot
      if (all(group_sizes(df) >= 3)) {
        type <- "geom_boxplot"
        plot2_message("Using ", font_blue("type = \"", gsub("geom_", "", type), "\"", collapse = NULL),
                      " since all groups in ",
                      paste0(font_blue(unique(c(get_x_name(df), get_category_name(df), get_facet_name(df))),
                                       collapse = NULL),
                             collapse = font_black(" and ")), 
                      " contain at least three values")
      } else {
        # otherwise: the default
        type <- getOption("plot2.default_type", "geom_col")
        if (!is.null(df) && has_x(df) && has_category(df) && n_distinct(get_category(df)) == 2) {
          count_nrs <- df |>
            group_by(across(c(get_x_name(df), get_category_name(df)))) |>
            summarise(n = n())
          if (length(plot2_env$x_variable_names) > 1) {
            plot2_message("To view the relation between multiple variables of ", font_blue("x"), ", a Sankey plot can be used (",
                          font_blue("type = \"sankey\""), " or ", font_blue("type = \"s\""), ")")
          } else if (all(count_nrs$n, na.rm = TRUE) == 1) {
            plot2_message("To compare single values in two categories (", font_blue(get_category_name(df)), "), a dumbbell plot can be used (",
                          font_blue("type = \"dumbbell\""), " or ", font_blue("type = \"d\""), ")")
          }
        }
      }
    }
  } else if (type_unset && is.null(df)) {
    return("") # for quick validation
  } else {
    if (length(type) > 1) {
      plot2_warning(font_blue("type"), " can only be of length 1")
    }
    type <- trimws(tolower(type[1L]))
    type <- gsub(".*::", "", type) # for `type = "ggplot2::geom_col()"`
    type <- gsub("[^a-z0-9_]", "", type)
    if (type == "a") type <- "area"
    if (type == "b") type <- "boxplot"
    if (type == "c") type <- "column"
    if (type == "h") type <- "histogram"
    if (type == "j") type <- "jitter"
    if (type == "l") type <- "line"
    if (type == "p") type <- "point"
    if (type == "r") type <- "ribbon"
    if (type == "v") type <- "violin"
    if (type == "bs") type <- "beeswarm"
    if (type %like% "column") { # don't catch "bar" here - we consider that a horizontal "col" like Excel
      type <- "col"
    }
    if (type %unlike% "^geom_") {
      type <- paste0("geom_", type)
    }
    # replace 'points' etc. with 'point' etc.
    type <- gsub("s$", "", type)
  }
  
  valid_geoms <- ls(pattern = "^geom_", envir = asNamespace("ggplot2"))
  if ("ggbeeswarm" %in% rownames(utils::installed.packages())) {
    valid_geoms <- c(valid_geoms, 
                     ls(pattern = "^geom_", envir = asNamespace("ggbeeswarm")))
  }
  if (!type %in% valid_geoms) {
    if (any(valid_geoms %like% type)) {
      type <- valid_geoms[valid_geoms %like% type][1L]
    } else {
      stop("plot type \"", type.bak, "\" is invalid since ggplot2::", type, "() does not exist", call. = FALSE)
    }
  }
  type
}

validate_legend.position <- function(legend.position) {
  if (is.null(legend.position) || is.na(legend.position[1L])) {
    legend.position <- "none"
  }
  legend.position <- trimws(tolower(legend.position[1L]))
  legend.position <- gsub("^t$", "top", legend.position)
  legend.position <- gsub("^r$", "right", legend.position)
  legend.position <- gsub("^b$", "bottom", legend.position)
  legend.position <- gsub("^l$", "left", legend.position)
  
  if (!legend.position %in% c("top", "right", "bottom", "left", "none")) {
    stop("`legend.position` must be 'top', 'right', 'bottom', 'left' or 'none'", call. = FALSE)
  }
  legend.position
}

#' @importFrom dplyr select pull mutate arrange across if_all cur_column filter distinct group_by summarise bind_rows
#' @importFrom tidyselect starts_with matches
validate_data <- function(df,
                          misses_x,
                          misses_category,
                          ...) {
  dots <- list(...)
  type <- validate_type(dots$type, df = NULL) # quick validation
  
  numeric_cols <- names(which(vapply(FUN.VALUE = logical(1), df, function(col) mode(col) == "numeric" & !inherits(col, c("Date", "POSIXTt", "factor")))))
  numeric_cols <- numeric_cols[numeric_cols %unlike% "^_var_"]
  character_cols <- names(which(vapply(FUN.VALUE = logical(1), df, function(col) is.character(col) || is.factor(col))))
  character_cols <- character_cols[character_cols %unlike% "^_var_"]
  non_numeric_cols <- names(which(vapply(FUN.VALUE = logical(1), df, function(col) mode(col) != "numeric" || inherits(col, c("Date", "POSIXTt", "factor")))))
  non_numeric_cols <- non_numeric_cols[non_numeric_cols %unlike% "^_var_"]
  
  if (!has_y(df) && "n" %in% numeric_cols && mode(df$n) == "numeric") {
    # give preference to "n" for the y axis
    plot2_message("Using ", font_blue("y = n"))
    df <- df |> 
      mutate(`_var_y` = df |> pull(n))
    set_plot2_env(y = "n")
  }
  
  if (!has_y(df)) {
    # try to find numeric column for y
    if (is.na(numeric_cols[1L])) {
      stop("no numeric column found to use for y", call. = FALSE)
    }
    if (length(numeric_cols) > 1) {
      # first check if there is also no x axis
      if (!has_x(df)) {
        # make x first numeric column and y second numeric column
        plot2_message("Using ", font_blue("x = ", numeric_cols[1L], collapse = NULL))
        if (!geom_is_continuous_x(type)) {
          # don't show when type for density geoms - y will not be used
          plot2_message("Using ", font_blue("y = ", numeric_cols[2L], collapse = NULL))
        }
        df <- df |> 
          mutate(`_var_x` = df |> pull(numeric_cols[1L]),
                 `_var_y` = df |> pull(numeric_cols[2L]))
        set_plot2_env(x = numeric_cols[1L], y = numeric_cols[2L])
      } else {
        if (!geom_is_continuous_x(type)) {
          # don't show when type for density geoms - y will not be used
          plot2_message("Using ", font_blue("y = ", numeric_cols[1L], collapse = NULL))
        }
        df <- df |> 
          mutate(`_var_y` = df |> pull(numeric_cols[1L]))
        set_plot2_env(y = numeric_cols[1L])
      }
    } else {
      # only one numeric column
      if (geom_is_continuous_x(type)) {
        if (!has_x(df)) {
          plot2_message("Using ", font_blue("x = ", numeric_cols, collapse = NULL))
          df <- df |> 
            mutate(`_var_x` = df |> pull(numeric_cols))
          set_plot2_env(x = numeric_cols)
        }
        # don't show when type for density geoms - y will not be used
        df <- df |> 
          mutate(`_var_y` = df |> pull(`_var_x`))
      } else if (!has_x(df) && type == "" && length(non_numeric_cols) == 0) {
        # has no x and no y, make it a histogram
        plot2_message("Using ", font_blue("x = ", numeric_cols, collapse = NULL))
        plot2_message("Assuming ", font_blue("type = \"histogram\""),
                      " since the data has only one numeric variable and no other variables")
        type <- "geom_histogram"
        df <- df |> 
          mutate(`_var_x` = df |> pull(numeric_cols),
                 `_var_y` = df |> pull(numeric_cols))
        set_plot2_env(x = numeric_cols, y = numeric_cols)
      } else {
        if (has_x(df) && get_x_name(df) == numeric_cols) {
          if (type == "") {
            plot2_message("Assuming ", font_blue("type = \"histogram\""),
                          " since the ", font_blue("x"),
                          " variable (", font_blue(get_x_name(df)), ") is the only numeric variable")
            type <- "geom_histogram"
            df <- df |> 
              mutate(`_var_y` = df |> pull(`_var_x`))
          } else {
            stop("No variable found for y since the x variable (", get_x_name(df),
                 ") is the only numeric variable in the data set.\nDid you mean type = \"histogram\"?", call. = FALSE)
          }
        } else {
          plot2_message("Using ", font_blue("y = ", numeric_cols, collapse = NULL))
          df <- df |> 
            mutate(`_var_y` = df |> pull(numeric_cols))
          set_plot2_env(y = numeric_cols)
        }
      }
    }
  }
  
  # this is required to plot e.g. difftime
  # integers and doubles both return FALSE for requires_numeric_coercion()
  if (has_y(df) &&
      (requires_numeric_coercion(get_y(df)) ||
       "AMR" %in% rownames(utils::installed.packages()) && AMR::is.mic(get_y(df)))) {
    plot2_message(paste0("Coercing values of ", font_blue("y"),
                         font_black(" from class "), font_blue(paste(class(get_y(df)), collapse = "/")),
                         font_black(" to class "), font_blue("double")))
    df <- df |> 
      mutate(`_var_y` = as.double(`_var_y`))
    df[, get_y_name(df)] <- df$`_var_y`
  }
  if (has_x(df) && requires_numeric_coercion(get_x(df))) {
    df <- df |> 
      mutate(`_var_x` = as.double(`_var_x`))
    df[, get_x_name(df)] <- df$`_var_x`
  }
  
  if (misses_x && !has_x(df) && ncol(df) > 1) {
    eligible_cols <- colnames(df)[colnames(df) %unlike% "^(y|_var_(x|y|category|facet|datalabels))$"]
    # take first column if it's not used for y
    if (identical(pull(df, 1), get_y(df))) {
      x_col <- eligible_cols[2L]
    } else {
      x_col <- eligible_cols[1L]
    }
    plot2_message("Using ", font_blue("x = ", x_col, collapse = NULL))
    df <- df |> 
      mutate(`_var_x` = df |> pull(x_col))
    set_plot2_env(x = x_col)
  }
  
  if (misses_x && misses_category && !has_category(df) && ncol(df) > 2 && type != "geom_sf") {
    # category must only be used if factor or character
    # and if x was also missing
    cols <- vapply(FUN.VALUE = logical(1),
                   df,
                   function(col) {
                     (is.factor(col) || is.character(col)) &
                       !identical(get_y(df), col) &
                       !(has_x(df) && identical(get_x(df), col))
                   })
    cols <- names(cols)[cols]
    if (has_facet(df)) {
      # remove columns that are already used for facet
      cols <- cols[!cols %in% c("_var_facet", get_facet_name(df))]
    }
    if (length(cols) > 0) {
      plot2_message("Using ", font_blue("category = ", cols[1L], collapse = NULL))
      df <- df |> 
        mutate(`_var_category` = df |> pull(cols[1L]))
      set_plot2_env(category = cols[1L])
    }
  }
  if (type == "geom_sf" && misses_category && !has_category(df) && !is.na(numeric_cols[1L])) {
    # try to take the first numeric column for 'sf' plots
    plot2_message("Using ", font_blue("category = ", numeric_cols[1L], collapse = NULL))
    df <- df |> 
      mutate(`_var_category` = df |> pull(numeric_cols[1L]))
    set_plot2_env(category = numeric_cols[1L])
  }
  
  # if given FALSE for a direction (e.g., category = FALSE), remove these columns
  if (has_category(df) && all(get_category(df) == FALSE)) {
    df <- df |> select(-`_var_category`)
    plot2_env$mapping_category <- NULL
  }
  if (has_facet(df) && all(get_facet(df) == FALSE)) {
    df <- df |> select(-`_var_facet`)
    plot2_env$mapping_facet <- NULL
  }
  if (has_datalabels(df) && 
      (all(get_datalabels(df) == FALSE) ||
       (!is.null(dots$type) && dots$type != "sf" &&
        geom_is_continuous(suppressMessages(validate_type(dots$type, df))) &&
        isTRUE(!type %in% c("geom_tile", "geom_raster", "geom_rect"))))) {
    # remove datalabels if `datalabels = FALSE`, or if the type now seems to be continuous
    df <- df |> select(-`_var_datalabels`)
  }
  
  # if the secondary y axis is not within the limits of the primary y axis, the primary axis will be transformed
  # so store the factor in which they change, and transform the data accordingly
  if (has_y_secondary(df)) {
    max_primary <- max(get_y(df), na.rm = TRUE)
    max_secondary <- max(get_y_secondary(df), na.rm = TRUE)
    # if 25% difference, create own secondary y breaks
    if (abs((max_secondary - max_primary) / max_secondary) >= 0.25)  {
      plot2_env$y_secondary_factor <- max_secondary / max_primary
      df$`_var_y_secondary` <- df$`_var_y_secondary` / plot2_env$y_secondary_factor
    }
  }
  
  # add surrogate columns to df
  if (has_x(df) && !is.null(plot2_env$mapping_x) &&
      !plot2_env$mapping_x %in% colnames(df) && plot2_env$mapping_x != "NULL") {
    df$`_label_x` <- get_x(df)
    colnames(df)[colnames(df) == "_label_x"] <- paste0(trimws(plot2_env$mapping_x), collapse = " ")
  }
  if (has_y(df) && !is.null(plot2_env$mapping_y) &&
      !plot2_env$mapping_y %in% colnames(df) && plot2_env$mapping_y != "NULL") {
    df$`_label_y` <- get_y(df)
    colnames(df)[colnames(df) == "_label_y"] <- paste0(trimws(plot2_env$mapping_y), collapse = " ")
  }
  if (has_category(df) && !is.null(plot2_env$mapping_category) &&
      !plot2_env$mapping_category %in% colnames(df) && plot2_env$mapping_category != "NULL") {
    df$`_label_category` <- get_category(df)
    colnames(df)[colnames(df) == "_label_category"] <- paste0(trimws(plot2_env$mapping_category), collapse = " ")
  }
  if (has_facet(df) && !is.null(plot2_env$mapping_facet) &&
      !plot2_env$mapping_facet %in% colnames(df) && plot2_env$mapping_facet != "NULL") {
    df$`_label_facet` <- get_facet(df)
    colnames(df)[colnames(df) == "_label_facet"] <- paste0(trimws(plot2_env$mapping_facet), collapse = " ")
  }
  if (has_y_secondary(df) && !is.null(plot2_env$mapping_y_secondary) &&
      !plot2_env$mapping_y_secondary %in% colnames(df) && plot2_env$mapping_y_secondary != "NULL") {
    df$`_label_y_secondary` <- get_y_secondary(df)
    colnames(df)[colnames(df) == "_label_y_secondary"] <- paste0(trimws(plot2_env$mapping_y_secondary), collapse = " ")
  }
  
  if (has_datalabels(df)) {
    if (all(get_datalabels(df) == TRUE)) {
      # for when given: datalabels = TRUE, guess the results
      if (type == "geom_sf") {
        # take values from first character column in case of sf plots
        if (!is.na(character_cols[1L])) {
          plot2_message("Using ", font_blue("datalabels = ", character_cols[1L], collapse = NULL))
          df <- df |> mutate(`_var_datalabels` = df |> pull(character_cols[1L]))
        } else {
          plot2_warning("No suitable column found for ", font_blue("datalabels = TRUE"))
          df <- df |> select(-`_var_datalabels`)
        }
      } else if (has_category(df) && type %in% c("geom_tile", "geom_raster", "geom_rect")) {
        # take the values from the category column
        df <- df |> mutate(`_var_datalabels` = `_var_category`)
      } else {
        # otherwise take values from the y column
        df <- df |> mutate(`_var_datalabels` = `_var_y`)
      }
    }
    # format datalabels
    if (requires_numeric_coercion(get_datalabels(df))) {
      # force double for e.g. difftime
      df <- df |> 
        mutate(`_var_datalabels` = as.double(`_var_datalabels`))
    }
    df <- df |>
      mutate(`_var_datalabels` = format_datalabels(`_var_datalabels`, 
                                                   datalabels.round = dots$datalabels.round,
                                                   datalabels.format = dots$datalabels.format,
                                                   decimal.mark = dots$decimal.mark,
                                                   big.mark = dots$big.mark,
                                                   y.percent = dots$y.percent))
  }
  
  # turn x to character if data seems to suggest so
  if (has_x(df) &&!isTRUE(dots$x.character)) {
    if (is.null(dots$x.character) &&
        is.numeric(get_x(df)) &&
        all(get_x(df, na.rm = TRUE) >= 2000) &&
        all(get_x(df, na.rm = TRUE) <= 2050)) {
      plot2_message("Assuming ", font_blue("x.character = TRUE"),
                    " since the ", font_blue("x"), " labels seem to be years")
      dots$x.character <- TRUE
    } else if (is.null(dots$x.character) &&
               is.numeric(get_x(df)) &&
               (identical(sort(unique(get_x(df))), seq_len(12)) ||
                identical(sort(unique(get_x(df))), as.double(seq_len(12))))) {
      plot2_message("Assuming ", font_blue("x.character = TRUE"),
                    " since the ", font_blue("x"), " labels seem to be months")
      dots$x.character <- TRUE
    } else if (is.numeric(get_x(df)) &&
               !type %in% c("", "geom_blank") &&
               !geom_is_continuous(type)) {
      plot2_message("Using ", font_blue("x.character = TRUE"),
                    " for discrete plot type (", font_blue(type), ")",
                    " since ", font_blue(get_x_name(df)), " is numeric")
      dots$x.character <- TRUE
    } else if (is.numeric(get_x(df)) &&
               !is.null(dots$x.sort)) {
      plot2_message("Using ", font_blue("x.character = TRUE"),
                    " since ", font_blue("x.sort"), " is set")
      dots$x.character <- TRUE
    }
  }
  if (isTRUE(dots$x.character)) {
    df <- df |>
      mutate(`_var_x` = as.character(`_var_x`))
  }
  # turn category to character if data seems to suggest so
  if (has_category(df) &&!isTRUE(dots$category.character)) {
    if (is.null(dots$category.character) &&
        is.numeric(get_category(df)) &&
        all(get_category(df, na.rm = TRUE) >= 2000) &&
        all(get_category(df, na.rm = TRUE) <= 2050)) {
      plot2_message("Assuming ", font_blue("category.character = TRUE"),
                    " since ", font_blue("category"), " seems to be years")
      dots$category.character <- TRUE
    } else if (is.null(dots$category.character) &&
               is.numeric(get_category(df)) &&
               (identical(sort(unique(get_category(df))), seq_len(12)) ||
                identical(sort(unique(get_category(df))), as.double(seq_len(12))))) {
      plot2_message("Assuming ", font_blue("category.character = TRUE"),
                    " since ", font_blue("category"), " seems to be months")
      dots$category.character <- TRUE
    } else if (is.numeric(get_category(df)) &&
               is.null(dots$category.character) &&
               !geom_is_continuous(type)) {
      type_prelim <- type
      if (type_prelim == "") {
        # get preliminary type
        type_prelim <- tryCatch(suppressMessages(validate_type("", df = df)), error = function(e) "geom_col")
      }
      if (!geom_is_continuous(type_prelim)) {
        plot2_message("Assuming ", font_blue("category.character = TRUE"),
                      " for discrete plot type (", font_blue(type_prelim), ")",
                      " since ", font_blue(get_category_name(df)), " is numeric")
        dots$category.character <- TRUE
      }
    }
  }
  if (isTRUE(dots$category.character) && has_category(df)) {
    df <- df |>
      mutate(`_var_category` = as.character(`_var_category`))
  }
  
  # remove infinite values
  if (has_y(df) && any(is.infinite(get_y(df)), na.rm = TRUE)) {
    inf_values <- sum(is.infinite(get_y(df)))
    df <- df |> filter(!is.infinite(`_var_y`))
    plot2_message("Removed ", inf_values,
                  " row", ifelse(inf_values > 1, "s", ""),
                  " with an infinite value of ",
                  font_blue("y"), 
                  ifelse(get_y_name(df) != "y",
                         paste0(font_black(" ("), font_blue(get_y_name(df)), font_black(")")),
                         ""))
  }
  # replace int64 values
  if (has_y(df) && inherits(get_y(df), "integer64")) {
    df <- df |> 
      mutate(`_var_y` = as.integer(`_var_y`))
    df[, get_y_name(df)] <- df$`_var_y`
    plot2_message("Replaced integer64 values of ",
                  font_blue("y"), 
                  ifelse(get_y_name(df) != "y",
                         paste0(font_black(" ("), font_blue(get_y_name(df)), font_black(")")),
                         ""),
                  " with regular integers")
  }
  
  # complete data
  df <- df |> 
    complete_direction("x", has_x(df), dots$x.complete) |> 
    complete_direction("category", has_category(df), dots$category.complete) |> 
    complete_direction("facet", has_facet(df), dots$facet.complete)
  
  # remove or replace NAs
  rows_with_NA <- df |>
    select(c(get_x_name(df), get_category_name(df), get_facet_name(df),
             matches("_var_(x|category|facet)"))) |> 
    stats::na.omit() |>
    attributes()
  rows_with_NA <- as.double(rows_with_NA$na.action)
  if (length(rows_with_NA) > 0) {
    # so some are NAs
    if (isTRUE(dots$na.rm)) {
      plot2_message("Removed ", length(rows_with_NA),
                    " row", ifelse(rows_with_NA > 1, "s", ""),
                    " since ", font_blue("na.rm = TRUE"))
      df <- df[-rows_with_NA, , drop = FALSE]
    } else {
      # replace NAs
      plot2_env$na_replaced <- 0
      plot2_env$na_replaced_vars <- character(0)
      is_numeric <- function(x) {
        mode(x) == "numeric" || is.numeric(x) || inherits(x, c("Date", "POSIXt"))
      }
      df <- df |>
        mutate(across(c(get_x_name(df), get_category_name(df), get_facet_name(df),
                        matches("_var_(x|category|facet)")),
                      function(x) {
                        if (is.factor(x) && any(is.na(x))) {
                          # add as last factor level
                          levels(x) <- c(levels(x), dots$na.replace)
                        }
                        if ((!is_numeric(x) || is.factor(x)) && any(is.na(x))) {
                          plot2_env$na_replaced_vars <- c(plot2_env$na_replaced_vars, cur_column())
                          plot2_env$na_replaced <- plot2_env$na_replaced + sum(is.na(x))
                          x[is.na(x)] <- dots$na.replace
                        }
                        x
                      }))
      if (plot2_env$na_replaced > 0) {
        plot2_env$na_replaced_vars <- plot2_env$na_replaced_vars[plot2_env$na_replaced_vars %unlike% "^_var_"]
        plot2_message("Replacing ", font_magenta("NA"),
                      " in column", ifelse(length(plot2_env$na_replaced_vars) > 1, "s ", " "),
                      paste(font_blue(plot2_env$na_replaced_vars, collapse = NULL), collapse = " and "),
                      " using ", font_blue(paste0("na.replace = \"", dots$na.replace, "\"")))
      }
    }
  }
  if (anyNA(df$`_var_y`)) {
    plot2_warning(paste0("Unable to plot ", sum(is.na(df$`_var_y`)),
                         " value", ifelse(sum(is.na(df$`_var_y`)) > 1, "s", ""),
                         " where ", get_y_name(df), " = NA"))
    df <- df |> 
      filter(!is.na(`_var_y`))
  }
  
  # check if years on x should be removed
  if (is.null(dots$x.date_remove_years) && 
      has_category(df) &&
      has_x(df) &&
      inherits(get_x(df), c("Date", "POSIXt")) &&
      all(format(get_x(df), "%Y") == get_category(df), na.rm = TRUE)) {
    plot2_message("Assuming ", font_blue("x.date_remove_years = TRUE"),
                  " since ", font_blue("category"), " contains the years of ", font_blue(get_x_name(df)))
    dots$x.date_remove_years <- TRUE
  }
  if (isTRUE(dots$x.date_remove_years) && has_x(df) && inherits(get_x(df), c("Date", "POSIXt"))) {
    df <- df |> mutate(`_var_x` = unify_years(get_x(df)))
    df[, get_x_name(df)] <- df$`_var_x`
  }
  
  # apply sorting
  df.bak <- df
  if (has_x(df) && type != "geom_sf") {
    if (dots$type_backup != "sankey") {
      if (is.null(dots$x.sort) && inherits(get_x(df), c("character", "factor"))) {
        dots$x.sort <- TRUE
      }
      df <- df |> 
        mutate(`_var_x` = sort_data(values = get_x(df),
                                    original_values = get_x(df.bak),
                                    sort_method = dots$x.sort,
                                    datapoints = get_y(df),
                                    summarise_function = dots$summarise_function,
                                    summarise_fn_name = dots$summarise_fn_name,
                                    horizontal = dots$horizontal,
                                    drop = dots$x.drop,
                                    argument = "x.sort")) |>
        arrange(across(`_var_x`))
      df[, get_x_name(df)] <- df$`_var_x` # required to keep sorting after summarising
    }
  }
  if (has_category(df)) {
    df <- df |> 
      mutate(`_var_category` = sort_data(values = get_category(df),
                                         original_values = get_category(df.bak),
                                         sort_method = dots$category.sort,
                                         datapoints = get_y(df),
                                         summarise_function = dots$summarise_function,
                                         summarise_fn_name = dots$summarise_fn_name,
                                         horizontal = dots$horizontal,
                                         drop = TRUE,
                                         argument = "category.sort"))
    df[, get_category_name(df)] <- df$`_var_category` # required to keep sorting after summarising
  }
  if (has_facet(df)) {
    df <- df |> 
      mutate(`_var_facet` = sort_data(values = get_facet(df),
                                      original_values = get_facet(df.bak),
                                      sort_method = dots$facet.sort,
                                      datapoints = get_y(df),
                                      summarise_function = dots$summarise_function,
                                      summarise_fn_name = dots$summarise_fn_name,
                                      horizontal = FALSE, # never reversely sort when horizontal
                                      drop = TRUE,
                                      argument = "facet.sort"))
    df[, get_facet_name(df)] <- df$`_var_facet` # required to keep sorting after summarising
  }
  
  # sankey plot reorganisation to be able to use {ggforce} later on
  if (dots$type_backup == "sankey") {
    x_names <- plot2_env$x_variable_names
    df_new <- df |> 
      mutate(`_sankey_split` = df |> pull(x_names[1]) |> as.character(),
             `_sankey_x` = x_names[1],
             `_sankey_id` = seq_len(nrow(df)))
    for (i in 2:length(x_names)) {
      df_new <- df_new |> 
        bind_rows(df |> 
                    mutate(`_sankey_split` = df |> pull(x_names[i]) |> as.character(),
                           `_sankey_x` = x_names[i],
                           `_sankey_id` = seq_len(nrow(df))))
    }
    # make sure that order of x gets preserved
    if (is.null(dots$x.sort)) {
      dots$x.sort <- "inorder"
    }
    df_new$`_sankey_x` <- sort_data(values = df_new$`_sankey_x`,
                                    original_values = df_new$`_sankey_x`,
                                    sort_method = dots$x.sort,
                                    datapoints = df_new$`_var_y`,
                                    summarise_function = dots$summarise_function,
                                    summarise_fn_name = dots$summarise_fn_name,
                                    horizontal = dots$horizontal,
                                    drop = dots$x.drop,
                                    argument = "x.sort")
    df <- df_new
  }
  
  # very last part before setting max items - everything has been transformed as needed.
  # are the data distinct, were tidyverse language selectors used in the right way?
  type_validated <- suppressMessages(validate_type(dots$type, df))
  if ((!geom_is_continuous(type_validated) || geom_is_line_or_area(type_validated)) &&
      !is.null(dots$summarise_function) &&
      dots$type_backup != "sankey" &&
      (df |> select(starts_with("_var")) |> distinct() |> nrow()) < nrow(df)) {
    y_name <- get_y_name(df)
    df <- df |> 
      group_by(across(c(get_x_name(df), get_category_name(df), get_facet_name(df),
                        matches("_var_(x|category|facet)")))) |> 
      summarise(`_var_y` = dots$summarise_function(`_var_y`),
                .groups = "drop")
    df[, y_name] <- df$`_var_y`
    
    if (!dots$summarise_fn_name %in% c("summarise_function", "function(x) x")) {
      plot2_warning("Values in ", font_blue("y"), " were not summarised, now using ",
                    font_blue(paste0("y = ", dots$summarise_fn_name, "(", get_y_name(df), ")")), " since ",
                    font_blue(paste0("summarise_function = ", dots$summarise_fn_name)), " was set.\n",
                    "  When using a transformation function on ", font_blue("x"), 
                    ifelse(has_category(df), paste0(" or ", font_blue("category")), ""),
                    ifelse(has_facet(df), paste0(" or ", font_blue("facet")), ""),
                    ", also use a summarising function on ", font_blue("y"), ".")
    }
  }
  
  if (type != "geom_sf") {
    # apply limitations (have to been after sorting, e.g. on frequency)
    df <- set_max_items(df = df,
                        y = get_y(df),
                        x = get_x_name(df),
                        x.max_items = dots$x.max_items,
                        x.max_txt = dots$x.max_txt,
                        category = get_category_name(df), 
                        category.max_items = dots$category.max_items,
                        category.max_txt = dots$category.max_txt,
                        facet = get_facet_name(df), 
                        facet.max_items = dots$facet.max_items, 
                        facet.max_txt = dots$facet.max_txt,
                        horizontal = dots$horizontal,
                        summarise_function = dots$summarise_function,
                        decimal.mark = dots$decimal.mark,
                        big.mark = dots$big.mark,
                        datalabels.round = dots$datalabels.round,
                        datalabels.format = dots$datalabels.format,
                        y.percent = dots$y.percent)
    # sort on x, important when piping plot2()'s after plot2()'s
    if (has_x(df)) {
      df <- df |> 
        arrange(across(`_var_x`))
    }
  }
  
  # return output
  df
}

#' @importFrom dplyr mutate across
validate_taxonomy <- function(df) {
  if (!has_x(df)) {
    return(df)
  }
  suppressWarnings(requireNamespace("AMR", quietly = TRUE))
  taxonomic_nms <- unique(c(AMR::microorganisms$family,
                            AMR::microorganisms$genus,
                            AMR::microorganisms$species,
                            AMR::microorganisms$subspecies))
  make_taxonomy <- function(x, nms = taxonomic_nms) {
    if (is.null(x)) {
      return(NULL)
    }
    out <- vapply(FUN.VALUE = character(1),
                  X = strsplit(x, " "),
                  FUN = function(nm) {
                    if (!all(is.na(nm))) {
                      nm[nm %in% nms] <- paste0("*", nm[nm %in% nms], "*")
                      nm <- paste0(nm, collapse = " ")
                      nm <- gsub("(.*)([A-Z][.]) [*]([a-z]+)[*](.*)", "\\1*\\2 \\3*\\4", nm, perl = TRUE)
                    } else if (length(nm) == 0) {
                      # this is because of `strsplit("", " ")`
                      nm <- ""
                    }
                    nm
                  },
                  USE.NAMES = FALSE)
    out <- gsub("* *", " ", out, fixed = TRUE)
    out
  }
  taxonomy_to_chr_expression <- function(x) {
    with_taxonomy <- make_taxonomy(as.character(x))
    if (any(with_taxonomy %like% "[*].+[*]", na.rm = TRUE)) {
      out <- vapply(with_taxonomy,
                    FUN.VALUE = character(1),
                    function(y) {
                      as.character(md_to_expression(y))
                    },
                    USE.NAMES = FALSE)
      if (is.factor(x)) {
        # take order of levels from original sorting, since e.g. `x.sort = "freq-desc"` may have been applied
        factor(out, levels = out[match(levels(x), x)], ordered = is.ordered(x))
      } else {
        out
      }
    } else {
      # no taxonomic values found
      x
    }
  }
  df <- df |>
    mutate(across(get_x_name(df), taxonomy_to_chr_expression))
  df$`_var_x` <- df[, get_x_name(df), drop = TRUE]
  df
}

#' @importFrom ggplot2 scale_x_discrete scale_x_date scale_x_datetime scale_x_continuous expansion waiver
#' @importFrom scales reverse_trans pretty_breaks
validate_x_scale <- function(df,
                             values,
                             x.date_breaks,
                             x.date_labels,
                             x.breaks,
                             x.n_breaks,
                             x.expand,
                             x.labels,
                             x.limits,
                             x.position,
                             x.transform,
                             x.drop,
                             x.zoom,
                             facet.fixed_x,
                             decimal.mark,
                             big.mark,
                             horizontal,
                             type_backup) {
  
  if (isTRUE(x.zoom) && is.null(x.limits)) {
    x.limits <- c(NA_real_, NA_real_)
    if (is.null(x.expand)) {
      # set default value to 0.5
      x.expand <- 0.5
    }
  }
  
  set_x.expand <- !is.null(x.expand)
  if (is.null(x.expand)) {
    if (is.null(x.limits)) {
      # set default value to 0.5
      x.expand <- 0.5
    } else {
      if (!inherits(values, c("Date", "POSIXt"))) {
        plot2_message("Assuming ", font_blue("x.expand = 0"), " since ", font_blue("x.limits"), " is set")
        x.expand <- 0
      } else {
        # dates - no need mention that x.expand is set to 0.5 - it's already the default
        x.expand <- 0.5
      }
    }
  }
  if (is.null(x.transform)) {
    x.transform <- "identity"
  }
  
  if (is.null(facet.fixed_x) && is.null(x.limits) && has_facet(df) && mode(get_x(df)) == "numeric" && !is.factor(get_x(df))) {
    # determine if scales should be fixed - if CV_xmax < 25% then fix them:
    x_maxima <- df |>
      group_by(across(get_facet_name(df))) |> 
      summarise(max = as.double(max(`_var_x`, na.rm = TRUE)))
    if (!any(is.infinite(x_maxima$max), na.rm = TRUE)) {   
      coeff_of_variation <- stats::sd(x_maxima$max) / mean(x_maxima$max)
      if (coeff_of_variation < 0.25) {
        plot2_message("Assuming ", font_blue("facet.fixed_x = TRUE"), 
                      " since the ", digit_to_text(nrow(x_maxima)), " ",
                      font_blue("x"), " scales differ by less than 25%")
        facet.fixed_x <- TRUE
      }
    }
  }
  
  if (!is.null(x.limits)) {
    if (length(x.limits) != 2) {
      if (length(x.limits) == 1) {
        x.limits <- rep(x.limits, 2)
      } else {
        stop("`x.limits` must be of length 1 or 2", call. = FALSE)
      }
    }
    if (inherits(values, "Date")) {
      x.limits <- as.Date(x.limits, origin = "1970-01-01")
    } else if (inherits(values, "POSIXt")) {
      x.limits <- as.POSIXct(x.limits, origin = "1970-01-01")
    }
    if (inherits(values, c("Date", "POSIXt"))) {
      # edit limits so x.limits has one spare one at each side
      x.limits[1] <- x.limits[1] - 1
      x.limits[2] <- x.limits[2] + 1
      # strip that extra ones from x.expand, so that all columns will plot
      if (!is.function(x.expand)) {
        x.expand <- x.expand - 1
      }
    }
  }
  
  if (!is.function(x.expand) && !set_x.expand) {
    if (length(x.expand) == 1) {
      x.expand <- c(x.expand, x.expand)
    }
    x.expand <- expansion(mult = ifelse(inherits(values, c("Date", "POSIXt")), 0.05, 0),
                          add = x.expand)
  }
  
  if (inherits(values, c("Date", "POSIXt"))) {
    auto_breaks_labels <- determine_date_breaks_labels(values)
    if (is.null(x.date_breaks)) {
      x.date_breaks <- auto_breaks_labels$breaks
      plot2_message("Using ", font_blue("x.date_breaks = \"", x.date_breaks, "\"", collapse = ""),
                    " based on data")
    }
    if (is.null(x.date_labels)) {
      x.date_labels <- auto_breaks_labels$labels
      plot2_message("Using ", font_blue("x.date_labels = \"", x.date_labels, "\"", collapse = ""),
                    " based on data")
    }
  }
  if (inherits(values, "Date")) {
    scale_x_date(position = x.position,
                 date_breaks = x.date_breaks,
                 date_labels = posix_date_format(x.date_labels),
                 expand = x.expand,
                 limits = x.limits, 
                 labels = if (is.null(x.labels)) waiver() else x.labels)
  } else if (inherits(values, "POSIXt")) {
    scale_x_datetime(position = x.position,
                     date_breaks = x.date_breaks,
                     date_labels = posix_date_format(x.date_labels),
                     expand = x.expand,
                     limits = x.limits, 
                     labels = if (is.null(x.labels)) waiver() else x.labels)
  } else {
    if (!is.numeric(values)) {
      scale_x_discrete(position = x.position,
                       drop = x.drop, 
                       labels = if (is.null(x.labels)) waiver() else x.labels,
                       expand = if (set_x.expand) x.expand else waiver())
    } else {
      if (x.transform == "identity" && isTRUE(horizontal)) {
        x.transform <- reverse_trans()
      }
      if (is.null(x.limits)) {
        x.limits <- c(ifelse(min(values) < 0, NA_real_, 0), NA_real_)
      }
      if (tryCatch(length(x.transform) == 1 && x.transform != "identity", error = function(x) FALSE)) {
        # some transformations, such as log, do not allow 0
        x.limits[x.limits == 0] <- NA_real_ 
      }
      if (is.null(x.labels)) {
        x.labels <- function(x, dec_mark = decimal.mark, big_mark = big.mark, ...) {
          format2(x,
                  round = max(2, sigfigs(diff(range(x, na.rm = TRUE))) + 1),
                  decimal.mark = dec_mark,
                  big.mark = big_mark)
        }
      }
      breaks_fn <- function(values, x.breaks, x.n_breaks, waiver) {
        if (!is.null(x.breaks)) {
          x.breaks
        } else if (all(values %% 1 == 0, na.rm = TRUE) && max(values, na.rm = TRUE) < 5) {
          # whole numbers - only strip decimal numbers if total y range is low
          function(x, ...) unique(floor(pretty(seq(0, (max(x, na.rm = TRUE) + 1) * 3))))
        } else {
          pretty_breaks(n = ifelse(is.null(x.n_breaks), 5, x.n_breaks))
        }
      }
      
      if (set_x.expand == FALSE) {
        if (min(values, na.rm = TRUE) >= 0) {
          # really start at zero
          x.expand <- expansion(mult = c(0.025, 0.2))
        } else {
          # leave some space to the left, same as to the right
          x.expand <- expansion(mult = c(0.2, 0.2))
        }
      } else if (length(x.expand) == 1) {
        x.expand <- expansion(mult = c(x.expand, x.expand))
      } else if (length(x.expand) == 2) {
        x.expand <- expansion(mult = x.expand)
      }
      
      if (isTRUE(facet.fixed_x)) {
        if (!is.na(x.limits[1])) {
          if (x.limits[1] == 0) {
            min_lim <- 0
          } else {
            min_lim <- min(values, na.rm = TRUE)
          }
        } else {
          min_lim <- 0
        }
        max_lim <- max(values, na.rm = TRUE)
        x.limits <- c(min_lim, max_lim)
      }
      scale_x_continuous(labels = x.labels,
                         breaks = breaks_fn(values = values,
                                            x.breaks = x.breaks,
                                            x.n_breaks = x.n_breaks,
                                            waiver = waiver()),
                         n.breaks = x.n_breaks,
                         transform = x.transform,
                         position = x.position,
                         limits = x.limits,
                         expand = x.expand)
    }
  }
}

#' @importFrom dplyr group_by across summarise
#' @importFrom ggplot2 waiver expansion scale_y_continuous sec_axis
#' @importFrom cleaner as.percentage
#' @importFrom scales pretty_breaks
validate_y_scale <- function(df,
                             type,
                             y.24h,
                             y.age,
                             y.scientific,
                             y.breaks,
                             y.n_breaks,
                             y.expand,
                             y.labels,
                             y.limits,
                             y.percent,
                             y.percent_break,
                             misses_y.percent_break,
                             y.position,
                             y.transform,
                             y.zoom,
                             stacked,
                             stacked_fill,
                             facet.fixed_y,
                             decimal.mark,
                             big.mark,
                             add_y_secondary,
                             y_secondary.breaks = NULL,
                             y_secondary.title = NULL,
                             y_secondary.scientific = NULL,
                             y_secondary.percent = NULL,
                             y_secondary.labels = NULL,
                             markdown = NULL) {
  if (isTRUE(y.zoom) && is.null(y.limits)) {
    y.limits <- c(NA_real_, NA_real_)
    if (is.null(y.expand)) {
      y.expand <- c(0.25, 0.25)
    }
  }
  if (is.null(y.transform)) {
    y.transform <- "identity"
  }
  if (is.null(y.expand)) {
    if (is.null(y.limits)) {
      # set default value to 0.25
      y.expand <- 0.25
    } else {
      plot2_message("Assuming ", font_blue("y.expand = 0"), " since ", font_blue("y.limits"), " is set")
      y.expand <- 0
    }
  }
  if (!is.null(y.limits) && length(y.limits) != 2) {
    if (length(y.limits) == 1) {
      y.limits <- rep(y.limits, 2)
    } else {
      stop("`y.limits` must be of length 1 or 2", call. = FALSE)
    }
  }
  
  values <- get_y(df)
  if (mode(values) != "numeric") {
    stop("The y scale must be numeric for plot type '", gsub("geom_", "", type), "' (current y class: ",
         paste0(class(values), collapse = "/"), ").",
         call. = FALSE)
  }
  
  if (is.null(facet.fixed_y) && is.null(y.limits) && has_facet(df) && !isTRUE(stacked_fill) && type != "geom_histogram") {
    # determine if scales should be fixed - if CV_ymax < 25% then fix them:
    # (this does not work for facetted histograms)
    y_maxima <- df |>
      group_by(across(get_facet_name(df))) |> 
      summarise(max = as.double(max(`_var_y`, na.rm = TRUE)))
    if (!any(is.infinite(y_maxima$max), na.rm = TRUE)) {   
      coeff_of_variation <- stats::sd(y_maxima$max) / mean(y_maxima$max)
      if (coeff_of_variation < 0.25) {
        plot2_message("Assuming ", font_blue("facet.fixed_y = TRUE"), 
                      " since the ", digit_to_text(nrow(y_maxima)), " ",
                      font_blue("y"), " scales differ by less than 25%")
        facet.fixed_y <- TRUE
        plot2_env$facet.fixed_y <- TRUE
      }
    }
  }
  
  breaks_fn <- function(values, waiver,
                        y.breaks, y.n_breaks, y.expand, stacked_fill,
                        y.age, y.percent, y.percent_break, y.24h, y.limits,
                        y.transform) {
    data_min <- min(0, values, na.rm = TRUE) * - (1 + y.expand[length(y.expand)])
    data_max <- max(values, na.rm = TRUE)
    if (!inherits(values, c("Date", "POSIXt"))) {
      data_max <- data_max * (1 + y.expand[length(y.expand)])
    }
    if (y.transform != "identity") {
      if (!is.null(y.breaks)) {
        plot2_warning("Ignoring ", font_blue("y.breaks"), " since ",
                      font_blue(paste0("y.transform = \"", y.transform, "\"")))
      }
      return(waiver)
    } else if (!is.null(y.breaks)) {
      y.breaks
    } else if (isTRUE(y.age)) {
      # no decimal numbers, generate max 12 labels
      function(x, ...) {
        seq(from = min(0, x, na.rm = TRUE),
            to = min(120, max(x, na.rm = TRUE), na.rm = TRUE),
            by = 10)
      }
    } else if (isTRUE(y.24h)) {
      function(x, ...) {
        seq(from = min(0, x, na.rm = TRUE),
            to = max(x, na.rm = TRUE),
            by = 24)
      }
    } else if (isTRUE(stacked_fill)) {
      # special case of y.percent, where the y scale is always 0 to 1
      function(x, y_percent_break = y.percent_break, ...) {
        seq(from = min(0, x, na.rm = TRUE),
            to = max(x, na.rm = TRUE),
            by = y_percent_break)
      }
    } else if (isTRUE(y.percent)) {
      # calculate how many labels will be printed, keep around 10
      if (is.null(y.limits)) {
        y.limits <- c(data_min, data_max)
      }
      labels_n <- (max(y.limits) - min(y.limits)) / y.percent_break
      if (is.na(labels_n)) {
        labels_n <- 10
      }
      if (isTRUE(misses_y.percent_break) && as.integer(labels_n) > 10) {
        y.percent_break <- round((max(y.limits, na.rm = TRUE) - min(y.limits, na.rm = TRUE)) / 10, 2)
        plot2_message("Using ", font_blue("y.percent_break =", y.percent_break),
                      " (", y.percent_break * 100, "%) to keep a maximum of ~10 labels")
      }
      if (!all(is.na(y.limits)) && (y.percent_break >= max(y.limits, na.rm = TRUE) || labels_n <= 3)) {
        y.percent_break.bak <- y.percent_break
        y.percent_break <- max(y.limits, na.rm = TRUE) / 6.5
        allowed <- c(1e6 / 10 ^ c(1:18), 5e6 / 10 ^ c(1:18))
        y.percent_break <- allowed[which.min(abs(allowed - y.percent_break))]
        plot2_message("Using ", font_blue("y.percent_break =", format(y.percent_break, scientific = FALSE)), 
                      " since the original setting (", font_blue(y.percent_break.bak), ")",
                      " would yield too few labels")
      }
      function(x, ...) {
        seq(from = min(0, x, na.rm = TRUE),
            to = max(x, na.rm = TRUE),
            by = y.percent_break)
      }
      
    } else if (all(values %% 1 == 0, na.rm = TRUE) && data_max < 5) {
      # whole numbers - only strip decimal numbers if total y range is low
      function(x, ...) {
        unique(floor(pretty(seq(0, (max(x, na.rm = TRUE) + 1) * 3))))
      }
    } else {
      pretty_breaks(n = ifelse(is.null(y.n_breaks), 5, y.n_breaks))
    }
  }
  
  labels_fn <- function(values, waiver,
                        y.labels,
                        y.age, y.scientific, y.percent, y.24h, stacked_fill,
                        decimal.mark, big.mark) {
    if (!is.null(y.labels)) {
      y.labels
    } else if (isTRUE(y.scientific)) {
      format2_scientific
    } else if (isTRUE(y.24h)) {
      function(x, dec = decimal.mark, big = big.mark, ...) {
        paste0(format2(x, decimal.mark = dec, big.mark = big),
               ifelse(Sys.getlocale("LC_COLLATE") %like% "nl|dutch", "u (", "h ("),
               x / 24,
               "d)")
      }
    } else if (isTRUE(y.age)) {
      function(x, dec = decimal.mark, big = big.mark, ...) {
        paste0(format2(x, decimal.mark = dec, big.mark = big, round = 0),
               ifelse(Sys.getlocale("LC_COLLATE") %like% "nl|dutch", " jr", " yrs"))
      }
    } else if (isTRUE(y.percent) || isTRUE(stacked_fill)) {
      function(x, dec = decimal.mark, big = big.mark, ...) {
        format2(as.percentage(x), round = max(1, sigfigs(x) - 2), decimal.mark = dec, big.mark = big)
      }
    } else {
      function(x, dec = decimal.mark, big = big.mark, ...) {
        is_scientific <- any(format(x) %like% "^(-?[0-9.]+e-?[0-9.]+)$", na.rm = TRUE) ||
          diff(range(values, na.rm = TRUE)) > 10e5
        non_unique <- length(unique(format2(x[!is.na(x)]))) < length(format2(x[!is.na(x)]))
        if (isTRUE(non_unique) || (isTRUE(is_scientific) && is.null(y.scientific))) {
          if (isTRUE(is_scientific)) {
            plot2_message("Assuming ", font_blue("y.scientific = TRUE"))
          }
          # scientific notation or non-unique labels
          format2_scientific(x, decimal.mark = dec, big.mark = big)
        } else {
          format2(x, decimal.mark = dec, big.mark = big)
        }
      }
    }
  }
  
  limits_fn <- function(values, y.limits,
                        y.expand, facet.fixed_y, y.age, y.transform,
                        df) {
    min_value <- min(0, min(values, na.rm = TRUE))
    if (y.transform != "identity") {
      # in certain transformations, such as log, 0 is not allowed
      min_value <- NA_real_
    }
    if (!is.null(y.limits)) {
      y.limits[y.limits == 0] <- min_value
      y.limits
    } else if (isTRUE(y.age)) {
      # so no function, but force a vector (y.expand is needed since it won't expand)
      c(min_value, max(values, na.rm = TRUE) * (1 + y.expand))
    } else if (has_facet(df) && isTRUE(facet.fixed_y)) {
      if (isTRUE(stacked)) {
        # max has to be determined based per sum on the category level, so calculate sum of y over x and facet
        max_y <- df |> 
          group_by(across(c(get_x_name(df), get_facet_name(df))),
                   .drop = FALSE) |>
          summarise(maximum = sum(`_var_y`, na.rm = TRUE))
        c(min_value, max(max_y$maximum))
      } else {
        if (type == "geom_histogram") {
          plot2_warning("Maximum limit of ", font_blue("y"), " cannot be determined well in histograms when ", font_blue("facet.fixed_y = TRUE"))
        }
        # otherwise, return max per y
        c(min_value, max(values, na.rm = TRUE))
      }
    } else {
      function(x, y_expand = y.expand, min_val = min_value, ...) c(min(min_val, x, na.rm = TRUE), max(x, na.rm = TRUE))
    }
  }
  
  expand_fn <- function(values, y.expand, y.age, stacked_fill, y.limits, type_backup) {
    if (is.function(y.expand)) {
      y.expand
    } else if (isTRUE(y.age) || isTRUE(stacked_fill)) {
      expansion(mult = c(0, 0))
    } else {
      y.expand.bak <- y.expand
      if (length(y.expand) == 1) {
        y.expand <- rep(y.expand, 2)
      }
      if (is.numeric(y.limits) && length(y.limits[!is.na(y.limits)]) == 2) {
        expansion(mult = c(ifelse(any(values < 0) || is.na(y.limits[1L]), y.expand[1], 0),
                           ifelse(any(values > 0) || is.na(y.limits[2L]), y.expand[2], 0)))
      } else {
        # when y.limits is not numeric, but e.g. a function
        expansion(mult = c(ifelse(any(values < 0) || length(y.expand.bak) == 2, y.expand[1], 0),
                           ifelse(any(values > 0) || length(y.expand.bak) == 2, y.expand[2], 0)))
      }
    }
  }
  
  if (isTRUE(add_y_secondary)) {
    # ggplot2::sec_axis() only supports a simple transformation function to determine the scale
    # so determine it, based on the primary y axis
    secondary_values <- get_y_secondary(df)
    fun <- function(x) x
    br <- y_secondary.breaks
    if (!is.null(plot2_env$y_secondary_factor)) {
      # we previously transformed this variable to stay within the range of the primary y axis,
      # so now transform back for the labels and the breaks
      secondary_values <- secondary_values * plot2_env$y_secondary_factor
      fctr <- eval(plot2_env$y_secondary_factor)
      fun <- function(x) x * fctr
      br <- br * plot2_env$y_secondary_factor
    }
    sec_y <- sec_axis(transform = fun,
                      breaks = br,
                      labels = labels_fn(values = secondary_values,
                                         waiver = waiver(),
                                         y.labels = y_secondary.labels,
                                         y.percent = y_secondary.percent,
                                         y.age = FALSE,
                                         y.24h = FALSE,
                                         y.scientific = y_secondary.scientific,
                                         stacked_fill = stacked_fill,
                                         decimal.mark = decimal.mark,
                                         big.mark = big.mark),
                      name = validate_title(y_secondary.title, markdown = markdown))
  } else {
    sec_y <- waiver()
  }
  
  limits_evaluated <- limits_fn(values = values,
                                y.limits,
                                y.expand = y.expand,
                                facet.fixed_y = facet.fixed_y,
                                y.age = y.age,
                                y.transform = y.transform,
                                df)
  
  scale_y_continuous(
    breaks = breaks_fn(values = values,
                       waiver = waiver(),
                       y.breaks = y.breaks,
                       y.n_breaks = y.n_breaks,
                       y.expand = y.expand,
                       stacked_fill = stacked_fill,
                       y.age = y.age,
                       y.percent = y.percent,
                       y.percent_break = y.percent_break,
                       y.24h = y.24h,
                       y.limits = y.limits,
                       y.transform = y.transform),
    n.breaks = y.n_breaks,
    labels = labels_fn(values = values,
                       waiver = waiver(),
                       y.labels,
                       y.percent = y.percent,
                       y.age = y.age,
                       y.24h = y.24h,
                       y.scientific = y.scientific,
                       stacked_fill = stacked_fill,
                       decimal.mark = decimal.mark,
                       big.mark = big.mark),
    limits = limits_evaluated,
    expand = expand_fn(values = values,
                       y.expand = y.expand,
                       y.age = y.age,
                       stacked_fill = stacked_fill,
                       y.limits = limits_evaluated),
    transform = y.transform,
    position = y.position,
    sec.axis = sec_y
  )
}

#' @importFrom ggplot2 scale_colour_gradient2 scale_colour_gradient scale_colour_gradientn expansion guide_colourbar element_text
#' @importFrom cleaner as.percentage
#' @importFrom scales pretty_breaks
validate_category_scale <- function(values,
                                    type,
                                    cols,
                                    category.labels,
                                    category.percent,
                                    category.breaks,
                                    category.limits,
                                    category.expand,
                                    category.midpoint,
                                    category.transform,
                                    category.date_breaks,
                                    category.date_labels,
                                    stacked_fill,
                                    legend.nbin,
                                    legend.barheight,
                                    legend.barwidth,
                                    legend.reverse,
                                    legend.position,
                                    decimal.mark,
                                    big.mark,
                                    font,
                                    colour_fill,
                                    original_colours,
                                    ...) {
  # only for a numeric and date category scale
  
  if (is.null(category.transform)) {
    category.transform <- "identity"
  }
  if (is.null(legend.position)) {
    legend.position <- "right"
  } else {
    legend.position <- validate_legend.position(legend.position)
  }
  
  labels_fn <- function(values, category.labels, category.percent, category.date_labels, stacked_fill, decimal.mark, big.mark) {
    if (!is.null(category.labels)) {
      category.labels
    } else if (isTRUE(category.percent) || isTRUE(stacked_fill)) {
      function(x, dec = decimal.mark, big = big.mark, ...) format2(as.percentage(x), decimal.mark = dec, big.mark = big)
    } else if (is_date(values)) {
      if (is.null(category.date_labels)) {
        lbls <- determine_date_breaks_labels(values)$labels
        plot2_message("Assuming ", font_blue("category.date_labels = \"", lbls, "\"", collapse = ""))
      } else {
        lbls <- category.date_labels
      }
      function(x, format = lbls, ...) format2(as.Date(as.numeric(x), origin = "1970-01-01"), format = format)
    } else {
      function(x, dec = decimal.mark, big = big.mark, ...) format2(x, decimal.mark = dec, big.mark = big)
    }
  }
  breaks_fn <- function(values, category.breaks, category.percent, category.transform, category.date_breaks, waiver) {
    if (category.transform != "identity") {
      if (!is.null(category.breaks)) {
        plot2_warning("Ignoring ", font_blue("category.breaks"), " since ",
                      font_blue(paste0("category.transform = \"", category.transform, "\"")))
      }
      return(waiver)
    } else if (!is.null(category.breaks)) {
      if (is_date(values) && is.null(category.date_breaks)) {
        plot2_warning("Setting ", font_blue("category.breaks"), " is not useful for dates. Did you mean ", font_blue("category.date_breaks"), "?")
      }
      category.breaks
    } else if (isTRUE(category.percent)) {
      if (max(c(1, values), na.rm = TRUE) == 1) {
        seq(0, 1, 0.25)
      } else {
        # print 5 labels nicely
        pretty_breaks(n = 5)
      }
    } else if (is_date(values)) {
      if (is.null(category.date_breaks)) {
        breaks <- determine_date_breaks_labels(values)$breaks
        plot2_message("Assuming ", font_blue("category.date_breaks = \"", breaks, "\"", collapse = ""))
      } else {
        breaks <- category.date_breaks
      }
      seq.Date(from = min(values, na.rm = TRUE),
               to = max(values, na.rm = TRUE),
               by = breaks)
      # pretty_breaks(n = 5)(values)
    } else if (all(values %% 1 == 0, na.rm = TRUE) && max(values, na.rm = TRUE) < 5) {
      # whole numbers - only strip decimal numbers if total y range is low
      if (diff(range(values, na.rm = TRUE)) < 5 && 0 %in% values[!is.na(values)]) {
        sort(unique(values[!is.na(values)]))
      } else {
        function(x, ...) unique(floor(pretty(seq(0, (max(x, na.rm = TRUE) + 1) * 3))))
      }
    } else {
      # print 5 labels nicely
      pretty_breaks(n = 5)
    }
  }
  limits_fn <- function(values, category.limits, category.percent, category.transform, category.date_breaks, waiver) {
    if (category.transform != "identity") {
      # in certain transformations, such as log, 0 is not allowed
      if (!is.null(category.limits)) {
        plot2_warning("Ignoring ", font_blue("category.limits"), " since ",
                      font_blue(paste0("category.transform = \"", category.transform, "\"")))
      }
      c(NA_real_, NA_real_)
    } else if (!is.null(category.limits)) {
      category.limits
    } else if (isTRUE(category.percent)) {
      function(x, ...) c(min(0, x, na.rm = TRUE), max(1, x, na.rm = TRUE))
    } else if (is_date(values)) {
      # for dates, take the outer range
      c(min(values, na.rm = TRUE) - 1, max(values, na.rm = TRUE) + 1)
    } else {
      # now determine if we should start at zero:
      # x will be the lower and upper limit - if zero under lower minus fifth of upper then start at zero
      upper <- max(values, na.rm = TRUE)
      lower <- min(values, na.rm = TRUE)
      # round upper to significance of lower
      new_upper <- max(upper, round(upper, digits = nchar(lower) * -1))
      # but only if within a 5th of the scale
      if (upper / new_upper >= 0.8) {
        upper <- new_upper
      }
      # and set lower to 0 if need be
      if (lower >= 0 && lower - (upper / 5) < 0) {
        lower <- 0
      }
      # check if negative lower should be equal to upper
      if (lower < 0 && lower / (upper * -1) >= 0.8) {
        lower <- upper * -1
      }
      c(lower, upper)
    }
  }
  
  if (is.numeric(category.expand)) {
    category.expand <- expansion(mult = c(0, category.expand))
  }
  
  if (geom_has_only_colour(type) || identical(cols$colour, cols$colour_fill)) {
    aest <- c("colour", "fill")
    cols_category <- cols$colour
  } else {
    aest <- "fill"
    cols_category <- cols$colour_fill
  }
  
  # general arguments for any scale function below (they are called with do.call())
  args <- list(aesthetics = aest,
               na.value = "white",
               guide = guide_colourbar(ticks = FALSE,
                                       draw.ulim = TRUE,
                                       draw.llim = TRUE,
                                       reverse = isTRUE(legend.reverse),
                                       nbin = legend.nbin,
                                       barheight = ifelse(legend.position %in% c("top", "bottom"),
                                                          legend.barwidth,
                                                          legend.barheight),
                                       barwidth = ifelse(legend.position %in% c("top", "bottom"),
                                                         legend.barheight,
                                                         legend.barwidth)),
               labels = labels_fn(values = values,
                                  category.labels = category.labels,
                                  category.percent = category.percent,
                                  category.date_labels = category.date_labels,
                                  stacked_fill = stacked_fill,
                                  decimal.mark = decimal.mark,
                                  big.mark = big.mark),
               breaks = breaks_fn(values = values,
                                  category.breaks = category.breaks,
                                  category.percent = category.percent,
                                  category.transform = category.transform,
                                  category.date_breaks = category.date_breaks,
                                  waiver = waiver()),
               limits = limits_fn(values = values,
                                  category.limits = category.limits,
                                  category.percent = category.percent,
                                  category.transform = category.transform,
                                  category.date_breaks = category.date_breaks),
               transform = category.transform)
  
  if (isTRUE(original_colours)) {
    # original ggplot2 colours chosen, so just return scale without setting manual colours
    return(do.call(scale_colour_gradient, args = args))
  }
  
  if (length(cols_category) == 1) {
    if (is.na(cols_category) || cols_category %like% "[A-F0-9]{6}00$") {
      # invisible, so don't return a scale in which colours are manually set, just the rest of the options
      do.call(scale_colour_gradient,
              args = args)
    } else {
      # 1 colour, start with white
      if (!identical(colour_fill, "ggplot2") && !is.null(colour_fill)) {
        plot2_message("Adding white to the ", font_blue("category"),
                      " scale - set two colours to ", font_blue("colour_fill"),
                      " to prevent this.")
      }
      do.call(scale_colour_gradient,
              args = c(list(low = "white",
                            high = cols_category),
                       args))
    }
    
  } else if (length(cols_category) == 2) {
    # 2 colours, low and high
    do.call(scale_colour_gradient,
            args = c(list(low = cols_category[1],
                          high = cols_category[2]),
                     args))
    
  } else if (length(cols_category) == 3) {
    # 3 colours, so low, mid (set as vector name) and high
    if (!is.null(category.midpoint)) {
      mid_point <- as.double(category.midpoint)
    } else {
      # default to the middle of the set limits
      if (is.function(args$limits)) {
        rng <- args$limits(values)
      } else {
        rng <- args$limits
      }
      mid_point <- sum(rng) / 2
      plot2_message("Using ", font_blue("category.midpoint =", round(mid_point, 2)),
                    ifelse(isTRUE(category.percent),
                           paste0(" (", format2(as.percentage(mid_point)), ", "),
                           " ("),
                    "the current ", font_blue("category"), " scale centre)")
    }
    do.call(scale_colour_gradient2,
            args = c(list(low = cols_category[1],
                          mid = cols_category[2],
                          high = cols_category[3],
                          midpoint = mid_point),
                     args))
    
  } else {
    # more than 3 colours, create own divergent scale
    # this can also be because one of the viridis colours was set with `colour` and/or `colour_fill`
    do.call(scale_colour_gradientn,
            args = c(list(colours = cols_category),
                     args))
  }
}

#' @importFrom ggplot2 position_stack position_fill position_dodge2 position_jitter
#' @importFrom rlang check_installed
generate_geom <- function(type,
                          df,
                          stacked,
                          stacked_fill,
                          horizontal,
                          width,
                          size,
                          linetype,
                          linewidth,
                          reverse,
                          na.rm,
                          violin_scale,
                          jitter_seed,
                          binwidth,
                          cols,
                          original_colours = original_colours,
                          dots_geom,
                          mapping = NULL) {
  
  if (type == "geom_col") {
    type <- "geom_bar"
  }
  if (type == "geom_beeswarm") {
    check_installed("ggbeeswarm")
    geom_fn <- ggbeeswarm::geom_beeswarm
  } else {
    geom_fn <- getExportedValue(name = type, ns = asNamespace("ggplot2"))
  }
  
  # set position
  if (isTRUE(stacked)) {
    position <- position_stack(reverse = reverse)
  } else if (isTRUE(stacked_fill)) {
    position <- position_fill(reverse = reverse)
  } else {
    # small whitespace between columns:
    position <- position_dodge2(width = width * 1.05, preserve = "single")
  }
  
  set_arguments <- function(..., dots = dots_geom) {
    arguments <- c(...)
    c(arguments[!names(arguments) %in% names(dots)], dots)
  }
  
  # set geoms - do.call() applies all arguments to the geom_fn function
  if (type == "geom_bar") {
    do.call(geom_fn,
            args = set_arguments(list(width = width,
                                      stat = "identity",
                                      position = position,
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(fill = cols$colour_fill)[!has_category(df) & !isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type == "geom_area") {
    do.call(geom_fn,
            args = set_arguments(list(linetype = linetype,
                                      linewidth = linewidth,
                                      stat = "identity",
                                      position = position,
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(fill = cols$colour_fill)[!has_category(df) & !isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type %in% c("geom_line", "geom_path")) {
    do.call(geom_fn,
            args = set_arguments(list(lineend = "round",
                                      linetype = linetype,
                                      linewidth = linewidth,
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type == "geom_point") {
    do.call(geom_fn,
            args = set_arguments(list(size = size,
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type == "geom_jitter") {
    do.call(geom_fn,
            args = set_arguments(list(size = size,
                                      position = position_jitter(seed = jitter_seed),
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type == "geom_beeswarm") {
    do.call(geom_fn,
            args = set_arguments(list(size = size,
                                      #position = ggbeeswarm::position_beeswarm(),
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type == "geom_boxplot") {
    do.call(geom_fn,
            args = set_arguments(list(outlier.size = size * 3,
                                      outlier.alpha = 0.75,
                                      width = width,
                                      linewidth = linewidth, # line width of whole box
                                      fatten = ifelse(linewidth < 1, 1.5, linewidth + 0.5), # factor to make median thicker compared to lwd
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(fill = cols$colour_fill)[!has_category(df) & !isTRUE(original_colours)],
                                 list(fill = "white")[has_category(df) & isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type == "geom_violin") {
    do.call(geom_fn,
            args = set_arguments(list(width = width,
                                      linewidth = linewidth, # line width, of whole violin
                                      scale = violin_scale,
                                      trim = TRUE,
                                      draw_quantiles = c(0.25, 0.5, 0.75),
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(fill = cols$colour_fill)[!has_category(df) & !isTRUE(original_colours)],
                                 list(fill = "white")[has_category(df) & isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type == "geom_histogram") {
    if (is.null(binwidth)) {
      # this will be the default binwidth: the difference in the range, divided by 12 to 22.
      values <- get_x(df)
      values <- values[!is.infinite(values)]
      binwidth <- as.double(diff(range(values, na.rm = TRUE))) / (12 + min(10, length(unique(values)) / 20))
      if (binwidth < 0.01) {
        # do not round
      } else if (binwidth < 1) {
        binwidth <- round(binwidth, 3)
      } else if (binwidth > 10) {
        binwidth <- round(binwidth, 0)
      } else {
        binwidth <- round(binwidth, 1)
      }
      plot2_message("Using ", font_blue("binwidth =", format(binwidth, scientific = FALSE)), " based on data")
    }
    do.call(geom_fn,
            args = set_arguments(list(linetype = linetype,
                                      linewidth = linewidth,
                                      binwidth = binwidth,
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(fill = cols$colour_fill)[!has_category(df) & !isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type == "geom_density") {
    do.call(geom_fn,
            args = set_arguments(list(linetype = linetype,
                                      linewidth = linewidth,
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(fill = cols$colour_fill)[!has_category(df) & !isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
    
  } else if (type == "geom_sf") {
    do.call(geom_fn,
            args = set_arguments(list(linetype = linetype,
                                      linewidth = linewidth,
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[length(cols$colour) == 1 & !isTRUE(original_colours)],
                                 list(fill = cols$colour_fill)[!has_category(df) & !isTRUE(original_colours)]))
    
  } else if (type == "geom_blank") {
    do.call(geom_fn,
            args = set_arguments(list(na.rm = na.rm)))
    
  } else if (type == "geom_tile") {
    do.call(geom_fn,
            args = set_arguments(list(linetype = linetype,
                                      linewidth = linewidth,
                                      na.rm = na.rm)))
    
  } else if (type == "geom_raster") {
    do.call(geom_fn,
            args = set_arguments(list(na.rm = na.rm)))
    
  } else {
    # try to put some arguments into the requested geom
    plot2_warning(font_blue("type = \"", type, "\"", collapse = ""), " is currently only loosely supported")
    do.call(geom_fn,
            args = set_arguments(list(width = width,
                                      size = size,
                                      na.rm = na.rm),
                                 list(colour = cols$colour)[!has_category(df) & !isTRUE(original_colours)],
                                 list(fill = cols$colour_fill)[!has_category(df) & !isTRUE(original_colours)],
                                 list(mapping = mapping)[!is.null(mapping)]))
  }
}

validate_colour <- function(df,
                            type,
                            type_backup,
                            colour,
                            colour_fill,
                            colour_opacity,
                            misses_colour_fill,
                            horizontal) {
  
  if (is.numeric(get_category(df)) || is_date(get_category(df))) {
    colour.bak <- colour
    # this is for validate_category_scale()
    if (length(colour) == 1 && !is.na(colour)) {
      if (colour %in% names(plot2_env$reg_cols) && length(get_registered_colour(colour)) > 1) {
        colour <- get_colour(colour, 4, opacity = colour_opacity)
      } else if (colour %in% viridisLite_colours) {
        # generate viridis colour
        colour <- get_colour(colour, 5, opacity = colour_opacity)
      } else {
        colour <- get_colour(colour, opacity = colour_opacity)
      }
    } else {
      colour <- get_colour(colour, opacity = colour_opacity)
    }
    
    if (is.null(colour_fill) || identical(colour.bak, colour_fill)) {
      colour_fill <- colour
    } else {
      if (length(colour_fill) == 1 && !is.na(colour_fill)) {
        if (colour_fill %in% names(plot2_env$reg_cols) && length(get_registered_colour(colour_fill)) > 1) {
          colour_fill <- get_colour(colour_fill, 4, opacity = colour_opacity)
        } else if (colour_fill %in% viridisLite_colours) {
          # generate viridis colour
          colour_fill <- get_colour(colour_fill, 5, opacity = colour_opacity)
        } else {
          colour_fill <- get_colour(colour_fill, opacity = colour_opacity)
        }
      } else {
        colour_fill <- get_colour(colour_fill, opacity = colour_opacity)
      }
    }
    return(list(colour = colour,
                colour_fill = colour_fill))
  }
  
  if (isTRUE(misses_colour_fill) && is.null(colour_fill) && !geom_is_continuous(type)) {
    colour_fill <- colour
  }
  
  if (!has_category(df)) {
    # has no category
    if (has_x(df) && length(unique(get_x(df))) != length(colour)) {
      # take only the first
      colour <- colour[1]
      colour_fill <- colour_fill[1]
      # take the official ggplot2 colour
      if (identical(colour, "ggplot2")) {
        colour <- "#595959"
      }
      if (identical(colour_fill, "ggplot2")) {
        colour_fill <- "#595959"
      }
    }
    colour <- get_colour(colour, opacity = colour_opacity)
    if (geom_is_continuous(type) && is.null(colour_fill)) {
      # specific treatment for continuous geoms (such as boxplots/violins/histograms/...)
      colour_fill <- add_white(colour, white = 0.75)
    } else {
      colour_fill <- get_colour(colour_fill, opacity = colour_opacity)
    }
    
  } else {
    # has also category, and it's not numeric
    n_unique <- length(unique(get_category(df)))
    colour <- get_colour(colour,
                     length = ifelse(length(colour) == 1, n_unique, 1),
                     opacity = colour_opacity)
    if (geom_is_continuous(type) && is.null(colour_fill) && type_backup != "sankey") {
      # specific treatment for continuous geoms (such as boxplots/violins/histograms/...)
      # but not for Sankey plots - they have sankey.alpha
      colour_fill <- add_white(colour, white = 0.75)
    } else {
      colour_fill <- get_colour(colour_fill,
                            length = ifelse(length(colour_fill) == 1, n_unique, 1),
                            opacity = colour_opacity)
    }
    
    if (isTRUE(horizontal)) {
      colour <- rev(colour)
      colour_fill <- rev(colour_fill)
    }
    
    if (length(colour) > 1 && length(colour_fill) == 1) {
      colour_fill <- colour
    }
    
    # expand the range
    df_nonempty <- df |> 
      filter(!is.na(`_var_category`) & !is.na(`_var_y`))
    if (has_x(df)) {
      df_nonempty <- df_nonempty |> 
        filter(!is.na(`_var_x`))
    }
    if (has_facet(df)) {
      df_nonempty <- df_nonempty |> 
        filter(!is.na(`_var_facet`))
    }
    # TODO this very hacky... since ggplot2 3.4.0 manual values in scale_*_manual work differently
    grp_sizes <- group_sizes(df_nonempty)
    grp_sizes <- grp_sizes[grp_sizes != 0]
    n_categories <- length(grp_sizes)
    if (any(grp_sizes > 1, na.rm = TRUE) && n_categories * n_distinct(get_category(df)) < nrow(df)) {
      if (length(colour) < n_categories) {
        # expand colour for all categories, except when all colours were named
        colour <- c(colour, rep(colour, n_categories)[seq_len(n_categories - length(colour))])
      }
      if (length(colour_fill) < n_categories) {
        # expand colour_fill for all categories, except when all colours were named
        colour_fill <- c(colour_fill, rep(colour_fill, n_categories)[seq_len(n_categories - length(colour_fill))])
        # remove empty groups
        colour_fill <- colour_fill[grp_sizes != 0]
      }
    }
  }
  
  if (type == "geom_sf" && !has_category(df)) {
    colour_fill <- colour_fill[1]
  }
  
  list(colour = get_colour(colour),
       colour_fill = get_colour(colour_fill))
}

validate_size <- function(size, type, type_backup) {
  if (is.null(size)) {
    if (type_backup == "dumbbell") {
      size <- 5
    } else if (type %in% c("geom_point", "geom_jitter")) {
      size <- 2
    } else {
      size <- 0.75
    }
  }
  size
}

validate_width <- function(width, type) {
  if (is.null(width)) {
    if (type %in% c("geom_boxplot", "geom_violin", "geom_jitter")) {
      width <- 0.75
    } else {
      width <- 0.5
    }
  }
  width
}

validate_linewidth <- function(linewidth, type, type_backup) {
  if (is.null(linewidth)) {
    if (type == "geom_sf") {
      linewidth <- 0.1
    } else if (type %in% c("geom_boxplot", "geom_violin")) {
      linewidth <- 0.5
    } else if (type_backup == "dumbbell") {
      linewidth <- 1
    } else if (geom_is_continuous(type) && !geom_has_only_colour(type)) {
      linewidth <- 0.25
    } else {
      linewidth <- 0.5
    }
  }
  linewidth
}

validate_markdown <- function(markdown,
                              x.title,
                              y.title,
                              legend.title,
                              title,
                              subtitle,
                              tag,
                              caption,
                              df = NULL) {
  if (!is.null(markdown)) {
    return(isTRUE(markdown))
  }
  if (!is.null(df)) {
    df_titles <- c(get_x_name(df),
                   get_y_name(df),
                   get_category_name(df),
                   get_facet_name(df),
                   get_datalabels(df))
  } else {
    df_titles <- NULL
  }
  txt <- paste(c(tryCatch(as.character(x.title), error = function(e) ""),
                 tryCatch(as.character(y.title), error = function(e) ""),
                 tryCatch(as.character(legend.title), error = function(e) ""),
                 tryCatch(as.character(title), error = function(e) ""),
                 tryCatch(as.character(subtitle), error = function(e) ""),
                 tryCatch(as.character(tag), error = function(e) ""),
                 tryCatch(as.character(caption),  error = function(e) ""),
                 tryCatch(as.character(df_titles),  error = function(e) "")),
               collapse = "")
  out <- txt %like% "(\\^|[_*].+[_*])"
  if (isTRUE(out)) {
    plot2_message("Assuming ", font_blue("markdown = TRUE"))
  }
  out
}

#' @importFrom dplyr mutate pull first
validate_title <- function(x, markdown, df = NULL, max_length = NULL) {
  if (isTRUE(try(is_empty(x), silent = TRUE))) {
    x <- NULL
  }
  suppressWarnings(
    if (isTRUE(try(is.expression(x), silent = TRUE)) ||
        isTRUE(try(is.null(x), silent = TRUE)) ||
        isTRUE(try(isTRUE(x), silent = TRUE))) {
      return(x)
    }
  )
  
  # support for calculations, e.g. `title = paste("Total number =", n(), "rows")`
  if (!is.null(df)) {
    out <- tryCatch(
      suppressWarnings(
        df |> 
          # no tibbles, data.tables, sf, etc. objects:
          as.data.frame(stringsAsFactors = FALSE) |>
          mutate(`_new_title` = {{ x }}) |> 
          pull(`_new_title`) |> 
          unique() |> 
          first()
      ), error = function(e) {
        plot2_warning(format_error(e,
                                   replace = c("`_new_title = ", "`_new_title`"),
                                   by = c("`", "A title")))
        NULL
      })
    if (is.null(out)) {
      out <- ""
    }
  } else {
    out <- paste0(as.character(x), collapse = "", sep = "")
  }
  
  out <- gsub("<br>", "\n", out, fixed = TRUE)
  out_plain <- gsub("[^a-zA-Z0-9,. .-]", "", out)
  
  if (isTRUE(markdown)) {
    # support mathematical characters
    out <- gsub("!=", "\u2260", out, fixed = TRUE)
    out <- gsub("<=", "\u2264", out, fixed = TRUE)
    out <- gsub(">=", "\u2265", out, fixed = TRUE)
  }
  
  # support for markdown
  if (isTRUE(markdown) &&
      (isTRUE(out %like% "[*]+.+[*]+")
       || isTRUE(out %like% "[a-zA-Z0-9,.-]_[{].+[}]")
       || isTRUE(out %like% "[a-zA-Z0-9,.-] ?\\^ ?[{].+[}]")
       || isTRUE(out %like% "[a-zA-Z0-9,.-] ?\\^ ?[a-zA-Z0-9,._-]")
       || isTRUE(out %like% "<sup>.+</sup>")
       || isTRUE(out %like% "<sub>.+</sub>")
       || isTRUE(out %like% "[$]"))) {
    out <- md_to_expression(out)
  }
  
  # support overly lengthy titles
  if (!is.null(max_length) && isTRUE(nchar(out_plain) > as.double(max_length))) {
    if (is.expression(out)) {
      x_deparsed <- trimws(deparse(substitute(x)))
      x_deparsed <- x_deparsed[!x_deparsed %in% c("{", "}")]
      plot2_warning("Multiple lines in ", font_blue(x_deparsed), 
                    " cannot be set since it is an expression (does it contain markdown characters?)")
    } else {
      out <- gsub("<br>", "\n", out, fixed = TRUE)
      out <- paste(strwrap(x = out, width = max_length),
                   collapse = "\n")
    }
  }
  
  out
}

#' @importFrom ggplot2 theme_grey element_blank margin rel
validate_theme <- function(theme,
                           type,
                           background,
                           text_factor,
                           font,
                           horizontal,
                           x.remove,
                           y.remove,
                           x.lbl_angle,
                           x.lbl_align,
                           x.lbl_italic,
                           facet.fill,
                           facet.bold,
                           facet.italic,
                           facet.size,
                           facet.margin,
                           legend.italic,
                           title.colour,
                           subtitle.colour,
                           has_y_secondary,
                           has_category,
                           col_y_primary,
                           col_y_secondary,
                           sankey.remove_axes) {
  
  if (!is.null(theme)) {
    if (is.character(theme)) {
      theme.bak <- theme
      if (theme == "ggplot2") {
        theme <- "ggplot2::theme_grey()"
      }
      # for `theme = "theme_bw"` and `theme = "theme_bw()"`
      theme <- tryCatch(eval(parse(text = theme)), error = function(e) NULL)
      if (is.null(theme)) {
        # try again with prefix `ggplot2::`
        theme <- tryCatch(eval(parse(text = paste0("ggplot2::", theme.bak))), error = function(e) NULL)
      }
      if (is.null(theme)) {
        stop("unknown theme: ", theme.bak, call. = FALSE)
      }
    }
    if (is.function(theme)) {
      # for `theme = theme_bw`
      theme <- theme()
    }
    if (!inherits(theme, "theme")) {
      plot2_warning("No valid ggplot2 theme, using ", font_blue("theme = ggplot2::theme_grey()"))
      theme <- NULL
    }
  }
  
  orginally_empty <- is_empty(theme)
  if (isTRUE(orginally_empty)) {
    # turn to default ggplot2 theme, so we can at least
    # add all theme options set as arguments, like x.lbl_angle
    theme <- theme_grey()
  }
  
  # set other properties to theme, that are set in plot2(...)
  if (!isTRUE(orginally_empty) && !is.null(background)) {
    theme$panel.background <- element_rect(fill = get_colour(background),
                                           colour = theme$panel.background$colour,
                                           linewidth = theme$panel.background$linewidth,
                                           linetype = theme$panel.background$linetype)
    theme$plot.background <- element_rect(fill = get_colour(background),
                                          colour = theme$plot.background$colour,
                                          linewidth = theme$plot.background$linewidth,
                                          linetype = theme$plot.background$linetype)
  }
  if (isTRUE(horizontal)) {
    if (isTRUE(x.lbl_italic)) {
      theme$axis.text.y$face <- "italic"
    }
    if (isTRUE(x.remove)) {
      theme$axis.text.y <- element_blank()
    }
    if (isTRUE(y.remove)) {
      theme$axis.text.x <- element_blank()
    }
  } else {
    if (isTRUE(x.lbl_italic)) {
      theme$axis.text.x$face <- "italic"
    }
    if (isTRUE(x.remove)) {
      theme$axis.text.x <- element_blank()
    }
    if (isTRUE(y.remove)) {
      theme$axis.text.y <- element_blank()
    }
  }
  
  if (isTRUE(has_y_secondary) && !isTRUE(has_category)) {
    # set colour of geoms to title texts
    theme$axis.title.y$colour <- col_y_primary
    theme$axis.title.y$face <- "bold"
    theme$axis.title.y.right$colour <- col_y_secondary
    theme$axis.title.y.right$face <- "bold"
  }
  
  theme$axis.text.x$angle <- x.lbl_angle
  if (is.null(x.lbl_align) && x.lbl_angle != 0) {
    # determine the better alignment
    if (abs(x.lbl_angle) %in% c(0:10, 171:190, 351:360)) {
      x.lbl_align <- 0.5 # centre
    }
    if (abs(x.lbl_angle) %in% 191:350) {
      x.lbl_align <- 0 # left
    }
    if (abs(x.lbl_angle) %in% 11:170) {
      x.lbl_align <- 1 # right
    }
    if (x.lbl_angle < 0) {
      x.lbl_align <- 1 - x.lbl_align
    }
  }
  if (!is.null(x.lbl_align)) {
    theme$axis.text.x$hjust <- x.lbl_align
  }
  
  if (isTRUE(legend.italic)) {
    theme$legend.text$face <- "italic"
  }
  
  if (!is.null(title.colour)) {
    theme$plot.title$colour <- get_colour(title.colour)
  }
  if (!is.null(subtitle.colour)) {
    theme$plot.subtitle$colour <- get_colour(subtitle.colour)
  }
  # facet
  theme$strip.background$fill <- facet.fill
  if (isTRUE(facet.bold) && isTRUE(facet.italic)) {
    theme$strip.text$face <- "bold.italic"
  } else if (isTRUE(facet.bold)) {
    theme$strip.text$face <- "bold"
  } else if (isTRUE(facet.italic)) {
    theme$strip.text$face <- "italic"
  } else {
    theme$strip.text$face <- "plain"
  }
  theme$strip.text$margin <- margin(t = facet.margin, b = facet.margin / 2)
  theme$strip.text$size <- unit(facet.size, "pt")
  
  # set the font family and font size, taking text_factor into account
  attr_bak <- attributes(theme)
  base_size <- theme$text$size
  theme <- lapply(theme, function(el) {
    if (inherits(el, "element_text")) {
      el$family <- font
      if (text_factor != 1 && !is.null(el$size) && is.numeric(el$size)) {
        if (inherits(el$size, "rel")) {
          # in theme_minimal2, these are the x and y axis labels, not their titles
          # in thme_bw, a lot of element have class 'rel'
          el$size <- base_size * text_factor * as.double(el$size)
        } else {
          el$size <- base_size * text_factor * (as.double(el$size) / base_size)
        }
      }
    }
    el
  })
  attributes(theme) <- attr_bak # restore class and all other attributes
  
  # special case for tile-like types, remove axis line and add raster
  if (type %in% c("geom_tile", "geom_raster", "geom_rect")) {
    theme$axis.line.x <- theme$axis.line.y
    theme$panel.grid.major.x <- theme$panel.grid.major.y
    theme$panel.grid.minor.x <- theme$panel.grid.minor.y
  }
  
  # if horizontal, all x and y grid lines etc. should be exchanged
  if (isTRUE(horizontal)) {
    theme.bak <- theme
    theme$panel.grid.major.x <- theme$panel.grid.major.y
    theme$panel.grid.major.y <- theme.bak$panel.grid.major.x
    theme$panel.grid.minor.x <- theme$panel.grid.minor.y
    theme$panel.grid.minor.y <- theme.bak$panel.grid.minor.x
    theme$axis.ticks.x <- theme$axis.ticks.y
    theme$axis.ticks.y <- theme.bak$axis.ticks.x
    theme$axis.line.x <- theme$axis.line.y
    theme$axis.line.y <- theme.bak$axis.line.x
  }
  
  # for Sankey plots, remove axes if indicated
  if (isTRUE(sankey.remove_axes)) {
    theme$axis.line <- element_blank()
    theme$panel.grid.major <- element_blank()
    theme$panel.grid.minor <- element_blank()
    theme$axis.ticks <- element_blank()
    if (isTRUE(horizontal)) {
      theme$axis.text.x <- element_blank()
    } else {
      theme$axis.text.y <- element_blank()
    }
    theme$axis.title <- element_blank()
  }
  
  # return the theme
  return(theme)
}

#' @importFrom ggplot2 facet_grid facet_wrap
validate_facet <- function(df,
                           type,
                           facet.repeat_lbls_x,
                           facet.repeat_lbls_y,
                           facet.fixed_y,
                           facet.relative,
                           facet.drop,
                           facet.nrow,
                           facet.position,
                           horizontal) {
  
  if (is.null(facet.repeat_lbls_y)) {
    if (isTRUE(facet.fixed_y)) {
      plot2_message("Assuming ", font_blue("facet.repeat_lbls_y = FALSE"),
                    " since ", font_blue("y"), " has fixed scales")
      facet.repeat_lbls_y <- FALSE
    } else {
      facet.repeat_lbls_y <- TRUE
    }
  }
  
  scales <- "fixed"
  if (isTRUE(facet.repeat_lbls_x) && isTRUE(facet.repeat_lbls_y)) {
    scales <- "free"
  } else if (isTRUE(facet.repeat_lbls_y)) {
    scales <- "free_y"
    if (isTRUE(horizontal)) {
      scales <- "free_x"
    }
  } else if (isTRUE(facet.repeat_lbls_x)) {
    scales <- "free_x"
    if (isTRUE(horizontal)) {
      scales <- "free_y"
    }
  }
  
  if (type == "geom_sf") {
    # force fixes scales, otherwise throws an error: coord_sf doesn't support free scales
    scales <- "fixed"
  }
  
  if (any(is.na(get_facet(df)))) {
    # 'drop' means dropping of factor levels. If this is FALSE and the columns contains NA, this throws an error:
    # Error in scale_apply(layer_data, x_vars, "train", SCALE_X, x_scales)
    facet.drop <- TRUE
  }
  if (isTRUE(facet.relative)) {
    if (facet.position == "top") {
      switch <- "y"  
    } else {
      switch <- "x"
    }
    if (is.null(facet.nrow) || facet.nrow == 1) {
      return(facet_grid(cols = vars(`_var_facet`),
                        space = scales, # <- this makes the facet.relative happen
                        drop = facet.drop,
                        scales = scales,
                        switch = switch))
    } else {
      plot2_warning("When using ", font_blue("facet.relative = TRUE"), ", the number of columns cannot be > 1 when ",
                    font_blue("facet.nrow"), " is larger than 1")
      return(facet_grid(rows = vars(`_var_facet`),
                        space = scales,
                        drop = facet.drop,
                        scales = scales,
                        switch = switch))
    }
  } else {
    facet.position <- trimws(tolower(facet.position[1L]))
    facet.position <- gsub("^t$", "top", facet.position)
    facet.position <- gsub("^r$", "right", facet.position)
    facet.position <- gsub("^b$", "bottom", facet.position)
    facet.position <- gsub("^l$", "left", facet.position)
    return(facet_wrap("`_var_facet`",
                      scales = scales,
                      strip.position = facet.position,
                      drop = facet.drop,
                      nrow = facet.nrow))
  }
}

#' @importFrom ggplot2 geom_text geom_label geom_sf_label geom_sf_text aes position_fill position_stack position_dodge2
set_datalabels <- function(p,
                           df,
                           type,
                           width,
                           stacked,
                           stacked_fill,
                           datalabels.colour_fill,
                           datalabels.colour,
                           datalabels.size,
                           datalabels.angle,
                           datalabels.lineheight,
                           datalabels.centroid,
                           font,
                           reverse,
                           horizontal,
                           misses_datalabels,
                           markdown) {
  
  if (isTRUE(misses_datalabels) && nrow(df) > 25) {
    plot2_caution("Omitting printing of ", nrow(df), " datalabels - use ",
                  font_blue("datalabels = TRUE"), " to force printing")
    return(p)
  }
  
  is_sf <- (type == "geom_sf")
  is_tile <- (type %in% c("geom_tile", "geom_raster", "geom_rect"))
  
  if (is.null(datalabels.colour_fill)) {
    if (isTRUE(is_tile)) {
      datalabels.colour_fill <- NA
    } else {
      # try to get from current theme
      datalabels.colour_fill <- p$theme$panel.background$fill
      if (is.null(datalabels.colour_fill)) {
        # still NULL, then make fill invisible (NA)
        datalabels.colour_fill <- NA
      }
    }
  }
  
  if (!isTRUE(stacked) && !isTRUE(stacked_fill) && !isTRUE(is_sf) && !isTRUE(is_tile)) {
    datalabels.colour_fill <- get_colour(datalabels.colour_fill, opacity = 0.4) # 40% transparency
  } else {
    datalabels.colour_fill <- get_colour(datalabels.colour_fill, opacity = 0.75) # 75% transparency
  }
  datalabels.colour <- get_colour(datalabels.colour)
  
  # set label and text offsets (does not apply to sf and tile plots)
  text_horizontal <- 0.5
  text_vertical <- -0.75
  label_horizontal <- 0.5
  label_vertical <- -0.1
  if (isTRUE(horizontal) || datalabels.angle == 90) {
    text_horizontal <- -0.25
    text_vertical <- 0.5
    label_horizontal <- -0.1
    label_vertical <- 0.5
  }
  
  # set positioning function
  if (isTRUE(stacked_fill)) {
    position_fn <- position_fill(reverse = reverse, vjust = 0.5)
  } else if (isTRUE(stacked)) {
    position_fn <- position_stack(reverse = reverse, vjust = 0.5)
  } else {
    position_fn <- position_dodge2(width = width, preserve = "single")
  }
  
  original_values <- p$data$`_var_datalabels`
  
  if (!isTRUE(is_sf)) {
    geom_label_fn <- geom_label
    geom_text_fn <- geom_text
    geometry_fix_fn <- NULL
  } else {
    geom_label_fn <- geom_sf_label
    geom_text_fn <- geom_sf_text
    # these functions from the 'sf' package fix invalid geometries
    st_is_valid <- getExportedValue(name = "st_is_valid", ns = asNamespace("sf"))
    st_point <- getExportedValue(name = "st_point", ns = asNamespace("sf"))
    st_centroid <- getExportedValue(name = "st_centroid", ns = asNamespace("sf"))
    st_point_on_surface <- getExportedValue(name = "st_point_on_surface", ns = asNamespace("sf"))
    st_zm <- getExportedValue(name = "st_zm", ns = asNamespace("sf"))
    if (is.null(datalabels.centroid)) {
      plot2_message("Assuming ", font_blue("datalabels.centroid = TRUE"), ". Set to FALSE for a point-on-surface placing of datalabels.")
      datalabels.centroid <- TRUE
    }
    if (isTRUE(datalabels.centroid)) {
      geometry_fix_fn <- function(x) {
        x[!st_is_valid(x)] <- st_point()
        suppressWarnings(st_centroid(st_zm(x)))
      }
    } else {
      geometry_fix_fn <- function(x) {
        x[!st_is_valid(x)] <- st_point()
        suppressWarnings(st_point_on_surface(st_zm(x)))
      }  
    }
  }
  
  # generate the datalabels
  p <- p +
    # set background label
    do.call(geom_label_fn,
            args = c(list(mapping = aes(label = ifelse(is.na(original_values),
                                                       NA_character_,
                                                       paste0(original_values,
                                                              strrep("-", ceiling(nchar(original_values) * 0.33))))),
                          colour = NA,
                          fill = datalabels.colour_fill,
                          size = datalabels.size,
                          family = font,
                          angle = datalabels.angle,
                          lineheight = datalabels.lineheight,
                          na.rm = TRUE),
                     # only when there's a category:
                     list(position = position_fn)[has_category(df) & !isTRUE(is_sf)],
                     # only when not stacked at all:
                     list(label.padding = unit(0.25, "lines"))[!isTRUE(stacked) & !isTRUE(stacked_fill) & !isTRUE(is_sf) & !isTRUE(is_tile)],
                     list(label.r = unit(0, "lines"))[!isTRUE(stacked) & !isTRUE(stacked_fill) & !isTRUE(is_sf) & !isTRUE(is_tile)],
                     list(vjust = label_vertical)[!isTRUE(stacked) & !isTRUE(stacked_fill) & !isTRUE(is_sf) & !isTRUE(is_tile)],
                     list(hjust = label_horizontal)[!isTRUE(stacked) & !isTRUE(stacked_fill) & !isTRUE(is_sf) & !isTRUE(is_tile)],
                     # only when stacked_fill:
                     list(vjust = 0.5)[isTRUE(stacked_fill) || isTRUE(is_sf) || isTRUE(is_tile)],
                     list(hjust = 0.5)[isTRUE(stacked_fill) || isTRUE(is_sf) || isTRUE(is_tile)],
                     # only when sf:
                     list(fun.geometry = geometry_fix_fn)[isTRUE(is_sf)])) +
    # set text
    do.call(geom_text_fn,
            args = c(list(mapping = aes(label = `_var_datalabels`),
                          colour = datalabels.colour,
                          size = datalabels.size * ifelse(isTRUE(markdown) & isTRUE(is_sf), 1.05, 1),
                          family = font,
                          angle = datalabels.angle,
                          lineheight = datalabels.lineheight,
                          na.rm = TRUE),
                     # only when there's a category:
                     list(position = position_fn)[has_category(df) & !isTRUE(is_sf) & !isTRUE(is_tile)],
                     # only when not stacked at all:
                     list(vjust = text_vertical)[!isTRUE(stacked) & !isTRUE(stacked_fill) & !isTRUE(is_sf) & !isTRUE(is_tile)],
                     list(hjust = text_horizontal)[!isTRUE(stacked) & !isTRUE(stacked_fill) & !isTRUE(is_sf) & !isTRUE(is_tile)],
                     # only when stacked_fill:
                     list(vjust = 0.5)[isTRUE(stacked_fill) || isTRUE(is_sf) || isTRUE(is_tile)],
                     list(hjust = 0.5)[isTRUE(stacked_fill) || isTRUE(is_sf) || isTRUE(is_tile)],
                     # only when sf:
                     list(fun.geometry = geometry_fix_fn)[isTRUE(is_sf)]))
  
  if (!isTRUE(stacked) && !isTRUE(stacked_fill) && !isTRUE(is_sf) && !isTRUE(is_tile)) {
    # move label layer to back + 1;
    # this will make the labels only interfere with plot lines,
    # not with the data (such as columns)
    layer_n <- seq_len(length(p$layers))
    layer_label <- length(layer_n) - 1
    layer_others <- layer_n[-layer_label]
    p$layers <- p$layers[c(layer_label, layer_others)]
  }
  
  p
}

validate_font <- function(font) {
  if (is_empty(font)) {
    # no font set, so return empty string to use default
    return("")
  }
  required_pkg <- c("showtext", "showtextdb", "sysfonts")
  misses_pkg <- !required_pkg %in% rownames(utils::installed.packages())
  if (any(misses_pkg)) {
    plot2_warning("Package ", paste0("'", required_pkg[misses_pkg], "'", collapse = " and "),
                  " not installed, ignoring ", font_blue("font = \"", font, "\"", collapse = ""))
    return("")
  }
  
  if (isTRUE(getOption("knitr.in.progress"))) {
    # we are in R Markdown - chunk option `fig.showtext` must be set to TRUE
    fig.showtext <- knitr::opts_current$get()$`fig.showtext`
    if (!isTRUE(fig.showtext)) {
      stop(paste0("Using custom fonts with `plot2()` requires `fig.showtext` in R Markdown or `fig-showtext` in Quarto, e.g.:\n",
                  "\n",
                  font_bold("For R Markdown\n"),
                  font_italic("Chunk-specific:\n"),
                  font_blue("```{r, fig.showtext = TRUE}\n"),
                  font_blue(paste0("your_data |> plot2(..., font = \"", font, "\")\n")),
                  font_blue("```\n"),
                  font_italic("Global; in top chunk:\n"),
                  font_blue("```{r setup, include = FALSE}\n"),
                  font_blue("knitr::opts_chunk$set(fig.showtext = TRUE)\n"),
                  font_blue("```\n"),
                  "\n",
                  font_bold("For Quarto\n"),
                  font_italic("Chunk-specific:\n"),
                  font_blue("```{r}\n"),
                  font_blue("#| fig-showtext: true\n"),
                  font_blue(paste0("your_data |> plot2(..., font = \"", font, "\")\n")),
                  font_blue("```\n"),
                  font_italic("Global; in YAML header or in `_quarto.yaml`:\n"),
                  font_blue("knitr:\n"),
                  font_blue("  opts_chunk:\n"),
                  font_blue("    fig.showtext = TRUE\n"),
                  "\n",
                  "For more information, see ", font_url("https://yihui.org/knitr/options/#plots", "this knitr manual"), "."),
           call. = FALSE)
    }
  } else {
    # enable showtext
    showtext::showtext_auto(enable = TRUE)
  }
  
  font.bak <- font
  font <- trimws(tolower(font)[1L])
  if (font %in% tolower(sysfonts::font_families())) {
    # this is for previously activated fonts, or fonts installed from Google Fonts
    return(sysfonts::font_families()[tolower(sysfonts::font_families()) == font])
  }
  
  # get font files from system
  if (is.null(plot2_env$fonts)) {
    # so it only runs the first time
    plot2_env$fonts <- sysfonts::font_files()
  }
  fonts <- plot2_env$fonts[which(tolower(plot2_env$fonts$family) == trimws(tolower(font)[1L])), , drop = FALSE]
  
  if (NROW(fonts) == 0) {
    # font does not exist yet - try to download from Google Fonts
    tryCatch({
      font_urls <- showtextdb::google_fonts(font.bak)
      # install and register using showtextdb
      suppressMessages(showtextdb::font_install(font_urls, quiet = TRUE))
      plot2_message("Downloaded font ", font_blue(font_url(paste0("https://fonts.google.com/specimen/", gsub(" ", "+", font.bak)), font.bak)), " from Google Fonts")
      showtextdb::load_showtext_fonts()
    }, error = function(e) invisible())
    
  } else if (!fonts$family[1L] %in% sysfonts::font_families()) {
    # helper function for adding fonts
    set_if_not_null <- function(type) {
      fonts$fullpath <- paste(fonts$path, fonts$file, sep = "/")
      fonts$plainface <- gsub(" +", "", trimws(tolower(fonts$face)))
      font <- fonts[which(fonts$plainface == type), "fullpath", drop = TRUE]
      if (length(font) == 0) {
        NULL
      } else {
        font
      }
    }
    # still has to be 'registered' with sysfonts, so do it
    sysfonts::font_add(family = fonts$family[1L],
                       regular = set_if_not_null("regular"),
                       bold = set_if_not_null("bold"),
                       italic = set_if_not_null("italic"),
                       bolditalic = set_if_not_null("bolditalic"))
  }
  
  # return the font if it is available
  if (font %in% tolower(sysfonts::font_families())) {
    return(sysfonts::font_families()[tolower(sysfonts::font_families()) == font])
  } else {
    plot2_warning("Ignoring unknown font family \"", font.bak, "\"")
    return("")
  }
}

#' @importFrom tidyr complete full_seq
complete_direction <- function(df, direction, has_direction, filler) {
  if (has_direction && !is.null(filler) && !isFALSE(filler)) {
    direction_data <- df[[paste0("_var_", direction)]]
    if (isTRUE(filler)) {
      filler <- 0
    }
    fill_list <- list(`_var_y` = filler,
                      y_name = filler)
    names(fill_list)[2] <- get_y_name(df)
    if (!inherits(direction_data, c("Date", "POSIXt")) && is.double(direction_data)) {
      # determine significance
      sig <- max(sigfigs(diff(direction_data)), na.rm = TRUE)
      period <- 1 / 10 ^ sig
    } else {
      period <- 1
    }
    if (direction == "x") {
      df <- df |> 
        complete(`_var_x` = full_seq(direction_data, period = period), fill = fill_list)
      df[, get_x_name(df)] <- df$`_var_x`  
    } else if (direction == "category") {
      df <- df |> 
        complete(`_var_category` = full_seq(direction_data, period = period), fill = fill_list)
      df[, get_category_name(df)] <- df$`_var_category`  
    } else if (direction == "facet") {
      df <- df |> 
        complete(`_var_facet` = full_seq(direction_data, period = period), fill = fill_list)
      df[, get_facet_name(df)] <- df$`_var_facet`  
    }
  }
  df
}

validate_sorting <- function(sort_method, horizontal) {
  if (is.null(sort_method)) {
    return(sort_method)
  }
  if (length(sort_method) > 1) {
    # is a vector of values
    return("manual")
  }
  sort_method <- tolower(sort_method[1L])
  sort_method <- gsub("[^a-z-]+", "", sort_method)
  sort_method <- gsub("true", "asc", sort_method)      # when sort_method = TRUE
  sort_method <- gsub("false", "inorder", sort_method) # when sort_method = FALSE
  sort_method <- gsub("^order", "inorder", sort_method)
  sort_method <- gsub("asc[a-z]+", "asc", sort_method)
  sort_method <- gsub("desc[a-z]+", "desc", sort_method)
  if (sort_method %like% "freq$") {
    sort_method <- paste0(sort_method, "-desc")
  }
  if (isTRUE(horizontal)) {
    # reverse asc and desc
    sort_method <- gsub("asc", "asc2", sort_method)
    sort_method <- gsub("desc", "asc", sort_method)
    sort_method <- gsub("asc2", "desc", sort_method)
  }
  sort_method
}

#' @importFrom forcats fct_inorder fct_reorder
#' @importFrom stringr str_sort
sort_data <- function(values,
                      original_values, # required for sort = FALSE, should be according to original values
                      sort_method,
                      datapoints,
                      summarise_function,
                      summarise_fn_name,
                      horizontal,
                      drop,
                      argument) {
  if (is.null(sort_method) ||
      is.numeric(values) ||
      is_date(values) ||
      ((isTRUE(sort_method) && is.factor(values) && !isTRUE(horizontal)))) {
    # don't sort at all
    return(values)
  }
  
  # set up sort_method
  sort_method.bak <- sort_method
  sort_method <- validate_sorting(sort_method = sort_method, horizontal = horizontal)
  if (sort_method != "manual") {
    # 'manual' is because of a manually set vector of values
    sort_method.bak <- sort_method.bak[1L]
  }
  
  # manually set values
  if (sort_method == "manual") {
    values <- as.character(values)
    sort_method.bak <- as.character(sort_method.bak)
    lvls <- union(sort_method.bak, values[!values %in% sort_method.bak])
    if (isTRUE(horizontal)) {
      lvls <- rev(lvls)
    }
    return(factor(values, levels = lvls, ordered = TRUE))
  }
  
  # factors get a special treatment - they are sorted on their levels
  if (is.factor(values)) {
    if (sort_method %in% c("alpha", "alpha-asc", "asc")) {
      if (isTRUE(horizontal)) {
        lvls <- rev(levels(values))
      } else {
        lvls <- levels(values)
      }
      return(factor(as.character(values),
                    levels = lvls,
                    ordered = is.ordered(values)))
    } else if (sort_method %in% c("alpha-desc", "desc")) {
      if (isTRUE(horizontal)) {
        lvls <- levels(values)
      } else {
        lvls <- rev(levels(values))
      }
      return(factor(as.character(values),
                    levels = lvls,
                    ordered = is.ordered(values)))
    }
  }
  if (!isTRUE(drop)) {
    levels <- levels(values)
  }
  
  # force characters
  values <- as.character(values)
  
  # start the sorting
  numeric_sort <- any(values %like% "[0-9]", na.rm = TRUE)
  if (sort_method %in% c("alpha", "alpha-asc", "asc")) {
    # alphabetical, or ascending
    out <- factor(values,
                  levels = str_sort(unique(values),
                                    numeric = numeric_sort))
  } else if (sort_method %in% c("alpha-desc", "desc")) {
    out <- factor(values,
                  levels = str_sort(unique(values),
                                    numeric = numeric_sort,
                                    decreasing = TRUE))
  } else if (sort_method == "inorder") {
    out <- factor(as.character(values),
                  levels = levels(fct_inorder(as.character(original_values))))
  } else if (sort_method %in% c("freq-asc", "infreq-asc")) {
    if (n_distinct(values) < length(values)) {
      plot2_message("Applying ", font_blue(paste0(argument, " = \"", sort_method, "\"")), " using ",
                    font_blue(paste0("summarise_function = ", summarise_fn_name)))
    }
    out <- fct_reorder(.f = as.character(values),
                       .x = datapoints,
                       .fun = summarise_function,
                       .desc = FALSE)
  } else if (sort_method %in% c("freq-desc", "infreq-desc")) {
    if (n_distinct(values) < length(values)) {
      plot2_message("Applying ", font_blue(paste0(argument, " = \"", sort_method, "\"")), " using ",
                    font_blue(paste0("summarise_function = ", summarise_fn_name)))
    }
    out <- fct_reorder(.f = as.character(values),
                       .x = datapoints,
                       .fun = summarise_function,
                       .desc = TRUE)
  } else {
    stop("invalid sorting option: '", sort_method.bak, "'", call. = FALSE)
  }
  
  if (!isTRUE(drop) && !is.null(levels)) {
    levels(out) <- c(levels(out), sort(levels[!levels %in% levels(out)]))
  }
  
  out
}

#' @importFrom forcats fct_relevel
#' @importFrom dplyr group_by across group_size mutate summarise
set_max_items <- function(df,
                          y,
                          x,
                          x.max_items,
                          x.max_txt,
                          category,
                          category.max_items,
                          category.max_txt,
                          facet,
                          facet.max_items,
                          facet.max_txt,
                          horizontal,
                          summarise_function,
                          decimal.mark,
                          big.mark,
                          datalabels.round,
                          datalabels.format,
                          y.percent) {
  if (is.infinite(x.max_items) && is.infinite(category.max_items) && is.infinite(facet.max_items)) {
    return(df)
  }
  if (is.null(x) && is.null(category) && is.null(facet)) {
    return(df)
  }
  
  # helper function
  set_max <- function(values, n_max, txt, horizontal) {
    if (is.null(n_max) || is.infinite(n_max)) {
      return(values)
    }
    if (!is.factor(values)) {
      plot2_warning("Setting ", font_blue("*.max_items"), " only works when values are a character or (sorted) factor, not ", font_magenta(paste0(class(values), collapse = "/")))
      return(values)
    }
    if (n_max < length(levels(values))) {
      if (isTRUE(horizontal)) {
        lvls_remove <- rev(levels(values))[c(n_max:length(levels(values)))]
      } else {
        lvls_remove <- levels(values)[c(n_max:length(levels(values)))]
      }
      lvls_remove <- lvls_remove[order(-nchar(lvls_remove))]
      value_new <- gsub("%n", format2(length(lvls_remove)), txt, fixed = TRUE)
      pct <- format2(length(values[values %in% lvls_remove]) / length(values), percent = TRUE)
      value_new <- gsub("%p", pct, value_new, fixed = TRUE)
      # add new factor level
      levels(values) <- c(levels(values), value_new)
      # replace all values that must be removed
      values[as.character(values) %in% lvls_remove] <- value_new
      # drop unused factor levels
      values <- droplevels(values)
      # set new level to last place, taking into account 'horizontal'
      if (isTRUE(horizontal)) {
        values <- fct_relevel(values, value_new, after = 0)
      } else {
        values <- fct_relevel(values, value_new, after = Inf)
      }
    }
    values
  }
  
  # set new factor levels
  if (!is.null(x)) {
    df[, x] <- set_max(values = get_x(df),
                       n_max = x.max_items,
                       txt = x.max_txt,
                       horizontal = horizontal)
    df$`_var_x` <- df[, x, drop = TRUE]
  }
  if (!is.null(category)) {
    df[, category] <- set_max(values = get_category(df),
                              n_max = category.max_items,
                              txt = category.max_txt,
                              horizontal = horizontal)
    df$`_var_category` <- df[, category, drop = TRUE]
  }
  if (!is.null(facet)) {
    df[, facet] <- set_max(values = get_facet(df),
                           n_max = facet.max_items,
                           txt = facet.max_txt,
                           horizontal = FALSE)
    df$`_var_facet` <- df[, facet, drop = TRUE]
  }
  
  if (all(group_sizes(df) == 1)) {
    # summarise again
    df <- summarise_data(df = df,
                         summarise_function = summarise_function,
                         decimal.mark = decimal.mark,
                         big.mark = big.mark,
                         datalabels.round = datalabels.round,
                         datalabels.format = datalabels.format,
                         y.percent = y.percent)
  }
  df
  
}

#' @importFrom dplyr mutate group_by across all_of summarise
summarise_data <- function(df,
                           summarise_function,
                           decimal.mark,
                           big.mark,
                           datalabels.round,
                           datalabels.format,
                           y.percent) {
  x <- get_x_name(df)
  y <- get_y_name(df)
  category <- get_category_name(df)
  facet <- get_facet_name(df)
  has_datalbls <- has_datalabels(df)
  summ_fn <- function(x, fn = summarise_function, ...) {
    # alter summarise_function to remove NAs
    out <- fn(x, ...)
    out[!is.na(out)]
  }
  df <- df |>
    mutate(n = get_y(df)) |>
    group_by(across(all_of(c(x, category, facet)))) |>
    summarise(n = summ_fn(n),
              .groups = "drop")
  colnames(df)[colnames(df) == "n"] <- y
  df$`_var_y` <- df[, y, drop = TRUE]
  if (!is.null(x)) df$`_var_x` <- df[, x, drop = TRUE]
  if (!is.null(category)) df$`_var_category` <- df[, category, drop = TRUE]
  if (!is.null(facet)) df$`_var_facet` <- df[, facet, drop = TRUE]
  if (isTRUE(has_datalbls)) {
    df <- df |>
      mutate(`_var_datalabels` = format_datalabels(`_var_y`,
                                                   datalabels.round = datalabels.round,
                                                   datalabels.format = datalabels.format,
                                                   decimal.mark = decimal.mark,
                                                   big.mark = big.mark,
                                                   y.percent = y.percent))
  }
  df
}

#' @importFrom dplyr n_distinct
format_datalabels <- function(datalabels,
                              datalabels.round,
                              datalabels.format,
                              decimal.mark,
                              big.mark,
                              y.percent) {
  datalabels[as.character(datalabels) %in% c("", "0")] <- NA
  datalabels_out <- datalabels
  
  if (is.function(datalabels.format)) {
    return(datalabels.format(datalabels_out))
  }
  
  if (isTRUE(y.percent)) {
    if (!is.null(datalabels.format)) {
      datalabels_out <- trimws(format2(datalabels,
                                       round = datalabels.round,
                                       decimal.mark = decimal.mark,
                                       big.mark = big.mark,
                                       percent = TRUE))
      if (datalabels.round == eval(formals(plot2)$datalabels.round) && n_distinct(datalabels) > n_distinct(datalabels_out)) {
        # formals() get the default value, so that's why it's in this if()
        plot2_message("Use ", font_blue("datalabels.round"), " to edit the rounding of datalabels")
      }
      if (datalabels.format != "%n") {
        plot2_message("Ignoring ", font_blue("datalabels.format = \"", datalabels.format, "\"", collapse = NULL),
                      " since ",  font_blue("y.percent = TRUE"))
      }
    }
  } else if (!is.null(datalabels.format) &&
             mode(datalabels) == "numeric" &&
             !inherits(datalabels, c("factor", "Date", "POSIXt"))) {
    datalabels <- as.double(datalabels)
    datalabels_out <- rep(datalabels.format, length(datalabels_out))
    if (datalabels.format %like% "%p") {
      datalabels_p <- trimws(format2(datalabels / sum(datalabels, na.rm = TRUE),
                                     round = datalabels.round,
                                     decimal.mark = decimal.mark,
                                     big.mark = big.mark,
                                     percent = TRUE))
      datalabels_out <- mapply(gsub,
                               x = datalabels_out,
                               pattern = "%p",
                               replacement = datalabels_p,
                               USE.NAMES = FALSE)
    }
    if (datalabels.format %like% "%n") {
      datalabels_n <- trimws(format2(datalabels,
                                     decimal.mark = decimal.mark,
                                     big.mark = big.mark,
                                     round = datalabels.round,
                                     force_decimals = FALSE))
      datalabels_out <- mapply(gsub,
                               x = datalabels_out,
                               pattern = "%n",
                               replacement = datalabels_n,
                               USE.NAMES = FALSE)
    }
  }
  datalabels_out
}

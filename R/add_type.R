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

#' Add Plot Element
#' 
#' Quickly and conveniently add a new 'geom' to an existing `plot2`/`ggplot` model. Like [plot2()], these functions support tidy evaluation, meaning that variables can be unquoted. Better yet, they can contain any function with any output length, or any vector. They can be added using the pipe (new base \R's `|>` or tidyverse's `%>%`).
#' @param plot a `ggplot2` plot
#' @param type a `ggplot2` geom name, all geoms are supported. Full function names can be used (e.g., `"geom_line"`), but they can also be abbreviated (e.g., `"l"`, `"line"`). These geoms can be abbreviated by their first character: area (`"a"`), boxplot (`"b"`), column (`"c"`), histogram (`"h"`), jitter (`"j"`), line (`"l"`), point (`"p"`), ribbon (`"r"`), violin (`"v"`).
#' @param mapping a mapping created with [`aes()`][ggplot2::aes()] to pass on to the geom
#' @param linetype,linewidth,shape,size,width,... arguments passed on to the geom
#' @param data data to use in mapping
#' @param move number of layers to move the newly added geom down, e.g., `move = 1` will place the newly added geom down 1 layer, thus directly under the highest layer
#' @param inherit.aes a [logical] to indicate whether the default aesthetics should be inherited, rather than combining with them
#' @param legend.value text to show in an additional legend that will be created. Since `ggplot2` does not actually support this, it may give some false-positive warnings or messages, such as "Removed 1 row containing missing values or values outside the scale range".
#' @importFrom ggplot2 is.ggplot aes
#' @rdname add_type
#' @return a `ggplot` object
#' @export
#' @examples 
#' head(iris)
#'                  
#' p <- iris |>
#'   plot2(x = Sepal.Length,
#'         y = Sepal.Width,
#'         category = Species,
#'         zoom = TRUE)
#' p
#'   
#' # if not specifying x or y, current plot data are taken
#' p |> add_line()
#'   
#' # single values for add_line() will plot 'hline' or 'vline'
#' # even considering the `category` if set
#' p |> 
#'   add_line(y = mean(Sepal.Width))
#'
#' # set `colour` to ignore existing colours
#' # and use `legend.value` to add a legend
#' p |> 
#'   add_line(y = mean(Sepal.Width),
#'            colour = "red",
#'            legend.value = "Average")
#'   
#' p |> 
#'   add_line(x = mean(Sepal.Length)) |> 
#'   add_line(y = mean(Sepal.Width))
#'   
#' p |>
#'   add_point(x = median(Sepal.Length),
#'             y = median(Sepal.Width),
#'             shape = 13,
#'             size = 25,
#'             show.legend = FALSE)
#'   
#' # multiple values will just plot multiple lines
#' p |> 
#'   add_line(y = fivenum(Sepal.Width),
#'            colour = "blue",
#'            legend.value = "Tukey's Numbers")
#'   
#' p |> 
#'   add_line(y = quantile(Sepal.Width, c(0.25, 0.5, 0.75)),
#'            colour = c("red", "black", "red"),
#'            linewidth = 1)
#'   
#' # use move to move the new layer down
#' p |> 
#'   add_point(size = 5,
#'             colour = "lightpink",
#'             move = -1)
#' 
#' # providing x and y will just plot the points as new data,
#' p |> 
#'   add_point(y = 2:4,
#'             x = 5:7,
#'             colour = "red",
#'             size = 5)
#' # even with expanded grid if x and y are not of the same length
#' p |> 
#'   add_point(y = 2:4,
#'             x = 5:8,
#'             colour = "red",
#'             size = 5)
#'
#' # any mathematical transformation of current values is supported
#' df <- data.frame(var_1 = c(1:100),
#'                  var_2 = rnorm(100, 100, 25),
#'                  var_3 = rep(LETTERS[1:5], 5))
#' df |>
#'   plot2(var_1, var_2) |> 
#'   add_line(y = mean(var_2), 
#'            linetype = 3,
#'            legend.value = "Average") |>
#'   add_col(y = var_2 / 5,
#'           width = 0.25,
#'           colour = "blue",
#'           legend.value = "This *is* **some** symbol: $beta$")
#' 
#' # plotting error bars was never easier
#' library("dplyr", warn.conflicts = FALSE)
#' df2 <- df |> 
#'   as_tibble() |> 
#'   slice(1:25) |>
#'   filter(var_1 <= 50) |> 
#'   mutate(error1 = var_2 * 0.9,
#'          error2 = var_2 * 1.1)
#' 
#' df2
#' 
#' df2 |> 
#'   plot2(var_1, var_2, var_3, type = "col", datalabels = FALSE, alpha = 0.25, width = 0.75) |> 
#'   # adding error bars was never easier - just reference the lower and upper values
#'   add_errorbar(error1, error2)
#' 
#' # adding sf objects is just as convenient as all else
#' plot2(netherlands)
#' plot2(netherlands) |>
#'   add_sf(netherlands, colour_fill = NA, colour = "red", linewidth = 1)
add_type <- function(plot, type = NULL, mapping = aes(), ..., data = NULL, move = 0) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  type <- validate_type(type[1L])
  if (type == "") {
    stop("`type` must be set for `add_type()`", call. = FALSE)
  } else if (type == "geom_smooth") {
    plot2_caution("Adding a smooth using `add_type()` is less convenient than using `plot2(..., smooth = TRUE)")
  }
  
  args <- list(...)
  if (length(args) == 1 && is.list(args[[1]])) {
    args <- args[[1]]
  }
  # "data" can also be in "args", so:
  if (is.null(data)) {
    data <- args$data
  }
  args$data <- NULL
  args <- utils::modifyList(list(mapping = mapping, data = data), args)
  args <- args[!vapply(FUN.VALUE = logical(1), args, is.null)]
  
  geom_fn <- getExportedValue(name = type, ns = asNamespace("ggplot2"))
  p <- plot +
    do.call(geom_fn, args = args)
  if (move != 0) {
    p <- move_layer(p, move = -abs(move))
  }
  p
}

NA_missing_ <- structure(NA, class = c("missing", "logical"))

#' @importFrom dplyr mutate group_by across reframe arrange select pull any_of if_else as_tibble bind_cols
#' @importFrom rlang as_label
new_geom_data <- function(plot, x, y, ..., colour_missing, inherit.aes) {
  if (!is.null(plot$mapping$colour) && isTRUE(colour_missing)) {
    category_name <- as_label(plot$mapping$colour)
    colour_unique <- unique(plot$data[[category_name]])
  } else {
    category_name <- NULL
    colour_unique <- ""
  }
  
  # split the x-part and x-part, so that even `add_point(y = 1:4, x = 1:3)` is possible with expand.grid()
  x_part <- plot$data |>
    mutate(`_row_index` = seq_len(nrow(plot$data))) |>
    # this also works if category is NULL:
    group_by(across(any_of(category_name))) |> 
    reframe(x = {{ x }},
            `_row_index` = first(`_row_index`)) |> 
    arrange(`_row_index`) |> 
    select(-`_row_index`)
  y_part <- plot$data |>
    mutate(`_row_index` = seq_len(nrow(plot$data))) |>
    # this also works if category is NULL:
    group_by(across(any_of(category_name))) |> 
    reframe(y = {{ y }},
            `_row_index` = first(`_row_index`)) |> 
    arrange(`_row_index`) |> 
    select(-`_row_index`)
  
  if (NROW(x_part) == NROW(y_part)) {
    new_df <- x_part |> 
      bind_cols(y_part |> select(-any_of(colnames(x_part))))
  } else if ("x" %in% colnames(x_part) && "y" %in% colnames(y_part)) {
    new_df <- expand.grid(x = x_part$x,
                          y = y_part$y) |> 
      as_tibble()
    category_name <- NULL
  } else if ("x" %in% colnames(x_part)) {
    new_df <- x_part
  } else if ("y" %in% colnames(y_part)) {
    new_df <- y_part
  } else {
    stop("Something went wrong - plot data could not be determined")
  }
  
  has_category <- !is.null(category_name) && !isFALSE(inherit.aes)
  has_x <- "x" %in% colnames(new_df)
  has_y <- "y" %in% colnames(new_df)
  
  if (!has_x && !has_y) {
    # just used e.g. `plot_object |> add_line()`
    inherit.aes <- TRUE
    x_name <- as_label(plot$mapping$x)
    y_name <- as_label(plot$mapping$y)
    new_df <- plot$data |> select(any_of(c(x_name, y_name)))
  }
  
  if (is.null(inherit.aes)) {
    # at this point, if we don't need to inherit, then make sure we don't
    inherit.aes <- FALSE
  }
  
  if (inherit.aes == FALSE && !has_x && has_y && NROW(plot$data) == NROW(new_df$y)) {
    # add x to data
    new_df <- new_df |>
      bind_cols(plot$data |>
                  select(x = as_label(plot$mapping$x)))
    has_x <- TRUE
  }
  if (inherit.aes == FALSE && !has_y && has_x && NROW(plot$data) == NROW(new_df$x)) {
    # add y to data
    new_df <- new_df |>
      bind_cols(plot$data |>
                  select(y = as_label(plot$mapping$y)))
    has_y <- TRUE
  }
  
  # additional parameters
  dots <- list(...)
  dots <- dots[vapply(FUN.VALUE = logical(1), dots, function(x) !identical(x, NA_missing_))]
  params <- list(inherit.aes = inherit.aes)
  params <- c(params, dots)
  if (has_category) {
    params <- utils::modifyList(params, list(colour = NULL, fill = NULL))
  }

  out <- list(plot = plot,
              has_category = has_category,
              has_x = has_x,
              has_y = has_y,
              is_single_x = has_x && !has_y && (NROW(new_df) == length(colour_unique) || NROW(new_df) == 1),
              is_single_y = has_y && !has_x && (NROW(new_df) == length(colour_unique) || NROW(new_df) == 1),
              is_single_xy = has_y && has_x && (NROW(new_df) == length(colour_unique) || NROW(new_df) == 1),
              plotdata_is_length_x = has_x && NROW(plot$data) == NROW(new_df$x),
              plotdata_is_length_y = has_y && NROW(plot$data) == NROW(new_df$y),
              params = params,
              mapping = setup_aes(x = if (has_x) "x" else as_label(plot$mapping$x),
                                   y = if (has_y) "y" else as_label(plot$mapping$y),
                                   group = 1),
              data = if (inherit.aes) NULL else new_df,
              new_df = new_df)
  
  if (!colour_missing && !has_category) {
    out$params$colour <- list(...)$colour
  } else if (has_category && !inherit.aes) {
    out$mapping <- setup_aes(out$mapping, colour = category_name)
  }
  
  if (!is.null(out$params$colour)) {
    out$params$colour <- get_colour(out$params$colour)
  }
  if (!is.null(out$params$fill)) {
    out$params$fill <- get_colour(out$params$fill)
  }
  return(out)
}

#' @rdname add_type
#' @importFrom ggplot2 geom_line aes scale_linetype_manual
#' @param x,y aesthetic arguments
#' @param colour,colour_fill colour of the line or column, will be evaluated with [get_colour()]. If `colour_fill` is missing but `colour` is given, `colour_fill` will inherit the colour set with `colour`.
#' @details The function [add_line()] will add:
#' * [`geom_hline()`][ggplot2::geom_hline()] if only `y` is provided;
#' * [`geom_vline()`][ggplot2::geom_vline()] if only `x` is provided;
#' * [`geom_line()`][ggplot2::geom_line()] in all other cases.
#' @export
add_line <- function(plot, y = NULL, x = NULL, colour = getOption("plot2.colour", "ggplot2"), linetype, linewidth, ..., inherit.aes = NULL, move = 0, legend.value = NULL) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  if (missing(linetype)) {
    linetype <- NA_missing_
  }
  if (missing(linewidth)) {
    linewidth <- NA_missing_
  }
  geom_data <- new_geom_data(plot, x = {{ x }}, y = {{ y }},
                             colour = colour, linetype = linetype, linewidth = linewidth, ...,
                             colour_missing = missing(colour), inherit.aes = inherit.aes)
  
  if (geom_data$has_y && !geom_data$has_x && !geom_data$plotdata_is_length_y) {
    type <- "hline"
    mapping <- setup_aes(yintercept = "y", colour = geom_data$plot$mapping$colour)
    geom_data$params$inherit.aes <- NULL
  } else if (geom_data$has_x && !geom_data$has_y && !geom_data$plotdata_is_length_x) {
    type <- "vline"
    mapping <- setup_aes(xintercept = "x", colour = geom_data$plot$mapping$colour)
    geom_data$params$inherit.aes <- NULL
  } else {
    type <- "line"
    mapping <- geom_data$mapping
  }
  
  p <- add_type(plot = geom_data$plot,
                type = type,
                data = geom_data$data,
                mapping = mapping,
                params = geom_data$params,
                move = move)
  
  if (!is.null(legend.value)) {
    if (is.expression(validate_title(legend.value, markdown = TRUE))) {
      label_fn <- md_to_expression
    } else {
      label_fn <- function(x) x
    }
    linetype <- ifelse(identical(linetype, NA_missing_), "solid", linetype)
    p <- p +
      geom_line(data = data.frame(x = c(Inf, Inf), y = c(Inf, Inf), group = c(legend.value, legend.value)),
                mapping = aes(x = x, y = y, linetype = group, group = group),
                colour = get_colour(colour[1L]),
                linewidth = validate_linewidth(geom_data$params$linewidth, type = "geom_line", type_backup = "geom_line"),
                inherit.aes = FALSE) +
      scale_linetype_manual(name = NULL, values = stats::setNames(linetype, legend.value), labels = label_fn)
  }
  
  p
}

#' @rdname add_type
#' @importFrom ggplot2 geom_point aes scale_shape_manual
#' @export
add_point <- function(plot, y = NULL, x = NULL, colour = getOption("plot2.colour", "ggplot2"), size, shape, ..., inherit.aes = NULL, move = 0, legend.value = NULL) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  if (missing(size)) {
    size <- NA_missing_
  }
  if (missing(shape)) {
    shape <- NA_missing_
  }
  geom_data <- new_geom_data(plot, x = {{ x }}, y = {{ y }},
                             colour = colour, size = size, shape = shape, ...,
                             colour_missing = missing(colour), inherit.aes = inherit.aes)
  
  p <- add_type(plot = geom_data$plot,
                type = "point",
                data = geom_data$data,
                mapping = geom_data$mapping,
                params = geom_data$params,
                move = move)
  
  if (!is.null(legend.value)) {
    if (is.expression(validate_title(legend.value, markdown = TRUE))) {
      label_fn <- md_to_expression
    } else {
      label_fn <- function(x) x
    }
    p <- p +
      geom_point(data = data.frame(x = c(Inf, Inf), y = c(-Inf, -Inf), group = c(legend.value, legend.value)),
                 mapping = aes(x = x, y = y, shape = group, group = group),
                 colour = get_colour(colour[1L]),
                 size = validate_size(geom_data$params$size, type = "geom_point", type_backup = "geom_point"),
                 inherit.aes = FALSE) +
      scale_shape_manual(name = NULL, values = stats::setNames(16, legend.value), labels = label_fn)
  }
  
  p
}

#' @rdname add_type
#' @importFrom ggplot2 geom_line aes geom_col scale_linewidth_manual
#' @export
add_col <- function(plot, y = NULL, x = NULL, colour = getOption("plot2.colour", "ggplot2"), colour_fill, width, ..., inherit.aes = NULL, move = 0, legend.value = NULL) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  if (missing(colour_fill)) {
    fill <- colour
  } else {
    fill <- colour_fill
  }
  if (missing(width)) {
    width <- NA_missing_
  }
  geom_data <- new_geom_data(plot, x = {{ x }}, y = {{ y }},
                             colour = colour, fill = fill, width = width, ...,
                             colour_missing = missing(colour), inherit.aes = inherit.aes)
  
  p <- add_type(plot = geom_data$plot,
                type = "column",
                data = geom_data$data,
                mapping = geom_data$mapping,
                params = geom_data$params,
                move = move)
  
  if (!is.null(legend.value)) {
    if (is.expression(validate_title(legend.value, markdown = TRUE))) {
      label_fn <- md_to_expression
    } else {
      label_fn <- function(x) x
    }
    p <- p +
      geom_col(data = data.frame(x = c(Inf, Inf), y = c(Inf, Inf), group = c(legend.value, legend.value)),
               mapping = aes(x = x, y = y, linewidth = group, group = group),
               colour = get_colour(colour[1L]),
               fill = get_colour(colour[1L]),
               inherit.aes = FALSE) +
      scale_linewidth_manual(name = NULL, values = stats::setNames(0.25, legend.value), labels = label_fn)
  }
  
  p
}

#' @rdname add_type
#' @param min,max minimum (lower) and maximum (upper) values of the error bars
#' @importFrom dplyr group_by across reframe
#' @importFrom ggplot2 aes
#' @importFrom rlang as_label
#' @details
#' The function [add_errorbar()] only adds error bars to the `y` values, see *Examples*.
#' @export
add_errorbar <- function(plot, min, max, colour = getOption("plot2.colour", "ggplot2"), width = 0.5, ..., inherit.aes = NULL, move = 0) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  
  if (!is.null(plot$mapping$colour) && missing(colour)) {
    category_name <- as_label(plot$mapping$colour)
    colour_unique <- unique(plot$data[[category_name]])
  } else {
    category_name <- NULL
    colour_unique <- ""
  }
  
  new_df <- plot$data |>
    mutate(`_row_index` = seq_len(nrow(plot$data))) |>
    # this also works if category is NULL:
    group_by(across(any_of(category_name))) |> 
    reframe(ymin = {{ min }},
            ymax = {{ max }},
            `_row_index` = `_row_index`) |> 
    arrange(`_row_index`) |> 
    select(-`_row_index`)
  
  new_df$x <- plot$data[[as_label(plot$mapping$x)]]
  
  if (!missing(colour) && is.null(inherit.aes)) {
    inherit.aes <- FALSE
    mapping <- setup_aes(x = "x", ymin = "ymin", ymax = "ymax")
  } else if (!is.null(category_name) && !isFALSE(inherit.aes)) {
    # has category
    inherit.aes <- FALSE
    mapping <- setup_aes(x = "x", ymin = "ymin", ymax = "ymax", colour = category_name)
  } else {
    inherit.aes <- TRUE
    mapping <- setup_aes(x = "x", ymin = "ymin", ymax = "ymax")
  }

  # build additional parameters
  params <- list(inherit.aes = inherit.aes)
  if (!missing(colour) && isFALSE(inherit.aes)) {
    params <- c(params, list(colour = get_colour(colour)))
  }
  params <- c(params, list(width = width))
  if (length(list(...)) > 0) {
    params <- c(params, list(...))
  }
  
  print(params)
  print(inherit.aes)
  
  add_type(plot = plot,
           type = "errorbar",
           data = new_df,
           mapping = mapping,
           params = params,
           move = move)
}

#' @rdname add_type
#' @param sf_data an 'sf' [data.frame]
#' @param datalabels a column of `sf_data` to add as label below the points
#' @param datalabels.colour,datalabels.size,datalabels.angle,datalabels.font properties of `datalabels`
#' @param datalabels.nudge_y is `datalabels` is not `NULL`, the amount of vertical adjustment of the datalabels (positive value: more to the North, negative value: more to the South)
#' @importFrom dplyr mutate
#' @importFrom ggplot2 geom_sf geom_sf_text aes is.ggplot
#' @export
add_sf <- function(plot,
                   sf_data,
                   colour = getOption("plot2.colour_sf", "grey50"),
                   colour_fill = getOption("plot2.colour_sf_fill", getOption("plot2.colour", "ggplot2")),
                   size = 2,
                   linewidth = 0.1,
                   datalabels = NULL,
                   datalabels.colour = "black",
                   datalabels.size = 3,
                   datalabels.angle = 0,
                   datalabels.font = getOption("plot2.font"),
                   datalabels.nudge_y = 2500,
                   ...,
                   inherit.aes = FALSE) {
  
  loadNamespace("sf") # will throw an error if not installed
  
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 model.", call. = FALSE)
  }
  if (!"geometry" %in% colnames(plot$data)) {
    stop("`plot` must be a ggplot2 model based on geographic data.", call. = FALSE)
  }
  
  # force sf type
  sf_data <- sf::st_as_sf(sf_data)
  
  crs <- c(plot = as.character(sf::st_crs(plot$data$geometry))[1],
           add = as.character(sf::st_crs(sf_data))[1])
  if (n_distinct(crs) > 1) {
    plot2_warning("The coordinate reference system (CRS) of `plot` and `sf_data` are different, transforming `sf_data` to ", crs[1])
    sf_data <- sf::st_transform(sf_data, crs = crs[1])
  }
  crs <- crs[1]
  
  p <- plot +
    geom_sf(data = sf_data,
            inherit.aes = inherit.aes,
            size = size,
            linewidth = linewidth,
            colour = get_colour(colour),
            fill = get_colour(colour_fill),
            ...)
  
  if (tryCatch(!is.null(datalabels), error = function(e) TRUE)) {
    
    if (abs(datalabels.nudge_y) > 0.25 && crs %unlike% "28992") {
      plot2_message(font_blue(paste0("datalabels.nudge_y = ", datalabels.nudge_y)),
                    " might be very ", ifelse(datalabels.nudge_y < 0, "low", "high"),
                    " for the current coordinate reference system (", crs, ")")
    }
    
    sf_data <- sf_data |> 
      mutate(`_var_datalabels` = {{ datalabels }})
    
    datalabels.font <- suppressMessages(validate_font(datalabels.font))
    
    p <- p +
      geom_sf_text(aes(label = `_var_datalabels`),
                   data = sf_data,
                   inherit.aes = inherit.aes,
                   size = datalabels.size,
                   family = datalabels.font,
                   angle = datalabels.angle,
                   nudge_y = datalabels.nudge_y,
                   colour = get_colour(colour),
                   fun.geometry = function(x) {
                     x[!sf::st_is_valid(x)] <- sf::st_point()
                     suppressWarnings(sf::st_point_on_surface(sf::st_zm(x)))
                   })
  }
  p
}

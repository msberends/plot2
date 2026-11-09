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

#' @importFrom dplyr select
#' @importFrom ggplot2 element_blank element_text
#' @importFrom tidyselect starts_with
back2back_plot <- function(df,
                           x.title,
                           y.title,
                           category.title,
                           title,
                           subtitle,
                           caption,
                           tag,
                           title.linelength,
                           title.colour,
                           subtitle.linelength,
                           subtitle.colour,
                           na.replace,
                           na.rm,
                           facet.position,
                           facet.colour_fill,
                           facet.bold,
                           facet.italic,
                           facet.size,
                           facet.margin,
                           facet.repeat_labels_x,
                           facet.repeat_labels_y,
                           facet.fixed_y,
                           facet.fixed_x,
                           facet.drop,
                           facet.nrow,
                           facet.relative,
                           x.date_breaks,
                           x.date_labels,
                           x.date_remove_years,
                           category.focus,
                           colour,
                           colour_fill,
                           colour_transparency,
                           x.label_angle,
                           x.label_align,
                           x.label_italic,
                           x.remove,
                           x.position,
                           x.max_items,
                           x.max_txt,
                           category.max_items,
                           category.max_txt,
                           facet.max_items,
                           facet.max_txt,
                           x.breaks,
                           x.n_breaks,
                           x.transform,
                           x.expand,
                           x.limits,
                           x.labels,
                           x.character,
                           x.drop,
                           x.zoom,
                           y.remove,
                           y.24h,
                           y.age,
                           y.scientific,
                           y.percent,
                           y.percent_break,
                           y.breaks,
                           y.n_breaks,
                           y.limits,
                           y.labels,
                           y.expand,
                           y.transform,
                           y.position,
                           y.zoom,
                           y_secondary,
                           y_secondary.type,
                           y_secondary.title,
                           y_secondary.colour,
                           y_secondary.colour_fill,
                           y_secondary.scientific,
                           y_secondary.percent,
                           y_secondary.labels,
                           category.type,
                           category.labels,
                           category.percent,
                           category.breaks,
                           category.limits,
                           category.expand,
                           category.midpoint,
                           category.transform,
                           category.date_breaks,
                           category.date_labels,
                           category.character,
                           x.sort,
                           category.sort,
                           facet.sort,
                           x.complete,
                           category.complete,
                           facet.complete,
                           datalabels,
                           datalabels.round,
                           datalabels.format,
                           datalabels.colour,
                           datalabels.colour_fill,
                           datalabels.size,
                           datalabels.angle,
                           datalabels.lineheight,
                           datalabels.percent_by,
                           decimal.mark,
                           big.mark,
                           summarise_function,
                           stacked,
                           stacked_fill,
                           horizontal,
                           reverse,
                           smooth,
                           smooth.method,
                           smooth.formula,
                           smooth.se,
                           smooth.level,
                           smooth.alpha,
                           smooth.linewidth,
                           smooth.linetype,
                           smooth.colour,
                           size,
                           linetype,
                           linewidth,
                           binwidth,
                           width,
                           jitter_seed,
                           violin_scale,
                           legend.position,
                           legend.title,
                           legend.reverse,
                           legend.nrow,
                           legend.barheight,
                           legend.barwidth,
                           legend.nbin,
                           legend.italic,
                           sankey_node_width,
                           sankey_node_whitespace,
                           sankey_alpha,
                           sankey_remove_axes,
                           zoom,
                           sep,
                           print,
                           text_factor,
                           font,
                           theme,
                           background,
                           markdown) {
  rlang::check_installed("patchwork", reason = "for back-to-back plots.")
  
  # do use facet.sort to allow order of facets
  facet_values <- as.character(unique(get_facet(df)))
  facet_values <- sort_data(facet_values, facet_values,
                            sort_method = facet.sort,
                            datapoints = facet_values,
                            summarise_function = NULL,
                            summarise_fn_name = "",
                            # reversed, except for the order in the data (as before F141)
                            horizontal = !identical(validate_sorting(facet.sort, horizontal = FALSE), "inorder"),
                            drop = FALSE,
                            argument = "facet")
  facet_values <- as.character(levels(facet_values))
  
  df_left <- df |> filter(`_var_facet` == facet_values[1])
  df_right <- df |> filter(`_var_facet` == facet_values[2])
  
  x.title <- validate_title(x.title, markdown = markdown)
  y.title <- validate_title(y.title, markdown = markdown)
  title <- validate_title(title, markdown = markdown)
  subtitle <- validate_title(subtitle, markdown = markdown)
  caption <- validate_title(caption, markdown = markdown)
  if (isTRUE(y.title)) {
    y.title <- validate_title(get_y_name(df), markdown = isTRUE(markdown), df = df)
  }
  
  
  # legend
  if (is.null(legend.position)) {
    if (has_category(df) && data_is_numeric(get_category(df))) {
      legend.position <- "right"
    } else {
      legend.position <- "top"
    }
  }
  legend.position <- validate_legend.position(legend.position)
  
  # one colour per category for both halves, from all data, so that a category has
  # the same colour on both sides also when it is missing on one side (F066)
  if (has_category(df) && !data_is_numeric(get_category(df))) {
    category_levels <- levels(as.factor(get_category(df)))
    colour <- b2b_named_colours(colour, category_levels, colour_transparency)
    if (!is.null(colour_fill) && !isTRUE(colour_fill)) {
      colour_fill <- b2b_named_colours(colour_fill, category_levels, colour_transparency)
    }
  }

  make_left <- function(y.limits) suppressMessages(
    df_left |>
      select(-starts_with("_var_")) |>
      plot2(x = !!rlang::sym(get_x_name(df)),
            y = !!get_y(df_left),
            category = !!(if (has_category(df)) rlang::sym(get_category_name(df)) else NULL),
            facet = !!facet_values[1],
            type = "col",
            horizontal = TRUE,
            x.title = NULL,
            y.transform = "reverse",
            title = NULL,
            subtitle = NULL,
            caption = NULL,
            x.position = "top", # move ticks to other side
            print = FALSE,
            facet.sort = NULL,
            legend.position = legend.position,
            y.title = y.title,
            na.replace = na.replace,
            na.rm = na.rm,
            facet.position = facet.position,
            facet.colour_fill = facet.colour_fill,
            facet.bold = facet.bold,
            facet.italic = facet.italic,
            facet.size = facet.size,
            facet.margin = facet.margin,
            x.date_breaks = x.date_breaks,
            x.date_labels = x.date_labels,
            x.date_remove_years = x.date_remove_years,
            category.focus = category.focus,
            colour = colour,
            colour_fill = colour_fill,
            colour_transparency = colour_transparency,
            x.label_angle = x.label_angle,
            x.label_align = x.label_align,
            x.label_italic = x.label_italic,
            x.remove = x.remove,
            x.max_items = x.max_items,
            x.max_txt = x.max_txt,
            category.max_items = category.max_items,
            category.max_txt = category.max_txt,
            x.breaks = x.breaks,
            x.n_breaks = x.n_breaks,
            x.transform = x.transform,
            x.expand = x.expand,
            x.limits = x.limits,
            x.labels = x.labels,
            x.character = x.character,
            x.drop = x.drop,
            x.zoom = x.zoom,
            y.remove = y.remove,
            y.24h = y.24h,
            y.age = y.age,
            y.scientific = y.scientific,
            y.percent = y.percent,
            y.percent_break = y.percent_break,
            y.breaks = y.breaks,
            y.n_breaks = y.n_breaks,
            y.limits = y.limits,
            y.labels = y.labels,
            y.expand = y.expand,
            y.position = y.position,
            y.zoom = y.zoom,
            category.type = category.type,
            category.labels = category.labels,
            category.percent = category.percent,
            category.breaks = category.breaks,
            category.limits = category.limits,
            category.expand = category.expand,
            category.midpoint = category.midpoint,
            category.transform = category.transform,
            category.date_breaks = category.date_breaks,
            category.date_labels = category.date_labels,
            category.character = category.character,
            x.sort = x.sort,
            category.sort = category.sort,
            x.complete = x.complete,
            category.complete = category.complete,
            datalabels = datalabels,
            datalabels.round = datalabels.round,
            datalabels.format = datalabels.format,
            datalabels.percent_by = datalabels.percent_by,
            datalabels.colour = datalabels.colour,
            datalabels.colour_fill = datalabels.colour_fill,
            datalabels.size = datalabels.size,
            datalabels.angle = datalabels.angle,
            datalabels.lineheight = datalabels.lineheight,
            decimal.mark = decimal.mark,
            big.mark = big.mark,
            summarise_function = summarise_function,
            stacked = stacked,
            stacked_fill = stacked_fill,
            reverse = reverse,
            smooth = smooth,
            smooth.method = smooth.method,
            smooth.formula = smooth.formula,
            smooth.se = smooth.se,
            smooth.level = smooth.level,
            smooth.alpha = smooth.alpha,
            smooth.linewidth = smooth.linewidth,
            smooth.linetype = smooth.linetype,
            smooth.colour = smooth.colour,
            size = size,
            linetype = linetype,
            linewidth = linewidth,
            binwidth = binwidth,
            width = width,
            jitter_seed = jitter_seed,
            violin_scale = violin_scale,
            legend.title = legend.title,
            legend.reverse = legend.reverse,
            legend.nrow = legend.nrow,
            legend.barheight = legend.barheight,
            legend.barwidth = legend.barwidth,
            legend.nbin = legend.nbin,
            legend.italic = legend.italic,
            sankey_node_width = sankey_node_width,
            sankey_node_whitespace = sankey_node_whitespace,
            sankey_alpha = sankey_alpha,
            sankey_remove_axes = sankey_remove_axes,
            zoom = zoom,
            sep = sep,
            text_factor = text_factor,
            font = font,
            theme = theme,
            background = background,
            markdown = markdown,
            # keys for all categories, also those missing in this half (F066)
            show.legend = if (has_category(df)) TRUE else NA
      ) +
      ggplot2::theme(axis.text.y = element_blank(),
            plot.margin = margin(5,5,5,5))
  )
  
  make_right <- function(y.limits) suppressMessages(
    df_right |>
      select(-starts_with("_var_")) |>
      plot2(x = !!rlang::sym(get_x_name(df)),
            y = !!get_y(df_right),
            category = !!(if (has_category(df)) rlang::sym(get_category_name(df)) else NULL),
            facet = !!facet_values[2],
            type = "col",
            horizontal = TRUE,
            x.title = NULL,
            y.transform = "identity",
            title = NULL,
            subtitle = NULL,
            caption = NULL,
            print = FALSE,
            facet.sort = NULL,
            legend.position = legend.position,
            y.title = y.title,
            na.replace = na.replace,
            na.rm = na.rm,
            facet.position = facet.position,
            facet.colour_fill = facet.colour_fill,
            facet.bold = facet.bold,
            facet.italic = facet.italic,
            facet.size = facet.size,
            facet.margin = facet.margin,
            x.date_breaks = x.date_breaks,
            x.date_labels = x.date_labels,
            x.date_remove_years = x.date_remove_years,
            category.focus = category.focus,
            colour = colour,
            colour_fill = colour_fill,
            colour_transparency = colour_transparency,
            x.label_angle = x.label_angle,
            x.label_align = x.label_align,
            x.label_italic = x.label_italic,
            x.remove = x.remove,
            x.max_items = x.max_items,
            x.max_txt = x.max_txt,
            category.max_items = category.max_items,
            category.max_txt = category.max_txt,
            x.breaks = x.breaks,
            x.n_breaks = x.n_breaks,
            x.transform = x.transform,
            x.expand = x.expand,
            x.limits = x.limits,
            x.labels = x.labels,
            x.character = x.character,
            x.drop = x.drop,
            x.zoom = x.zoom,
            y.remove = y.remove,
            y.24h = y.24h,
            y.age = y.age,
            y.scientific = y.scientific,
            y.percent = y.percent,
            y.percent_break = y.percent_break,
            y.breaks = y.breaks,
            y.n_breaks = y.n_breaks,
            y.limits = y.limits,
            y.labels = y.labels,
            y.expand = y.expand,
            y.position = y.position,
            y.zoom = y.zoom,
            category.type = category.type,
            category.labels = category.labels,
            category.percent = category.percent,
            category.breaks = category.breaks,
            category.limits = category.limits,
            category.expand = category.expand,
            category.midpoint = category.midpoint,
            category.transform = category.transform,
            category.date_breaks = category.date_breaks,
            category.date_labels = category.date_labels,
            category.character = category.character,
            x.sort = x.sort,
            category.sort = category.sort,
            x.complete = x.complete,
            category.complete = category.complete,
            datalabels = datalabels,
            datalabels.round = datalabels.round,
            datalabels.format = datalabels.format,
            datalabels.percent_by = datalabels.percent_by,
            datalabels.colour = datalabels.colour,
            datalabels.colour_fill = datalabels.colour_fill,
            datalabels.size = datalabels.size,
            datalabels.angle = datalabels.angle,
            datalabels.lineheight = datalabels.lineheight,
            decimal.mark = decimal.mark,
            big.mark = big.mark,
            summarise_function = summarise_function,
            stacked = stacked,
            stacked_fill = stacked_fill,
            reverse = reverse,
            smooth = smooth,
            smooth.method = smooth.method,
            smooth.formula = smooth.formula,
            smooth.se = smooth.se,
            smooth.level = smooth.level,
            smooth.alpha = smooth.alpha,
            smooth.linewidth = smooth.linewidth,
            smooth.linetype = smooth.linetype,
            smooth.colour = smooth.colour,
            size = size,
            linetype = linetype,
            linewidth = linewidth,
            binwidth = binwidth,
            width = width,
            jitter_seed = jitter_seed,
            violin_scale = violin_scale,
            legend.title = legend.title,
            legend.reverse = legend.reverse,
            legend.nrow = legend.nrow,
            legend.barheight = legend.barheight,
            legend.barwidth = legend.barwidth,
            legend.nbin = legend.nbin,
            legend.italic = legend.italic,
            sankey_node_width = sankey_node_width,
            sankey_node_whitespace = sankey_node_whitespace,
            sankey_alpha = sankey_alpha,
            sankey_remove_axes = sankey_remove_axes,
            zoom = zoom,
            sep = sep,
            text_factor = text_factor,
            font = font,
            theme = theme,
            background = background,
            markdown = markdown,
            # keys for all categories, also those missing in this half (F066)
            show.legend = if (has_category(df)) TRUE else NA
      ) +
      ggplot2::theme(axis.text.y = element_text(hjust = 0.5,
                                       margin = margin(l = 10, r = 10)),
            plot.margin = margin(5, 5, 5, 0))
  )

  left_plot <- make_left(y.limits)
  right_plot <- make_right(y.limits)
  category_levels <- if (has_category(df) && !data_is_numeric(get_category(df))) levels(as.factor(get_category(df))) else NULL
  if (is.null(y.limits)) {
    # one value scale for both halves, so that equal lengths mean equal values (F064)
    y_max <- max(b2b_max_value(left_plot), b2b_max_value(right_plot))
    if (is.finite(y_max) && y_max > 0) {
      left_plot <- make_left(c(0, y_max))
      right_plot <- make_right(c(0, y_max))
    }
  }
  if (!is.null(category_levels)) {
    left_plot <- b2b_all_levels(left_plot, category_levels)
    right_plot <- b2b_all_levels(right_plot, category_levels)
  }

  p <- patchwork::wrap_plots(left_plot,
                             right_plot,
                             nrow = 1) +
    patchwork::plot_layout(guides = "collect",
                           axis_titles = "collect") +
    patchwork::plot_annotation(title = title,
                               subtitle = subtitle,
                               caption = caption,
                               theme = validate_theme(theme, "geom_col",
                                                      background = background,
                                                      text_factor = text_factor,
                                                      font = font,
                                                      horizontal = TRUE,
                                                      x.remove = x.remove,
                                                      y.remove = y.remove,
                                                      x.label_angle = x.label_angle,
                                                      x.label_align = x.label_align,
                                                      x.label_italic = x.label_italic,
                                                      facet.colour_fill = facet.colour_fill,
                                                      facet.bold = facet.bold,
                                                      facet.italic = facet.italic,
                                                      facet.size = facet.size,
                                                      facet.margin = facet.margin,
                                                      legend.italic = legend.italic,
                                                      title.colour = title.colour,
                                                      subtitle.colour = subtitle.colour,
                                                      has_y_secondary = FALSE,
                                                      has_category = has_category(df),
                                                      col_y_primary = NULL,
                                                      col_y_secondary = NULL,
                                                      sankey_remove_axes = FALSE)) & 
    ggplot2::theme(legend.position = legend.position,
          legend.justification = "center",
          legend.direction = "horizontal")
  
  
  # return plot ----
  if (isTRUE(print)) {
    print(p)
    invisible(p)
  } else {
    p
  }
}

# colours named by category level, unless they are named already
b2b_named_colours <- function(colour, levels, transparency) {
  if (!is.null(names(colour)) || length(levels) == 0) {
    return(colour)
  }
  cols <- if (length(colour) == 1) {
    get_colour(colour, length = length(levels), transparency = transparency)
  } else {
    get_colour(colour, transparency = transparency)
  }
  if (length(cols) < length(levels)) {
    cols <- rep_len(cols, length(levels))
  }
  cols <- as.character(cols)[seq_along(levels)]
  stats::setNames(cols, levels)
}

# the largest absolute value of the value axis of one half, including the room
# that plot2 leaves for datalabels
b2b_max_value <- function(p) {
  built <- ggplot2::ggplot_build(p)
  params <- built$layout$panel_params[[1]]
  # the halves are horizontal: the value axis is drawn as x
  range <- params$x.range
  if (is.null(range)) range <- params$x$continuous_range
  range <- abs(suppressWarnings(as.numeric(range)))
  if (any(is.finite(range))) max(range[is.finite(range)]) else -Inf
}

# the same categories in the legend of both halves, so that patchwork collects one
# legend (F066): a copy of each discrete colour scale with all levels as limits
b2b_all_levels <- function(p, levels) {
  for (aes in c("colour", "fill")) {
    sc <- p$scales$get_scales(aes)
    if (!is.null(sc) && inherits(sc, "ScaleDiscrete")) {
      sc <- sc$clone()
      sc$limits <- levels
      p <- suppressMessages(p + sc)
    }
  }
  p
}

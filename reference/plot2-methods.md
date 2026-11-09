# Methods for [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)

These are the implemented methods for different S3 classes to be used in
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md). Since
they have an extensive list of arguments, they are placed here on a
separate manual page.

## Usage

``` r
# Default S3 method
plot2(
  .data,
  x = NULL,
  y = NULL,
  category = NULL,
  facet = NULL,
  type = NULL,
  x.title = TRUE,
  y.title = TRUE,
  category.title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  title.linelength = 60,
  title.colour = getOption("plot2.colour_font_primary", "black"),
  subtitle.linelength = 60,
  subtitle.colour = getOption("plot2.colour_font_secondary", "grey35"),
  na.replace = "",
  na.rm = FALSE,
  facet.position = "top",
  facet.colour_fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_labels_x = TRUE,
  facet.repeat_labels_y = NULL,
  facet.fixed_y = NULL,
  facet.fixed_x = NULL,
  facet.drop = FALSE,
  facet.nrow = NULL,
  facet.relative = FALSE,
  x.date_breaks = NULL,
  x.date_labels = NULL,
  x.date_remove_years = NULL,
  category.focus = NULL,
  colour = getOption("plot2.colour", "ggplot2"),
  colour_fill = NULL,
  colour_transparency = 0,
  x.label_angle = 0,
  x.label_align = NULL,
  x.label_italic = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = NULL,
  x.max_txt = "(rest, x%n)",
  category.max_items = Inf,
  category.max_txt = "(rest, x%n)",
  facet.max_items = Inf,
  facet.max_txt = "(rest, x%n)",
  x.breaks = NULL,
  x.n_breaks = NULL,
  x.transform = "identity",
  x.expand = NULL,
  x.limits = NULL,
  x.labels = NULL,
  x.character = NULL,
  x.drop = FALSE,
  x.zoom = NULL,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = NULL,
  y.percent_break = NULL,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = NULL,
  y_secondary.title = TRUE,
  y_secondary.colour = NULL,
  y_secondary.colour_fill = NULL,
  y_secondary.scientific = NULL,
  y_secondary.percent = FALSE,
  y_secondary.labels = NULL,
  category.type = "colour",
  category.labels = NULL,
  category.percent = FALSE,
  category.breaks = NULL,
  category.limits = NULL,
  category.expand = 0,
  category.midpoint = NULL,
  category.transform = "identity",
  category.date_breaks = NULL,
  category.date_labels = NULL,
  category.character = NULL,
  x.sort = NULL,
  category.sort = TRUE,
  facet.sort = TRUE,
  x.complete = NULL,
  category.complete = NULL,
  facet.complete = NULL,
  datalabels = NULL,
  datalabels.round = ifelse(isTRUE(y.percent), 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  datalabels.percent_by = "x",
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = NULL,
  stacked = FALSE,
  stacked_fill = FALSE,
  horizontal = FALSE,
  reverse = horizontal,
  smooth = NULL,
  smooth.method = NULL,
  smooth.formula = NULL,
  smooth.se = TRUE,
  smooth.level = 0.95,
  smooth.alpha = NULL,
  smooth.linewidth = NULL,
  smooth.linetype = NULL,
  smooth.colour = NULL,
  size = NULL,
  linetype = 1,
  linewidth = NULL,
  binwidth = NULL,
  width = NULL,
  jitter_seed = NA,
  violin_scale = "count",
  legend = NULL,
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey_node_width = 0.15,
  sankey_node_whitespace = 0.03,
  sankey_alpha = 0.5,
  sankey_remove_axes = NULL,
  zoom = FALSE,
  sep = " / ",
  print = FALSE,
  text_factor = getOption("plot2.text_factor", 1),
  font = getOption("plot2.font"),
  theme = getOption("plot2.theme", "theme_minimal2"),
  background = getOption("plot2.colour_background", "white"),
  markdown = TRUE,
  ...
)

# S3 method for class 'formula'
plot2(
  .data = NULL,
  x = NULL,
  y = NULL,
  category = NULL,
  facet = NULL,
  type = NULL,
  x.title = TRUE,
  y.title = TRUE,
  category.title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  title.linelength = 60,
  title.colour = getOption("plot2.colour_font_primary", "black"),
  subtitle.linelength = 60,
  subtitle.colour = getOption("plot2.colour_font_secondary", "grey35"),
  na.replace = "",
  na.rm = FALSE,
  facet.position = "top",
  facet.colour_fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_labels_x = TRUE,
  facet.repeat_labels_y = NULL,
  facet.fixed_y = NULL,
  facet.fixed_x = NULL,
  facet.drop = FALSE,
  facet.nrow = NULL,
  facet.relative = FALSE,
  x.date_breaks = NULL,
  x.date_labels = NULL,
  x.date_remove_years = NULL,
  category.focus = NULL,
  colour = getOption("plot2.colour", "ggplot2"),
  colour_fill = NULL,
  colour_transparency = 0,
  x.label_angle = 0,
  x.label_align = NULL,
  x.label_italic = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = NULL,
  x.max_txt = "(rest, x%n)",
  category.max_items = Inf,
  category.max_txt = "(rest, x%n)",
  facet.max_items = Inf,
  facet.max_txt = "(rest, x%n)",
  x.breaks = NULL,
  x.n_breaks = NULL,
  x.transform = "identity",
  x.expand = NULL,
  x.limits = NULL,
  x.labels = NULL,
  x.character = NULL,
  x.drop = FALSE,
  x.zoom = NULL,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = NULL,
  y.percent_break = NULL,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = NULL,
  y_secondary.title = TRUE,
  y_secondary.colour = NULL,
  y_secondary.colour_fill = NULL,
  y_secondary.scientific = NULL,
  y_secondary.percent = FALSE,
  y_secondary.labels = NULL,
  category.type = "colour",
  category.labels = NULL,
  category.percent = FALSE,
  category.breaks = NULL,
  category.limits = NULL,
  category.expand = 0,
  category.midpoint = NULL,
  category.transform = "identity",
  category.date_breaks = NULL,
  category.date_labels = NULL,
  category.character = NULL,
  x.sort = NULL,
  category.sort = TRUE,
  facet.sort = TRUE,
  x.complete = NULL,
  category.complete = NULL,
  facet.complete = NULL,
  datalabels = NULL,
  datalabels.round = ifelse(isTRUE(y.percent), 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  datalabels.percent_by = "x",
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = NULL,
  stacked = FALSE,
  stacked_fill = FALSE,
  horizontal = FALSE,
  reverse = horizontal,
  smooth = NULL,
  smooth.method = NULL,
  smooth.formula = NULL,
  smooth.se = TRUE,
  smooth.level = 0.95,
  smooth.alpha = NULL,
  smooth.linewidth = NULL,
  smooth.linetype = NULL,
  smooth.colour = NULL,
  size = NULL,
  linetype = 1,
  linewidth = NULL,
  binwidth = NULL,
  width = NULL,
  jitter_seed = NA,
  violin_scale = "count",
  legend = NULL,
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey_node_width = 0.15,
  sankey_node_whitespace = 0.03,
  sankey_alpha = 0.5,
  sankey_remove_axes = NULL,
  zoom = FALSE,
  sep = " / ",
  print = FALSE,
  text_factor = getOption("plot2.text_factor", 1),
  font = getOption("plot2.font"),
  theme = getOption("plot2.theme", "theme_minimal2"),
  background = getOption("plot2.colour_background", "white"),
  markdown = TRUE,
  data = NULL,
  ...
)

# S3 method for class 'freq'
plot2(
  .data,
  x = .data$item,
  y = .data$count,
  category = NULL,
  facet = NULL,
  type = NULL,
  x.title = "Item",
  y.title = "Count",
  category.title = TRUE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  title.linelength = 60,
  title.colour = getOption("plot2.colour_font_primary", "black"),
  subtitle.linelength = 60,
  subtitle.colour = getOption("plot2.colour_font_secondary", "grey35"),
  na.replace = "",
  na.rm = FALSE,
  facet.position = "top",
  facet.colour_fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_labels_x = TRUE,
  facet.repeat_labels_y = NULL,
  facet.fixed_y = NULL,
  facet.fixed_x = NULL,
  facet.drop = FALSE,
  facet.nrow = NULL,
  facet.relative = FALSE,
  x.date_breaks = NULL,
  x.date_labels = NULL,
  x.date_remove_years = NULL,
  category.focus = NULL,
  colour = getOption("plot2.colour", "ggplot2"),
  colour_fill = NULL,
  colour_transparency = 0,
  x.label_angle = 0,
  x.label_align = NULL,
  x.label_italic = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = NULL,
  x.max_txt = "(rest, x%n)",
  category.max_items = Inf,
  category.max_txt = "(rest, x%n)",
  facet.max_items = Inf,
  facet.max_txt = "(rest, x%n)",
  x.breaks = NULL,
  x.n_breaks = NULL,
  x.transform = "identity",
  x.expand = NULL,
  x.limits = NULL,
  x.labels = NULL,
  x.character = NULL,
  x.drop = FALSE,
  x.zoom = NULL,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = NULL,
  y.percent_break = NULL,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = NULL,
  y_secondary.title = TRUE,
  y_secondary.colour = NULL,
  y_secondary.colour_fill = NULL,
  y_secondary.scientific = NULL,
  y_secondary.percent = FALSE,
  y_secondary.labels = NULL,
  category.type = "colour",
  category.labels = NULL,
  category.percent = FALSE,
  category.breaks = NULL,
  category.limits = NULL,
  category.expand = 0,
  category.midpoint = NULL,
  category.transform = "identity",
  category.date_breaks = NULL,
  category.date_labels = NULL,
  category.character = NULL,
  x.sort = "freq-desc",
  category.sort = TRUE,
  facet.sort = TRUE,
  x.complete = NULL,
  category.complete = NULL,
  facet.complete = NULL,
  datalabels = NULL,
  datalabels.round = ifelse(isTRUE(y.percent), 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  datalabels.percent_by = "x",
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = NULL,
  stacked = FALSE,
  stacked_fill = FALSE,
  horizontal = FALSE,
  reverse = horizontal,
  smooth = NULL,
  smooth.method = NULL,
  smooth.formula = NULL,
  smooth.se = TRUE,
  smooth.level = 0.95,
  smooth.alpha = NULL,
  smooth.linewidth = NULL,
  smooth.linetype = NULL,
  smooth.colour = NULL,
  size = NULL,
  linetype = 1,
  linewidth = NULL,
  binwidth = NULL,
  width = NULL,
  jitter_seed = NA,
  violin_scale = "count",
  legend = NULL,
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey_node_width = 0.15,
  sankey_node_whitespace = 0.03,
  sankey_alpha = 0.5,
  sankey_remove_axes = NULL,
  zoom = FALSE,
  sep = " / ",
  print = FALSE,
  text_factor = getOption("plot2.text_factor", 1),
  font = getOption("plot2.font"),
  theme = getOption("plot2.theme", "theme_minimal2"),
  background = getOption("plot2.colour_background", "white"),
  markdown = TRUE,
  ...
)

# S3 method for class 'sf'
plot2(
  .data,
  x = NULL,
  y = NULL,
  category = NULL,
  facet = NULL,
  type = NULL,
  x.title = FALSE,
  y.title = FALSE,
  category.title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  title.linelength = 60,
  title.colour = getOption("plot2.colour_font_primary", "black"),
  subtitle.linelength = 60,
  subtitle.colour = getOption("plot2.colour_font_secondary", "grey35"),
  na.replace = "",
  na.rm = FALSE,
  facet.position = "top",
  facet.colour_fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_labels_x = TRUE,
  facet.repeat_labels_y = NULL,
  facet.fixed_y = NULL,
  facet.fixed_x = NULL,
  facet.drop = FALSE,
  facet.nrow = NULL,
  facet.relative = FALSE,
  x.date_breaks = NULL,
  x.date_labels = NULL,
  x.date_remove_years = NULL,
  category.focus = NULL,
  colour = getOption("plot2.colour_sf", "grey50"),
  colour_fill = getOption("plot2.colour_sf_fill", getOption("plot2.colour", "ggplot2")),
  colour_transparency = 0,
  x.label_angle = 0,
  x.label_align = NULL,
  x.label_italic = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = NULL,
  x.max_txt = "(rest, x%n)",
  category.max_items = Inf,
  category.max_txt = "(rest, x%n)",
  facet.max_items = Inf,
  facet.max_txt = "(rest, x%n)",
  x.breaks = NULL,
  x.n_breaks = NULL,
  x.transform = "identity",
  x.expand = 0,
  x.limits = NULL,
  x.labels = NULL,
  x.character = NULL,
  x.drop = FALSE,
  x.zoom = NULL,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = NULL,
  y.percent_break = NULL,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = 0,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = NULL,
  y_secondary.title = TRUE,
  y_secondary.colour = NULL,
  y_secondary.colour_fill = NULL,
  y_secondary.scientific = NULL,
  y_secondary.percent = FALSE,
  y_secondary.labels = NULL,
  category.type = "colour",
  category.labels = NULL,
  category.percent = FALSE,
  category.breaks = NULL,
  category.limits = NULL,
  category.expand = 0,
  category.midpoint = NULL,
  category.transform = "identity",
  category.date_breaks = NULL,
  category.date_labels = NULL,
  category.character = NULL,
  x.sort = NULL,
  category.sort = TRUE,
  facet.sort = TRUE,
  x.complete = NULL,
  category.complete = NULL,
  facet.complete = NULL,
  datalabels = NULL,
  datalabels.round = ifelse(isTRUE(y.percent), 2, 1),
  datalabels.format = NULL,
  datalabels.colour = "black",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  datalabels.percent_by = "x",
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = NULL,
  stacked = FALSE,
  stacked_fill = FALSE,
  horizontal = FALSE,
  reverse = horizontal,
  smooth = NULL,
  smooth.method = NULL,
  smooth.formula = NULL,
  smooth.se = TRUE,
  smooth.level = 0.95,
  smooth.alpha = NULL,
  smooth.linewidth = NULL,
  smooth.linetype = NULL,
  smooth.colour = NULL,
  size = NULL,
  linetype = 1,
  linewidth = NULL,
  binwidth = NULL,
  width = NULL,
  jitter_seed = NA,
  violin_scale = "count",
  legend = NULL,
  legend.position = "right",
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey_node_width = 0.15,
  sankey_node_whitespace = 0.03,
  sankey_alpha = 0.5,
  sankey_remove_axes = NULL,
  zoom = FALSE,
  sep = " / ",
  print = FALSE,
  text_factor = getOption("plot2.text_factor", 1),
  font = getOption("plot2.font"),
  theme = theme_minimal2(panel.grid.major = element_blank(), panel.grid.minor =
    element_blank(), panel.border = element_blank(), plot.margin = unit(c(5, 5, 0, 0),
    units = "pt"), axis.title = element_blank(), axis.text = element_blank(), axis.line =
    element_blank(), axis.ticks = element_blank()),
  background = getOption("plot2.colour_background", "white"),
  markdown = TRUE,
  crs = NULL,
  datalabels.centroid = NULL,
  ...
)

# S3 method for class 'data.frame'
plot2(
  .data,
  x = NULL,
  y = NULL,
  category = NULL,
  facet = NULL,
  type = NULL,
  x.title = TRUE,
  y.title = TRUE,
  category.title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  title.linelength = 60,
  title.colour = getOption("plot2.colour_font_primary", "black"),
  subtitle.linelength = 60,
  subtitle.colour = getOption("plot2.colour_font_secondary", "grey35"),
  na.replace = "",
  na.rm = FALSE,
  facet.position = "top",
  facet.colour_fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_labels_x = TRUE,
  facet.repeat_labels_y = NULL,
  facet.fixed_y = NULL,
  facet.fixed_x = NULL,
  facet.drop = FALSE,
  facet.nrow = NULL,
  facet.relative = FALSE,
  x.date_breaks = NULL,
  x.date_labels = NULL,
  x.date_remove_years = NULL,
  category.focus = NULL,
  colour = getOption("plot2.colour", "ggplot2"),
  colour_fill = NULL,
  colour_transparency = 0,
  x.label_angle = 0,
  x.label_align = NULL,
  x.label_italic = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = NULL,
  x.max_txt = "(rest, x%n)",
  category.max_items = Inf,
  category.max_txt = "(rest, x%n)",
  facet.max_items = Inf,
  facet.max_txt = "(rest, x%n)",
  x.breaks = NULL,
  x.n_breaks = NULL,
  x.transform = "identity",
  x.expand = NULL,
  x.limits = NULL,
  x.labels = NULL,
  x.character = NULL,
  x.drop = FALSE,
  x.zoom = NULL,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = NULL,
  y.percent_break = NULL,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = NULL,
  y_secondary.title = TRUE,
  y_secondary.colour = NULL,
  y_secondary.colour_fill = NULL,
  y_secondary.scientific = NULL,
  y_secondary.percent = FALSE,
  y_secondary.labels = NULL,
  category.type = "colour",
  category.labels = NULL,
  category.percent = FALSE,
  category.breaks = NULL,
  category.limits = NULL,
  category.expand = 0,
  category.midpoint = NULL,
  category.transform = "identity",
  category.date_breaks = NULL,
  category.date_labels = NULL,
  category.character = NULL,
  x.sort = NULL,
  category.sort = TRUE,
  facet.sort = TRUE,
  x.complete = NULL,
  category.complete = NULL,
  facet.complete = NULL,
  datalabels = NULL,
  datalabels.round = ifelse(isTRUE(y.percent), 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  datalabels.percent_by = "x",
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = NULL,
  stacked = FALSE,
  stacked_fill = FALSE,
  horizontal = FALSE,
  reverse = horizontal,
  smooth = NULL,
  smooth.method = NULL,
  smooth.formula = NULL,
  smooth.se = TRUE,
  smooth.level = 0.95,
  smooth.alpha = NULL,
  smooth.linewidth = NULL,
  smooth.linetype = NULL,
  smooth.colour = NULL,
  size = NULL,
  linetype = 1,
  linewidth = NULL,
  binwidth = NULL,
  width = NULL,
  jitter_seed = NA,
  violin_scale = "count",
  legend = NULL,
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey_node_width = 0.15,
  sankey_node_whitespace = 0.03,
  sankey_alpha = 0.5,
  sankey_remove_axes = NULL,
  zoom = FALSE,
  sep = " / ",
  print = FALSE,
  text_factor = getOption("plot2.text_factor", 1),
  font = getOption("plot2.font"),
  theme = getOption("plot2.theme", "theme_minimal2"),
  background = getOption("plot2.colour_background", "white"),
  markdown = TRUE,
  ...
)

# S3 method for class 'matrix'
plot2(
  .data,
  x = NULL,
  y = NULL,
  category = NULL,
  facet = NULL,
  type = NULL,
  x.title = FALSE,
  y.title = FALSE,
  category.title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  title.linelength = 60,
  title.colour = getOption("plot2.colour_font_primary", "black"),
  subtitle.linelength = 60,
  subtitle.colour = getOption("plot2.colour_font_secondary", "grey35"),
  na.replace = "",
  na.rm = FALSE,
  facet.position = "top",
  facet.colour_fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_labels_x = TRUE,
  facet.repeat_labels_y = NULL,
  facet.fixed_y = NULL,
  facet.fixed_x = NULL,
  facet.drop = FALSE,
  facet.nrow = NULL,
  facet.relative = FALSE,
  x.date_breaks = NULL,
  x.date_labels = NULL,
  x.date_remove_years = NULL,
  category.focus = NULL,
  colour = getOption("plot2.colour", "ggplot2"),
  colour_fill = NULL,
  colour_transparency = 0,
  x.label_angle = 0,
  x.label_align = NULL,
  x.label_italic = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = NULL,
  x.max_txt = "(rest, x%n)",
  category.max_items = Inf,
  category.max_txt = "(rest, x%n)",
  facet.max_items = Inf,
  facet.max_txt = "(rest, x%n)",
  x.breaks = NULL,
  x.n_breaks = NULL,
  x.transform = "identity",
  x.expand = NULL,
  x.limits = NULL,
  x.labels = NULL,
  x.character = NULL,
  x.drop = FALSE,
  x.zoom = NULL,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = NULL,
  y.percent_break = NULL,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = NULL,
  y_secondary.title = TRUE,
  y_secondary.colour = NULL,
  y_secondary.colour_fill = NULL,
  y_secondary.scientific = NULL,
  y_secondary.percent = FALSE,
  y_secondary.labels = NULL,
  category.type = "colour",
  category.labels = NULL,
  category.percent = FALSE,
  category.breaks = NULL,
  category.limits = NULL,
  category.expand = 0,
  category.midpoint = NULL,
  category.transform = "identity",
  category.date_breaks = NULL,
  category.date_labels = NULL,
  category.character = NULL,
  x.sort = NULL,
  category.sort = TRUE,
  facet.sort = TRUE,
  x.complete = NULL,
  category.complete = NULL,
  facet.complete = NULL,
  datalabels = NULL,
  datalabels.round = ifelse(isTRUE(y.percent), 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  datalabels.percent_by = "x",
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = NULL,
  stacked = FALSE,
  stacked_fill = FALSE,
  horizontal = FALSE,
  reverse = horizontal,
  smooth = NULL,
  smooth.method = NULL,
  smooth.formula = NULL,
  smooth.se = TRUE,
  smooth.level = 0.95,
  smooth.alpha = NULL,
  smooth.linewidth = NULL,
  smooth.linetype = NULL,
  smooth.colour = NULL,
  size = NULL,
  linetype = 1,
  linewidth = NULL,
  binwidth = NULL,
  width = NULL,
  jitter_seed = NA,
  violin_scale = "count",
  legend = NULL,
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey_node_width = 0.15,
  sankey_node_whitespace = 0.03,
  sankey_alpha = 0.5,
  sankey_remove_axes = NULL,
  zoom = FALSE,
  sep = " / ",
  print = FALSE,
  text_factor = getOption("plot2.text_factor", 1),
  font = getOption("plot2.font"),
  theme = getOption("plot2.theme", "theme_minimal2"),
  background = getOption("plot2.colour_background", "white"),
  markdown = TRUE,
  ...
)
```

## Arguments

- .data, data:

  Data to plot. A single character value that is the name of a data
  frame in the calling environment (such as `plot2("df")`, as used in
  tool calling) plots that data frame; only the calling environment is
  searched, not the search path.

- x:

  Plotting 'direction' for the X-axis. This can be:

  - A single variable from `.data`, such as `x = column1`

  - A [function](https://rdrr.io/r/base/function.html) to calculate over
    one or more variables from `.data`, such as
    `x = format(column1, "%Y")`, or
    `x = ifelse(column1 == "A", "Group A", "Other")`

  - Multiple variables from `.data`, such as
    `x = c(column1, column2, column2)`, or using [selection
    helpers](https://tidyselect.r-lib.org/reference/language.html) such
    as `x = where(is.character)` or `x = starts_with("var_")` *(only
    allowed and required for Sankey plots using `type = "sankey"`)*

- y:

  Values to use for plotting along the Y-axis. This can be:

  - A single variable from `.data`, such as `y = column1`

  - Multiple variables from `.data`, such as `y = c(column1, column2)`
    or `y = c(name1 = column1, "name 2" = column2)`, or using [selection
    helpers](https://tidyselect.r-lib.org/reference/language.html) such
    as `y = where(is.double)` or `y = starts_with("var_")` *(multiple
    variables only allowed if `category` is not set)*

  - A [function](https://rdrr.io/r/base/function.html) to calculate over
    `.data` returning a single value, such as `y = n()` for the row
    count, or based on other variables such as
    `y = n_distinct(person_id)`, `y = max(column1)`, or
    `y = median(column2) / column3`

  - A [function](https://rdrr.io/r/base/function.html) to calculate over
    `.data` returning multiple values, such as
    `y = quantile(column1, c(0.25, 0.75))` or `y = range(age)`
    *(multiple values only allowed if `category` is not set)*

- category, facet:

  Plotting 'direction' (`category` is called 'fill' and 'colour' in
  `ggplot2`). This can be:

  - A single variable from `.data`, such as `category = column1`

  - A [function](https://rdrr.io/r/base/function.html) to calculate over
    one or more variables from `.data`, such as
    `category = median(column2) / column3`, or
    `facet = ifelse(column1 == "A", "Group A", "Other")`

  - Multiple variables from `.data`, such as
    `facet = c(column1, column2)` (use `sep` to control the separator
    character)

  - One or more variables from `.data` using [selection
    helpers](https://tidyselect.r-lib.org/reference/language.html), such
    as `category = where(is.double)` or `facet = starts_with("var_")`

  The `category` can also be a date or date/time (class `Date` or
  `POSIXt`).

- type:

  Type of visualisation to use. This can be:

  - A `ggplot2` geom name or their abbreviation such as `"col"` and
    `"point"`. All geoms are supported (including
    [`geom_blank()`](https://ggplot2.tidyverse.org/reference/geom_blank.html)).

    Full function names can be used (e.g., `"geom_histogram"`), but they
    can also be abbreviated (e.g., `"h"`, `"hist"`). The following geoms
    can be abbreviated by their first character: area (`"a"`), boxplot
    (`"b"`), column (`"c"`), histogram (`"h"`), jitter (`"j"`), line
    (`"l"`), point (`"p"`), ribbon (`"r"`), and violin (`"v"`). Beeswarm
    plots (`"beeswarm"`, short: `"bs"`) require the `ggbeeswarm`
    package.

    Please note: in `ggplot2`, 'bars' and 'columns' are equal, while it
    is common to many people that 'bars' are oriented horizontally and
    'columns' are oriented vertically since Microsoft Excel has been
    using these terms this way for many years. For this reason,
    `type = "bar"` will set `type = "col"` and `horizontal = TRUE`.

  - One of these additional types:

    - `"barpercent"` (short: `"bp"`), which is effectively a shortcut to
      set `type = "col"` and `horizontal = TRUE` and `x.max_items = 10`
      and `x.sort = "freq-desc"` and `datalabels.format = "%n (%p)"`.

    - `"dumbbell"` (short: `"d"` or `"db"`), which sets `type = "point"`
      and `horizontal = TRUE`, and adds a line between the points (using
      [`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)).
      The line colour cannot be changed. This plot type is only possible
      when the `category` has two distinct values.

    - `"linedot"` or `"line-point"` (short: `"ld"` or `"lp"`), which
      sets `type = "line"` and adds two point geoms using
      [`add_point()`](https://msberends.github.io/plot2/reference/add_type.md);
      one with large white dots and one with smaller dots using the
      colours set in `colour`. This is essentially equal to base R
      `plot(..., type = "b")` but with closed shapes.

    - `"back-to-back"` (short: `"b2b"`, or `"butterfly"`, `"tornado"` or
      `"population"`) creates a back-to-back plot, sometimes called a
      [Tornado diagram](https://en.wikipedia.org/wiki/Tornado_diagram),
      a butterfly plot, or a [Population
      Pyramid](https://en.wikipedia.org/wiki/Population_pyramid). It
      uses `facet` to distinguish the left and right plots. Therefore,
      `facet` must be set to (a column containing) two unique values.

    - `"upset"` or `"UpSet"` (short: `"u"`) creates an [UpSet
      plot](https://en.wikipedia.org/wiki/UpSet_plot), which requires
      `x` to contain multiple variables from `.data` that contain
      `0`/`1` or `FALSE`/`TRUE` values. It is also possible to provide
      `y`, on which then `summarise_function` will be applied.

    - `"sankey"` (short: `"s"`) creates a [Sankey
      plot](https://en.wikipedia.org/wiki/Sankey_diagram) using
      `category` for the flows and requires `x` to contain multiple
      variables from `.data`. At default, it also sets
      `x.expand = c(0.05, 0.05)` and `y.limits = c(NA, NA)` and
      `y.expand = c(0.01, 0.01)`. The so-called 'nodes' (the 'blocks'
      with text) are considered the datalabels, so you can set the text
      size and colour of the nodes using `datalabels.size`,
      `datalabels.colour`, and `datalabels.colour_fill`. The
      transparency of the flows can be set using `sankey_alpha`, and the
      width of the nodes can be set using `sankey_node_width`. Sankey
      plots can also be flipped using `horizontal = TRUE`, and the label
      angles can be set with `datalabels.angle`.

    - `"spider"` or `"radar"` (short: `"sp"`) creates a [Spider
      plot](https://en.wikipedia.org/wiki/Spider_chart) using our novel
      [`CoordSpider`](https://msberends.github.io/plot2/reference/coord_spider.md)
      coordinate system. Facets are supported, always with the same
      scale in all facets.

  - Left blank. In this case, the type will be determined automatically:
    `"boxplot"` if there is no X-axis or if the length of unique values
    per X-axis item is at least 3, `"point"` if both the y and x axes
    are numeric, and the [option](https://rdrr.io/r/base/options.html)
    `"plot2.default_type"` otherwise (which defaults to `"col"`, and
    accepts any value of `type`, such as `"line"` or `"l"`). Use
    `type = "blank"` or `type = "geom_blank"` to *not* add a geom.

- title, subtitle, caption, tag, x.title, y.title, category.title,
  legend.title, y_secondary.title:

  A title to use. This can be:

  - A [character](https://rdrr.io/r/base/character.html), which supports
    markdown by using
    [`md_to_expression()`](https://msberends.github.io/plot2/reference/md_to_expression.md)
    internally if `markdown = TRUE` (which is the default)

  - A [function](https://rdrr.io/r/base/function.html) to calculate over
    `.data`, such as
    `title = paste("Based on n =", n_distinct(person_id), "individuals")`
    or `subtitle = paste("Total rows:", n())`, see *Examples*

  - An [expression](https://rdrr.io/r/base/expression.html), e.g. using
    `parse(text = "...")`

  The `category.title` defaults to `TRUE` if the legend items are
  numeric. `category.title` and `legend.title` are the same setting; use
  one of them.

- title.linelength:

  Maximum number of characters per line in the title, before a linebreak
  occurs.

- title.colour:

  Text colour of the title.

- subtitle.linelength:

  Maximum number of characters per line in the subtitle, before a
  linebreak occurs.

- subtitle.colour:

  Text colour of the subtitle.

- na.replace:

  Character to put in place of `NA` values if `na.rm = FALSE`.

- na.rm:

  Remove `NA` values from showing in the plot.

- facet.position, facet.colour_fill, facet.bold, facet.italic,
  facet.size, facet.margin, facet.repeat_labels_x,
  facet.repeat_labels_y, facet.drop, facet.nrow, facet.relative:

  Additional settings for the plotting direction `facet`.
  `facet.repeat_labels_x` and `facet.repeat_labels_y` repeat the axis
  labels in every facet, which also makes the scales of that axis free
  (unless `facet.fixed_x` or `facet.fixed_y` is `TRUE`).

- facet.fixed_y:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  all y scales should have the same limits. If not set, this depends on
  the data: `TRUE` only if the coefficient of variation (standard
  deviation divided by mean) of the maximum y values per facet is less
  than 25%, otherwise every facet gets its own y scale. Both outcomes
  are reported in a message. Since this can change when the data change
  (e.g., next month's data), set `facet.fixed_y = TRUE` when panels must
  be compared.

- facet.fixed_x:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  all x scales should have the same breaks. This acts like the inverse
  of `x.drop`. If not set and x is numeric, the same rule as for
  `facet.fixed_y` applies to the maximum x values per facet.

- x.date_breaks:

  Breaks to use when the X-axis contains dates, will be determined
  automatically if left blank. This accepts values such as `"1 day"` and
  `"2 years"`.

- x.date_labels:

  Labels to use when the X-axis contains dates, will be determined
  automatically if left blank. This accepts 'Excel' date-language such
  as `"d mmmm yyyy"`: `d`/`dd` (day), `ddd`/`dddd` (weekday name), `mm`
  (month number), `mmm`/`mmmm` (month name), `yy`/`yyyy` (year),
  `q`/`qq` (quarter: `1` or `Q1`), `w`/`ww` (ISO week), `h`/`hh` (hour),
  `MM` (minutes), `ss` (seconds). Other words are kept as they are (e.g.
  `"Week ww"`), and text in single quotes is always kept literally (e.g.
  `"'d' d"`). A format containing `%` is used as a
  [`strftime()`](https://rdrr.io/r/base/strptime.html) format. Month and
  weekday names follow the [option](https://rdrr.io/r/base/options.html)
  `plot2.language` if set, and the system locale otherwise.

- x.date_remove_years:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the years of all `x` values must be unified. This will set the years
  of all `x` values [to 1970](https://en.wikipedia.org/wiki/Unix_time)
  if the data does not contain a leap year, and to 1972 otherwise. This
  allows to plot years on the `category` while maintaining a date range
  on `x`. The default is `FALSE`, unless `category` contains all years
  present in `x`.

- category.focus:

  A value of `category` that should be highlighted, meaning that all
  other values in `category` will be greyed out. This can also be a
  numeric value between 1 and the length of unique values of `category`,
  e.g. `category.focus = 2` to focus on the second legend item.

- colour:

  Colour(s) to set, will be evaluated with
  [`get_colour()`](https://msberends.github.io/plot2/reference/colour.md)
  if set. This can also be one of the viridis colours with automatic
  implementation for any plot: `"viridis"`, `"magma"`, `"inferno"`,
  `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. Also, this
  can be a named vector to match values of `category`, see *Examples*.
  Using a named vector can be used to manually sort the values of
  `category`.

- colour_fill:

  Colour(s) to be used for filling, will be determined automatically if
  left blank and will be evaluated with
  [`get_colour()`](https://msberends.github.io/plot2/reference/colour.md).

- colour_transparency:

  Amount of transparency for `colour`/`colour_fill` (0 = solid, 1 =
  fully transparent).

- x.label_angle:

  Angle to use for the X-axis in a counter-clockwise direction (i.e., a
  value of `90` will orient the axis labels from bottom to top, a value
  of `270` will orient the axis labels from top to bottom).

- x.label_align:

  Alignment for the X-axis between `0` (left aligned) and `1` (right
  aligned). If not set, rotated labels (see `x.label_angle`) are
  anchored at their ticks with
  [`guide_axis(angle = )`](https://ggplot2.tidyverse.org/reference/guide_axis.html),
  for every angle and also when `horizontal = TRUE`.

- x.label_italic:

  [logical](https://rdrr.io/r/base/logical.html) to indicate whether the
  x labels should be in *italics*.

- x.remove, y.remove:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the axis labels and title should be removed.

- x.position, y.position:

  Position of the axis.

- x.max_items, category.max_items, facet.max_items:

  Maximum number of items to show, including the item that holds the
  rest: `x.max_items = 5` shows four values and the rest, see
  `x.max_txt`. Defaults to infinite. Left `NULL` (default for
  `x.max_items`), this is infinite, except for `type = "barpercent"` and
  back-to-back plots (10). The remaining values are grouped into one
  item and summarised using the `summarise_function` function. **Please
  note:** the sorting will be applied first, so that e.g.
  `x.sort = "freq-desc"` with `x.max_items = 5` shows the four most
  frequent values of the X-axis and the rest.

- x.max_txt, category.max_txt, facet.max_txt:

  The text of the item that holds the values beyond `*.max_items`. The
  placeholder `%n` is replaced with the number of values in that item,
  and `%p` with their share of the total of `y` (or of the rows, when
  `y` is not numeric). For example, the default `"(rest, x%n)"` gives
  `"(rest, x4)"` for four values.

- x.breaks, y.breaks:

  A breaks function or numeric vector to use for the axis.

- x.n_breaks, y.n_breaks:

  Number of breaks, only useful if `x.breaks` or `y.breaks` respectively
  is `NULL`.

- x.transform, y.transform, category.transform:

  A transformation function to use, e.g. `"log2"`. This can be:
  `"asinh"`, `"asn"`, `"atanh"`, `"boxcox"`, `"compose"`, `"date"`,
  `"exp"`, `"hms"`, `"identity"`, `"log"`, `"log10"`, `"log1p"`,
  `"log2"`, `"logit"`, `"modulus"`, `"probability"`, `"probit"`,
  `"pseudo_log"`, `"reciprocal"`, `"reverse"`, `"sqrt"`, `"time"`,
  `"timespan"`, `"yj"`.

- x.expand, y.expand:

  [expansion](https://ggplot2.tidyverse.org/reference/expansion.html) to
  use for the axis, can be length 1 or 2. Left `NULL` (default), they
  are determined automatically: `x.expand` adds half a category on both
  sides of a discrete axis and a proportion of the range on a continuous
  axis, `y.expand` adds 25% above the data; both are 0 for sf objects or
  when the limits are set. An explicit value is used as given.

- x.limits, y.limits:

  Limits to use for the axis, must be length 2. Use `NA` for the highest
  or lowest value in the data, e.g. `y.limits = c(0, NA)` to have the y
  scale start at zero. For a discrete x axis, `x.limits` are the items
  to show, in this order.

- x.labels, y.labels, y_secondary.labels:

  A labels function or character vector to use for the axis.

- x.character:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the values of the X-axis should be forced to
  [character](https://rdrr.io/r/base/character.html). If not set, whole
  numbers that look like years (2000 to 2050) or months (exactly 1
  to 12) become character when `type` is not set or is a discrete type
  such as `"col"` or `"boxplot"`, with a message; for continuous types
  such as `"point"`, `"line"` and `"area"` they stay numeric. Numeric
  values also become character for other discrete types. Use
  `x.character = FALSE` to keep numeric values.

- x.drop:

  [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  factor levels should be dropped.

- x.zoom, y.zoom:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate if the
  axis should be zoomed on the data, i.e., cover the range of the data
  instead of starting at 0. Left `NULL` (default for `x.zoom`), the x
  axis is zoomed for plot types with a continuous x axis (such as
  histograms) only. This sets `x.limits = c(NA, NA)` (or `y.limits`)
  and, unless `x.expand` (or `y.expand`) is set, a 5% expansion on both
  sides, like `ggplot2` does. It has no effect if `x.limits` (or
  `y.limits`) is set.

- y.24h:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the y labels and breaks should be formatted as 24-hour sequences.

- y.age:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the y labels and breaks should be formatted as ages in years.

- y.scientific, y_secondary.scientific:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the y labels should be formatted in scientific notation. If not set,
  scientific notation is used only for labels of one billion (`1e9`) or
  more, or for non-zero labels below `0.0001`; other labels use
  `big.mark`, e.g. `2 000 000`.

- y.percent, y_secondary.percent:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the y labels should be formatted as percentages. Left `NULL` (default
  for `y.percent`), percentages are used for density plots only.

- y.percent_break:

  A value on which the Y-axis should have breaks if `y.percent = TRUE`.
  Left `NULL` (default), this is 0.1 (10%), widened to keep about 10
  labels and narrowed if that would give three or fewer labels. An
  explicit value is used as given.

- y_secondary:

  Values to use for plotting along the secondary Y-axis. This
  functionality is poorly supported by `ggplot2` and might give
  unexpected results. Setting the secondary Y-axis will set the colour
  to the axis titles.

- y_secondary.type:

  Type of the secondary series, see **`type`**. The default (`NULL`) is
  a line with points if the primary type is a column or bar plot, so
  that the secondary series does not cover the bars, and the primary
  type otherwise.

- y_secondary.colour, y_secondary.colour_fill:

  Colours to set for the secondary Y-axis, will be evaluated with
  [`get_colour()`](https://msberends.github.io/plot2/reference/colour.md).
  Left `NULL` (default), the second colour of the `plot2.colour` option
  is used.

- category.type:

  Type of the `category`, one or more of: `"colour"` (default),
  `"shape"`, `"size"`, `"linetype"`, `"linewidth"`, `"alpha"`. There is
  no need to set `"fill"`, as `plot2` handles colour-setting internally
  and determines automatically whether the `colour` or `fill` aesthetic
  must be used.

- category.labels, category.percent, category.breaks, category.expand,
  category.midpoint:

  Settings for the plotting direction `category`.

- category.limits:

  Limits to use for a numeric category, must be length 2. Use `NA` for
  the highest or lowest value in the data, e.g.
  `category.limits = c(0, NA)` to have the scale start at zero. If not
  set, the limits are the range of the data, rounded outwards to pretty
  values with [`pretty()`](https://rdrr.io/r/base/pretty.html) (e.g.,
  values from 12 to 87 give limits 0 to 100). With three colours and
  `category.midpoint` set, the limits are made symmetric around the
  midpoint.

- category.date_breaks:

  Breaks to use when the category contains dates, will be determined
  automatically if left blank. This will be passed on to
  [`seq.Date(by = ...)`](https://rdrr.io/r/base/seq.Date.html) and thus
  can be: a number, taken to be in days, or a character string
  containing one of "day", "week", "month", "quarter" or "year"
  (optionally preceded by an integer and a space, and/or followed by
  "s").

- category.date_labels:

  Labels to use when the category contains dates, will be determined
  automatically if left blank. This accepts 'Excel' date-language such
  as `"d mmmm yyyy"`.

- category.character:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the values of the category should be forced to
  [character](https://rdrr.io/r/base/character.html). If not set, whole
  numbers that look like years (2000 to 2050) or months (exactly 1
  to 12) become character, with a message. Use
  `category.character = FALSE` to keep numeric values.

- x.sort, category.sort, facet.sort:

  Sorting of the plotting direction. `category.sort` and `facet.sort`
  default to `TRUE`; `x.sort` defaults to `NULL`, which sorts text and
  factors as `TRUE` and leaves continuous values (such as dates and
  numbers) unsorted. Applying one of the sorting methods will transform
  the values to an ordered [factor](https://rdrr.io/r/base/factor.html),
  which `ggplot2` uses to orient the data. Valid values are:

  - A manual vector of values

  - `TRUE`: sort [factor](https://rdrr.io/r/base/factor.html)s on their
    levels, otherwise sort ascending on alphabet, while maintaining
    numbers in the text (*numeric* sort)

  - `FALSE`: sort according to the order in the data

  - `NULL`: sort automatically, as described above

  - `"asc"`, `"alpha"` or `"alpha-asc"`: sort as `TRUE`

  - `"desc"` or `"alpha-desc"`: sort
    [factor](https://rdrr.io/r/base/factor.html)s on their
    [reversed](https://rdrr.io/r/base/rev.html) levels, otherwise sort
    descending on alphabet, while maintaining numbers in the text
    (*numeric* sort)

  - `"order"` or `"inorder"`: sort as `FALSE`

  - `"freq"` or `"freq-desc"`: sort descending according to the
    frequencies of `y` computed by `summarise_function` (highest value
    first)

  - `"freq-asc"`: sort ascending according to the frequencies of `y`
    computed by `summarise_function` (lowest value first)

- x.complete, category.complete, facet.complete:

  A value to complete the data. This makes use of
  [`tidyr::full_seq()`](https://tidyr.tidyverse.org/reference/full_seq.html)
  and
  [`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html).
  For example, using `x.complete = 0` will apply
  `data |> complete(x = full_seq(x, ...), fill = list(y = 0))`, crossed
  with the values of `category` and `facet` if set. Numeric values and
  dates are completed with
  [`tidyr::full_seq()`](https://tidyr.tidyverse.org/reference/full_seq.html)
  at their observed resolution, date-times at their observed interval,
  and character values and factors with their unique values or levels.
  Using value `TRUE` (e.g., `x.complete = TRUE`) is identical to using
  value `0`.

- datalabels:

  Values to show as datalabels, see also `datalabels.format`. This can
  be:

  - `NULL` (default): automatic. This shows the values of `y` in
    column-type plots, or when plotting spatial 'sf' data, the values of
    the first column, with a maximum of 25 labels and none for
    continuous plot types such as lines and points.

  - `TRUE` to always show the values of `y`, without the maximum of 25
    labels, or `FALSE` to remove datalabels

  - A function to calculate over `.data`, such as
    `datalabels = paste(round(column1), "\n", column2)`

- datalabels.round:

  Number of digits to round the datalabels, applies to both `"%n"` and
  `"%p"` for replacement (see `datalabels.format`).

- datalabels.format:

  Format to use for datalabels. This can be a function (such as
  [`euros()`](https://msberends.github.io/plot2/reference/labellers.md))
  or a text. For the text, `"%n"` will be replaced by the count number,
  and `"%p"` will be replaced by the percentage (see
  `datalabels.percent_by`). Use `datalabels.format = NULL` to *not*
  transform the datalabels.

- datalabels.colour, datalabels.colour_fill, datalabels.size,
  datalabels.angle, datalabels.lineheight:

  Settings for the datalabels. Rotated datalabels (`datalabels.angle`)
  start or end at their bar with a small fixed gap; if `y.expand` and
  `y.limits` are not set, the value axis gets extra room for long
  rotated labels, estimated for a figure of about 6 by 4 inches.

- datalabels.percent_by:

  Group within which the `"%p"` percentages of `datalabels.format` sum
  to 100%. This can be:

  - `"x"` (default): within each x value and facet, so that the stacked
    or dodged bars of one x value sum to 100%. Without a `category`,
    each x value has a single bar, so this is the same as `"facet"`.

  - `"category"`: within each category and facet

  - `"facet"`: within each facet

  - `"total"`: over all data in the plot

- decimal.mark:

  Decimal mark, defaults to
  [`dec_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md).

- big.mark:

  Thousands separator, defaults to
  [`big_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md).

- summarise_function:

  A [function](https://rdrr.io/r/base/function.html) to use if the data
  has to be summarised, see *Examples*. Left `NULL` (default), this is
  [`sum()`](https://rdrr.io/r/base/sum.html), except when `y` contains
  several columns (then the values are not summarised). Use `FALSE` to
  not summarise the data at all.

- stacked:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate that
  values must be
  [stacked](https://ggplot2.tidyverse.org/reference/position_stack.html),
  or `"fill"` to stack them to 100% (the same as `stacked_fill = TRUE`).

- stacked_fill:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate that
  values must be
  [filled](https://ggplot2.tidyverse.org/reference/position_stack.html)
  (i.e., stacked to 100%). If `TRUE`, this takes precedence over
  `stacked`.

- horizontal:

  A [logical](https://rdrr.io/r/base/logical.html) to turn the plot 90
  degrees using
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).
  This option also updates some theme options, so that e.g.,
  `x.label_italic` will still apply to the original X-axis.

- reverse:

  A [logical](https://rdrr.io/r/base/logical.html) to reverse the
  *values* of `category`. Use `legend.reverse` to reverse the *legend*
  of `category`.

- smooth:

  A [logical](https://rdrr.io/r/base/logical.html) to add a smooth. In
  histograms, this will add the density count as an overlaying line
  (default: `TRUE`). In all other cases, a smooth will be added using
  [`geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)
  (default: `FALSE`).

- smooth.method, smooth.formula, smooth.se, smooth.level, smooth.alpha,
  smooth.linewidth, smooth.linetype, smooth.colour:

  Settings for `smooth`.

- size:

  Size of the geom. Defaults to `2` for geoms
  [point](https://ggplot2.tidyverse.org/reference/geom_point.html),
  [jitter](https://ggplot2.tidyverse.org/reference/geom_jitter.html),
  and [beeswarm](https://rdrr.io/pkg/ggbeeswarm/man/geom_beeswarm.html);
  `4` for UpSet plots (using `type = "upset"`); `5` for dumbbell plots
  (using `type = "dumbbell"`); `0.75` otherwise.

- linetype:

  Linetype of the geom, only suitable for geoms that draw lines.
  Possible options are:

  1.  `"solid"` (default)

  2.  `"dashed"`

  3.  `"dotted"`

  4.  `"dotdash"`

  5.  `"longdash"`

  6.  `"twodash"`

  Both numbers and text are supported, e.g., `linetype = 2` and
  `linetype = "dashed"` yield the same results.

- linewidth:

  Linewidth of the geom, only suitable for geoms that draw lines.
  Defaults to:

  - `0.10` for [sf](https://ggplot2.tidyverse.org/reference/ggsf.html)

  - `0.25` for geoms that are continuous and have fills (such as
    [area](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)),
    except for geoms
    [boxplot](https://ggplot2.tidyverse.org/reference/geom_boxplot.html)
    and
    [violin](https://ggplot2.tidyverse.org/reference/geom_violin.html)

  - `1.0` for dumbbell plots (using `type = "dumbbell"`) and UpSet plots
    (using `type = "upset"`)

  - `0.5` otherwise (such as geoms
    [line](https://ggplot2.tidyverse.org/reference/geom_path.html),
    [boxplot](https://ggplot2.tidyverse.org/reference/geom_boxplot.html),
    [violin](https://ggplot2.tidyverse.org/reference/geom_violin.html),
    and
    [histogram](https://ggplot2.tidyverse.org/reference/geom_histogram.html))

- binwidth:

  Width of bins (only useful for `type = "histogram"`), can be specified
  as a numeric value or as a function that calculates width from `x`,
  see
  [`geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).
  It defaults to approx. `diff(range(x))` divided by 12 to 22 based on
  the data.

- width:

  Width of the geom. Defaults to `0.66` for geoms
  [boxplot](https://ggplot2.tidyverse.org/reference/geom_boxplot.html),
  [violin](https://ggplot2.tidyverse.org/reference/geom_violin.html) and
  [jitter](https://ggplot2.tidyverse.org/reference/geom_jitter.html),
  and to `0.5` otherwise.

- jitter_seed:

  Seed (randomisation factor) to be set when using `type = "jitter"`.

- violin_scale:

  Scale to be set when using `type = "violin"`, can also be set to
  `"area"`.

- legend:

  A [logical](https://rdrr.io/r/base/logical.html) to show (`TRUE`) or
  hide (`FALSE`) the legend, i.e. the box explaining the values of
  `category`. The default `NULL` shows it when the plot has a
  `category`.

- legend.position:

  Position of the legend. This can be:

  - `"top"`, `"right"`, `"bottom"` or `"left"`, can be abbreviated

  - `"inside"`, to place the legend inside the plot area at ggplot2's
    default position

  - Two numbers between 0 and 1, to place the legend inside the plot
    area at these relative coordinates, e.g. `c(0.95, 0.95)` for the top
    right

  - `"none"`, `FALSE` or `NA` to remove the legend

  The default `NULL` sets it automatically: `"right"` for numeric
  `category` values and 'sf' plots, and `"top"` otherwise.

- legend.reverse, legend.barheight, legend.barwidth, legend.nbin,
  legend.italic, legend.nrow:

  Other settings for the legend.

- sankey_node_width:

  Width of the vertical nodes in a Sankey plot.

- sankey_node_whitespace:

  Whitespace between the nodes in a Sankey plot.

- sankey_alpha:

  Alpha of the flows in a Sankey plot.

- sankey_remove_axes:

  Logical to indicate whether all axes must be removed in a Sankey plot.

- zoom:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate if the
  plot should be scaled to the data, i.e., not having the x and y axes
  to start at 0. This is identical to setting `x.zoom = TRUE` and
  `y.zoom = TRUE`.

- sep:

  Separator character to use if multiple columns are given to either of
  the three directions: `x`, `category` and `facet`, e.g.
  `facet = c(column1, column2)`.

- print:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate if the
  result should be [printed](https://rdrr.io/r/base/print.html) instead
  of just returned.

- text_factor:

  Text factor to use, which will apply to all texts shown in the plot.

- font:

  Font (family) to use, can be set with `options(plot2.font = "...")`.
  Can be any installed system font or any of the \> 1400 font names from
  [Google Fonts](https://fonts.google.com). Custom fonts require the
  `showtext`, `showtextdb` and `sysfonts` packages. A Google Font that
  is not installed is downloaded once into the user cache folder of
  plot2
  ([`tools::R_user_dir("plot2", "cache")`](https://rdrr.io/r/tools/userdir.html));
  fonts that have not been used for 90 days are removed from it
  automatically. The font is applied only while the plot is drawn, so
  other plots in the session are not affected. When using custom fonts
  in R Markdown, be sure to set the chunk option `fig.showtext = TRUE`,
  otherwise an informative error will be generated.

- theme:

  A valid `ggplot2`
  [theme](https://ggplot2.tidyverse.org/reference/theme.html) to apply,
  or `NULL` to use the default
  [`theme_grey()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).
  This argument accepts themes (e.g.,
  [`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)),
  functions (e.g., `theme_bw`) and characters themes (e.g.,
  `"theme_bw"`). A partial theme, such as
  `theme(panel.background = element_blank())`, is added on top of
  [`theme_minimal2()`](https://msberends.github.io/plot2/reference/theme_minimal2.md),
  and its elements take precedence over plot2's own settings. The
  default is
  [`theme_minimal2()`](https://msberends.github.io/plot2/reference/theme_minimal2.md),
  but can be set with `options(plot2.theme = "...")`.

- background:

  The background colour of the entire plot, can also be `NA` to remove
  it. Will be evaluated with
  [`get_colour()`](https://msberends.github.io/plot2/reference/colour.md).
  Only applies when `theme` is not `NULL`.

- markdown:

  A [logical](https://rdrr.io/r/base/logical.html) to turn all labels
  and titles into [plotmath](https://rdrr.io/r/grDevices/plotmath.html)
  expressions, by converting common markdown language using the
  [`md_to_expression()`](https://msberends.github.io/plot2/reference/md_to_expression.md)
  function (defaults to `TRUE`).

- ...:

  Any argument to give to the geom. This will override automatically-set
  settings for the geom.

- crs:

  The coordinate reference system (CRS) to use. If this is not left
  blank,
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)
  will be used to transform the geometric data to the new CRS.

- datalabels.centroid:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  datalabels must be centred on the polygon (using
  [`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html),
  the default), or be placed on the 'best' spot on the surface (using
  [`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html)).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

The methods differ from the data frame method as follows:

- **Vectors and other objects** (`plot2.default()`): the input is
  converted to a data frame first. Character and factor values are
  counted. A single character value that is the name of a data frame is
  replaced by that data frame.

- **Formulas** (`plot2.formula()`): `y ~ x` takes the variables from
  `data`, e.g. `plot2(Sepal.Length ~ Species, data = iris)`.

- **Frequency tables** (`plot2.freq()`, from
  [`cleaner::freq()`](https://msberends.github.io/cleaner/reference/freq.html)):
  `x` and `y` default to the items and their counts, the axis titles to
  "Item" and "Count", and `x.sort` to `"freq-desc"`.

- **Spatial data** (`plot2.sf()`, from the `sf` package): plots a map,
  without axis titles, axis expansion, grid or axes; the borders use the
  `plot2.colour_sf` option (default `"grey50"`), the fill uses
  `plot2.colour_sf_fill`, and the legend is on the right. The extra
  arguments `crs` and `datalabels.centroid` apply only here.

- **Matrices** (`plot2.matrix()`): the row names (or row numbers) become
  `x` and every column becomes a `category`. A matrix with identical row
  and column names, such as the outcome of
  [`cor()`](https://rdrr.io/r/stats/cor.html), is plotted as a heatmap
  (`type = "tile"`). The axis titles are left out, unless the dimensions
  are named (e.g. `dimnames = list(region = ..., year = ...)`); these
  names are then used as titles of x and of the category.

For geographic information system (GIS) analysis, use the `sf` package
with a data set containing geometries. The result can be used as input
for [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md).

## Examples

``` r
# vectors: character values are counted
plot2(c("a", "b", "b", "c", "c", "c"))
#> Using y = y
#> Using x = x


# formulas
plot2(Sepal.Length ~ Species, data = iris)
#> Using type = "boxplot" since all groups in Species contain at least three values


# frequency tables
if (requireNamespace("cleaner", quietly = TRUE)) {
  plot2(cleaner::freq(iris$Species))
}


# correlation matrices become a heatmap
plot2(cor(mtcars[, 1:5]))
#> Assuming type = "tile" since the matrix contains identical row and column names


# spatial data
if (requireNamespace("sf", quietly = TRUE)) {
  plot2(netherlands)
}
#> Using category = area_km2
#> Using datalabels = province
#> Assuming datalabels.centroid = TRUE. Set to FALSE for a point-on-surface placing of datalabels.
```

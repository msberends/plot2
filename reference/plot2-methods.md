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
  facet.fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_lbls_x = TRUE,
  facet.repeat_lbls_y = NULL,
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
  colour_opacity = 0,
  x.lbl_angle = 0,
  x.lbl_align = NULL,
  x.lbl_italic = FALSE,
  x.lbl_taxonomy = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = Inf,
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
  x.mic = FALSE,
  x.zoom = FALSE,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = FALSE,
  y.percent_break = 0.1,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = type,
  y_secondary.title = TRUE,
  y_secondary.colour = get_colour(getOption("plot2.colour", "ggplot2"), 2),
  y_secondary.colour_fill = get_colour(getOption("plot2.colour", "ggplot2"), 2),
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
  datalabels = TRUE,
  datalabels.round = ifelse(y.percent, 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = base::sum,
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
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey.node_width = 0.15,
  sankey.node_whitespace = 0.03,
  sankey.alpha = 0.5,
  sankey.remove_axes = NULL,
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
  facet.fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_lbls_x = TRUE,
  facet.repeat_lbls_y = NULL,
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
  colour_opacity = 0,
  x.lbl_angle = 0,
  x.lbl_align = NULL,
  x.lbl_italic = FALSE,
  x.lbl_taxonomy = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = Inf,
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
  x.mic = FALSE,
  x.zoom = FALSE,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = FALSE,
  y.percent_break = 0.1,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = type,
  y_secondary.title = TRUE,
  y_secondary.colour = get_colour(getOption("plot2.colour", "ggplot2"), 2),
  y_secondary.colour_fill = get_colour(getOption("plot2.colour", "ggplot2"), 2),
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
  datalabels = TRUE,
  datalabels.round = ifelse(y.percent, 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = base::sum,
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
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey.node_width = 0.15,
  sankey.node_whitespace = 0.03,
  sankey.alpha = 0.5,
  sankey.remove_axes = NULL,
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
  facet.fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_lbls_x = TRUE,
  facet.repeat_lbls_y = NULL,
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
  colour_opacity = 0,
  x.lbl_angle = 0,
  x.lbl_align = NULL,
  x.lbl_italic = FALSE,
  x.lbl_taxonomy = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = Inf,
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
  x.mic = FALSE,
  x.zoom = FALSE,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = FALSE,
  y.percent_break = 0.1,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = type,
  y_secondary.title = TRUE,
  y_secondary.colour = get_colour(getOption("plot2.colour", "ggplot2"), 2),
  y_secondary.colour_fill = get_colour(getOption("plot2.colour", "ggplot2"), 2),
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
  datalabels = TRUE,
  datalabels.round = ifelse(y.percent, 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = base::sum,
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
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey.node_width = 0.15,
  sankey.node_whitespace = 0.03,
  sankey.alpha = 0.5,
  sankey.remove_axes = NULL,
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
  facet.fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_lbls_x = TRUE,
  facet.repeat_lbls_y = NULL,
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
  colour_opacity = 0,
  x.lbl_angle = 0,
  x.lbl_align = NULL,
  x.lbl_italic = FALSE,
  x.lbl_taxonomy = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = Inf,
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
  x.mic = FALSE,
  x.zoom = FALSE,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = FALSE,
  y.percent_break = 0.1,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = 0,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = type,
  y_secondary.title = TRUE,
  y_secondary.colour = get_colour(getOption("plot2.colour", "ggplot2"), 2),
  y_secondary.colour_fill = get_colour(getOption("plot2.colour", "ggplot2"), 2),
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
  datalabels = TRUE,
  datalabels.round = ifelse(y.percent, 2, 1),
  datalabels.format = NULL,
  datalabels.colour = "black",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = base::sum,
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
  legend.position = "right",
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey.node_width = 0.15,
  sankey.node_whitespace = 0.03,
  sankey.alpha = 0.5,
  sankey.remove_axes = NULL,
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
  data = NULL,
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
  facet.fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_lbls_x = TRUE,
  facet.repeat_lbls_y = NULL,
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
  colour_opacity = 0,
  x.lbl_angle = 0,
  x.lbl_align = NULL,
  x.lbl_italic = FALSE,
  x.lbl_taxonomy = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = Inf,
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
  x.mic = FALSE,
  x.zoom = FALSE,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = FALSE,
  y.percent_break = 0.1,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = type,
  y_secondary.title = TRUE,
  y_secondary.colour = get_colour(getOption("plot2.colour", "ggplot2"), 2),
  y_secondary.colour_fill = get_colour(getOption("plot2.colour", "ggplot2"), 2),
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
  datalabels = TRUE,
  datalabels.round = ifelse(y.percent, 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = base::sum,
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
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey.node_width = 0.15,
  sankey.node_whitespace = 0.03,
  sankey.alpha = 0.5,
  sankey.remove_axes = NULL,
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
  facet.fill = NULL,
  facet.bold = TRUE,
  facet.italic = FALSE,
  facet.size = 10,
  facet.margin = 8,
  facet.repeat_lbls_x = TRUE,
  facet.repeat_lbls_y = NULL,
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
  colour_opacity = 0,
  x.lbl_angle = 0,
  x.lbl_align = NULL,
  x.lbl_italic = FALSE,
  x.lbl_taxonomy = FALSE,
  x.remove = FALSE,
  x.position = "bottom",
  x.max_items = Inf,
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
  x.mic = FALSE,
  x.zoom = FALSE,
  y.remove = FALSE,
  y.24h = FALSE,
  y.age = FALSE,
  y.scientific = NULL,
  y.percent = FALSE,
  y.percent_break = 0.1,
  y.breaks = NULL,
  y.n_breaks = NULL,
  y.limits = NULL,
  y.labels = NULL,
  y.expand = NULL,
  y.transform = "identity",
  y.position = "left",
  y.zoom = FALSE,
  y_secondary = NULL,
  y_secondary.type = type,
  y_secondary.title = TRUE,
  y_secondary.colour = get_colour(getOption("plot2.colour", "ggplot2"), 2),
  y_secondary.colour_fill = get_colour(getOption("plot2.colour", "ggplot2"), 2),
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
  datalabels = TRUE,
  datalabels.round = ifelse(y.percent, 2, 1),
  datalabels.format = "%n",
  datalabels.colour = "grey25",
  datalabels.colour_fill = NULL,
  datalabels.size = (3 * text_factor),
  datalabels.angle = 0,
  datalabels.lineheight = 1,
  decimal.mark = dec_mark(),
  big.mark = big_mark(),
  summarise_function = base::sum,
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
  legend.position = NULL,
  legend.title = NULL,
  legend.reverse = FALSE,
  legend.nrow = NULL,
  legend.barheight = 6,
  legend.barwidth = 1.5,
  legend.nbin = 300,
  legend.italic = FALSE,
  sankey.node_width = 0.15,
  sankey.node_whitespace = 0.03,
  sankey.alpha = 0.5,
  sankey.remove_axes = NULL,
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

  Data to plot.

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
    (`"b"`), beeswarm (`"bs"`), column (`"c"`), histogram (`"h"`),
    jitter (`"j"`), line (`"l"`), point (`"p"`), ribbon (`"r"`), and
    violin (`"v"`).

    Please note: in `ggplot2`, 'bars' and 'columns' are equal, while it
    is common to many people that 'bars' are oriented horizontally and
    'columns' are oriented vertically since Microsoft Excel has been
    using these terms this way for many years. For this reason,
    `type = "bar"` will set `type = "col"` and `horizontal = TRUE`.

  - One of these additional types:

    - `"barpercent"` (short: `"bp"`), which is effectively a shortcut to
      set `type = "col"` and `horizontal = TRUE` and `x.max_items = 10`
      and `x.sort = "freq-desc"` and `datalabels.format = "%n (%p)"`.

    - `"dumbbell"` (short: `"d"`), which sets `type = "point"` and
      `horizontal = TRUE`, and adds a line between the points (using
      [`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)).
      The line colour cannot be changed. This plot type is only possible
      when the `category` has two distinct values.

    - `"linedot"` (short: `"ld"`), which sets `type = "line"` and adds
      two point geoms using
      [`add_point()`](https://msberends.github.io/plot2/reference/add_type.md);
      one with large white dots and one with smaller dots using the
      colours set in `colour`. This is essentially equal to base R
      `plot(..., type = "b")` but with closed shapes.

    - `"back-to-back"` (short: `"b2b"`) creates a back-to-back plot,
      sometimes called a [Tornado
      diagram](https://en.wikipedia.org/wiki/Tornado_diagram), a
      [Butterfly plot](https://en.wikipedia.org/wiki/Tornado_diagram),
      or a [Population
      Pyramid](https://en.wikipedia.org/wiki/Population_pyramid). It
      uses `facet` to distinquish the left and right plots. Therefore,
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
      transparency of the flows can be set using `sankey.alpha`, and the
      width of the nodes can be set using `sankey.node_width`. Sankey
      plots can also be flipped using `horizontal = TRUE`.

  - Left blank. In this case, the type will be determined automatically:
    `"boxplot"` if there is no X-axis or if the length of unique values
    per X-axis item is at least 3, `"point"` if both the y and x axes
    are numeric, and the [option](https://rdrr.io/r/base/options.html)
    `"plot2.default_type"` otherwise (which defaults to `"col"`). Use
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
  numeric.

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

- facet.position, facet.fill, facet.bold, facet.italic, facet.size,
  facet.margin, facet.repeat_lbls_x, facet.repeat_lbls_y, facet.drop,
  facet.nrow, facet.relative:

  Additional settings for the plotting direction `facet`.

- facet.fixed_y:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  all y scales should have the same limits. Defaults to `TRUE` only if
  the coefficient of variation (standard deviation divided by mean) of
  the maximum values of y is less than 25%.

- facet.fixed_x:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  all x scales should have the same breaks. This acts like the inverse
  of `x.drop`.

- x.date_breaks:

  Breaks to use when the X-axis contains dates, will be determined
  automatically if left blank. This accepts values such as `"1 day"` and
  `"2 years"`.

- x.date_labels:

  Labels to use when the X-axis contains dates, will be determined
  automatically if left blank. This accepts 'Excel' date-language such
  as `"d mmmm yyyy"`.

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

  Get_colour(s) to set, will be evaluated with
  [`get_colour()`](https://msberends.github.io/plot2/reference/colour.md)
  if set. This can also be one of the viridis colours with automatic
  implementation for any plot: `"viridis"`, `"magma"`, `"inferno"`,
  `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. Also, this
  can be a named vector to match values of `category`, see *Examples*.
  Using a named vector can be used to manually sort the values of
  `category`.

- colour_fill:

  Get_colour(s) to be used for filling, will be determined automatically
  if left blank and will be evaluated with
  [`get_colour()`](https://msberends.github.io/plot2/reference/colour.md).

- colour_opacity:

  Amount of opacity for `colour`/`colour_fill` (0 = solid, 1 =
  transparent).

- x.lbl_angle:

  Angle to use for the X-axis in a counter-clockwise direction (i.e., a
  value of `90` will orient the axis labels from bottom to top, a value
  of `270` will orient the axis labels from top to bottom).

- x.lbl_align:

  Alignment for the X-axis between `0` (left aligned) and `1` (right
  aligned).

- x.lbl_italic:

  [logical](https://rdrr.io/r/base/logical.html) to indicate whether the
  x labels should in in *italics*.

- x.lbl_taxonomy:

  A [logical](https://rdrr.io/r/base/logical.html) to transform all
  words of the `x` labels into italics that are in the
  [microorganisms](https://amr-for-r.org/reference/microorganisms.html)
  data set of the `AMR` package. This uses
  [`md_to_expression()`](https://msberends.github.io/plot2/reference/md_to_expression.md)
  internally and will set `x.labels` to parse expressions.

- x.remove, y.remove:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the axis labels and title should be removed.

- x.position, y.position:

  Position of the axis.

- x.max_items, category.max_items, facet.max_items:

  Number of maximum items to use, defaults to infinite. All other values
  will be grouped and summarised using the `summarise_function`
  function. **Please note:** the sorting will be applied first, allowing
  to e.g. plot the top *n* most frequent values of the X-axis by
  combining `x.sort = "freq-desc"` with `x.max_items =` *n*.

- x.max_txt, category.max_txt, facet.max_txt:

  The text to use of values not included number of `*.max_items`. The
  placeholder `%n` will be replaced with the outcome of the
  `summarise_function` function, the placeholder `%p` will be replaced
  with the percentage.

- x.breaks, y.breaks:

  A breaks function or numeric vector to use for the axis.

- x.n_breaks, y.n_breaks:

  Number of breaks, only useful if `x.breaks` cq. `y.breaks` is `NULL`.

- x.transform, y.transform, category.transform:

  A transformation function to use, e.g. `"log2"`. This can be:
  `"asinh"`, `"asn"`, `"atanh"`, `"boxcox"`, `"compose"`, `"date"`,
  `"exp"`, `"hms"`, `"identity"`, `"log"`, `"log10"`, `"log1p"`,
  `"log2"`, `"logit"`, `"modulus"`, `"probability"`, `"probit"`,
  `"pseudo_log"`, `"reciprocal"`, `"reverse"`, `"sqrt"`, `"time"`,
  `"timespan"`, `"yj"`.

- x.expand, y.expand:

  [expansion](https://ggplot2.tidyverse.org/reference/expansion.html) to
  use for the axis, can be length 1 or 2. `x.expand` defaults to 0.5 and
  `y.expand` defaults to `0.25`, except for sf objects (then both
  default to 0).

- x.limits, y.limits:

  Limits to use for the axis, can be length 1 or 2. Use `NA` for the
  highest or lowest value in the data, e.g. `y.limits = c(0, NA)` to
  have the y scale start at zero.

- x.labels, y.labels, y_secondary.labels:

  A labels function or character vector to use for the axis.

- x.character:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the values of the X-axis should be forced to
  [character](https://rdrr.io/r/base/character.html). The default is
  `FALSE`, except for years (values between 2000 and 2050) and months
  (values from 1 to 12).

- x.drop:

  [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  factor levels should be dropped.

- x.mic:

  [logical](https://rdrr.io/r/base/logical.html) to indicate whether the
  X-axis should be formatted as [MIC
  values](https://amr-for-r.org/reference/as.mic.html), by dropping all
  factor levels and adding missing factors of 2.

- x.zoom, y.zoom:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate if the
  axis should be zoomed on the data, by setting `x.limits = c(NA, NA)`
  and `x.expand = 0` for the X-axis, or `y.limits = c(NA, NA)` and
  `y.expand = 0` for the Y-axis.

- y.24h:

  a [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the y labels and breaks should be formatted as 24-hour sequences

- y.age:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the y labels and breaks should be formatted as ages in years.

- y.scientific, y_secondary.scientific:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the y labels should be formatted in scientific notation. Defaults to
  `TRUE` only if the range of the y values spans more than `10e5`.

- y.percent, y_secondary.percent:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the y labels should be formatted as percentages.

- y.percent_break:

  A value on which the Y-axis should have breaks.

- y_secondary:

  Values to use for plotting along the secondary Y-axis. This
  functionality is poorly supported by `ggplot2` and might give
  unexpected results. Setting the secondary Y-axis will set the colour
  to the axis titles.

- y_secondary.type:

  see **`type`**

- y_secondary.colour, y_secondary.colour_fill:

  Colours to set for the secondary Y-axis, will be evaluated with
  [`get_colour()`](https://msberends.github.io/plot2/reference/colour.md).

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

  Limits to use for a numeric category, can be length 1 or 2. Use `NA`
  for the highest or lowest value in the data, e.g.
  `category.limits = c(0, NA)` to have the scale start at zero.

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
  [character](https://rdrr.io/r/base/character.html). The default is
  `FALSE`, except for years (values between 2000 and 2050) and months
  (values from 1 to 12).

- x.sort, category.sort, facet.sort:

  Sorting of the plotting direction, defaults to `TRUE`, except for
  continuous values on the X-axis (such as dates and numbers). Applying
  one of the sorting methods will transform the values to an ordered
  [factor](https://rdrr.io/r/base/factor.html), which `ggplot2` uses to
  orient the data. Valid values are:

  - A manual vector of values

  - `TRUE`: sort [factor](https://rdrr.io/r/base/factor.html)s on their
    levels, otherwise sort ascending on alphabet, while maintaining
    numbers in the text (*numeric* sort)

  - `FALSE`: sort according to the order in the data

  - `NULL`: do not sort/transform at all

  - `"asc"` or `"alpha"`: sort as `TRUE`

  - `"desc"`: sort [factor](https://rdrr.io/r/base/factor.html)s on
    their [reversed](https://rdrr.io/r/base/rev.html) levels, otherwise
    sort descending on alphabet, while maintaining numbers in the text
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
  `data |> complete(full_seq(x, ...), fill = list(x = 0))`. Using value
  `TRUE` (e.g., `x.complete = TRUE`) is identical to using value `0`.

- datalabels:

  Values to show as datalabels, see also `datalabels.format`. This can
  be:

  - Left blank. This will default to the values of `y` in column-type
    plots, or when plotting spatial 'sf' data, the values of the first
    column. It will print a maximum of 25 labels unless
    `datalabels = TRUE`.

  - `TRUE` or `FALSE` to force or remove datalabels

  - A function to calculate over `.data`, such as
    `datalabels = paste(round(column1), "\n", column2)`

- datalabels.round:

  Number of digits to round the datalabels, applies to both `"%n"` and
  `"%p"` for replacement (see `datalabels.format`).

- datalabels.format:

  Format to use for datalabels. This can be a function (such as
  [`euros()`](https://msberends.github.io/plot2/reference/labellers.md))
  or a text. For the text, `"%n"` will be replaced by the count number,
  and `"%p"` will be replaced by the percentage of the total count. Use
  `datalabels.format = NULL` to *not* transform the datalabels.

- datalabels.colour, datalabels.colour_fill, datalabels.size,
  datalabels.angle, datalabels.lineheight:

  Settings for the datalabels.

- decimal.mark:

  Decimal mark, defaults to
  [`dec_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md).

- big.mark:

  Thousands separator, defaults to
  [`big_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md).

- summarise_function:

  A [function](https://rdrr.io/r/base/function.html) to use if the data
  has to be summarised, see *Examples*. This can also be `NULL`, which
  will be converted to `function(x) x`.

- stacked:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate that
  values must be
  [stacked](https://ggplot2.tidyverse.org/reference/position_stack.html).

- stacked_fill:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate that
  values must be
  [filled](https://ggplot2.tidyverse.org/reference/position_stack.html)
  (i.e., stacked to 100%).

- horizontal:

  A [logical](https://rdrr.io/r/base/logical.html) to turn the plot 90
  degrees using
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).
  This option also updates some theme options, so that e.g.,
  `x.lbl_italic` will still apply to the original X-axis.

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
  `4` for a UpSet plots (using `type = "upset"`); `5` for a dumbbell
  plots (using `type = "dumbbell"`); `0.75` otherwise.

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

  - `0.25` for geoms that are continous and have fills (such as
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

  Width of bins (only useful for `geom = "histogram"`), can be specified
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

- legend.position:

  Position of the legend, must be `"top"`, `"right"`, `"bottom"`,
  `"left"` or `"none"` (or `NA` or `NULL`), can be abbreviated. Defaults
  to `"right"` for numeric `category` values and 'sf' plots, and `"top"`
  otherwise.

- legend.reverse, legend.barheight, legend.barwidth, legend.nbin,
  legend.italic, legend.nrow:

  Other settings for the legend.

- sankey.node_width:

  Width of the vertical nodes in a Sankey plot.

- sankey.node_whitespace:

  Whitespace between the nodes in a Sankey plot.

- sankey.alpha:

  Alpha of the flows in a Sankey plot.

- sankey.remove_axes:

  Logical to indicate whether all axes must be removed in a Sankey plot.

- zoom:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate if the
  plot should be scaled to the data, i.e., not having the x and y axes
  to start at 0. This will set `x.zoom = TRUE` and `y.zoom = TRUE`.

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
  [Google Fonts](https://fonts.google.com). When using custom fonts in R
  Markdown, be sure to set the chunk option `fig.showtext = TRUE`,
  otherwise an informative error will be generated.

- theme:

  A valid `ggplot2`
  [theme](https://ggplot2.tidyverse.org/reference/theme.html) to apply,
  or `NULL` to use the default `theme_grey()`. This argument accepts
  themes (e.g., `theme_bw()`), functions (e.g., `theme_bw`) and
  characters themes (e.g., `"theme_bw"`). The default is
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

## Details

For geographic information system (GIS) analysis, use the `sf` package
with a data set containing geometries. The result can be used as input
for [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md).

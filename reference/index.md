# Package index

## Creating plots

[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
creates a complete plot from data in one call. Its methods accept data
frames, vectors, tables, formulas, matrices, models and geographic data.
[`add_type()`](https://msberends.github.io/plot2/reference/add_type.md)
and its shortcuts add layers to a plot.

- [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) :

  Conveniently Create a New `ggplot`

- [`plot2(`*`<default>`*`)`](https://msberends.github.io/plot2/reference/plot2-methods.md)
  [`plot2(`*`<formula>`*`)`](https://msberends.github.io/plot2/reference/plot2-methods.md)
  [`plot2(`*`<freq>`*`)`](https://msberends.github.io/plot2/reference/plot2-methods.md)
  [`plot2(`*`<sf>`*`)`](https://msberends.github.io/plot2/reference/plot2-methods.md)
  [`plot2(`*`<data.frame>`*`)`](https://msberends.github.io/plot2/reference/plot2-methods.md)
  [`plot2(`*`<matrix>`*`)`](https://msberends.github.io/plot2/reference/plot2-methods.md)
  :

  Methods for
  [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)

- [`add_type()`](https://msberends.github.io/plot2/reference/add_type.md)
  [`add_line()`](https://msberends.github.io/plot2/reference/add_type.md)
  [`add_point()`](https://msberends.github.io/plot2/reference/add_type.md)
  [`add_col()`](https://msberends.github.io/plot2/reference/add_type.md)
  [`add_errorbar()`](https://msberends.github.io/plot2/reference/add_type.md)
  [`add_smooth()`](https://msberends.github.io/plot2/reference/add_type.md)
  [`add_sf()`](https://msberends.github.io/plot2/reference/add_type.md)
  : Add Plot Element

## Changing plots

Change the mapping or the order of the layers of an existing plot, or
use the coordinate system of spider plots.

- [`add_mapping()`](https://msberends.github.io/plot2/reference/add_mapping.md)
  : Add Additional Mapping

- [`move_layer()`](https://msberends.github.io/plot2/reference/move_layer.md)
  :

  Move a `ggplot` Layer

- [`coord_spider()`](https://msberends.github.io/plot2/reference/coord_spider.md)
  [`CoordSpider`](https://msberends.github.io/plot2/reference/coord_spider.md)
  : Spider (Radar) Coordinate System for ggplot2

## Colours

Get colours from names and palettes, and register the colours of an
organisation for use in all plots.

- [`get_colour()`](https://msberends.github.io/plot2/reference/colour.md)
  [`register_colour()`](https://msberends.github.io/plot2/reference/colour.md)
  [`unregister_colour()`](https://msberends.github.io/plot2/reference/colour.md)
  [`add_white()`](https://msberends.github.io/plot2/reference/colour.md)
  :

  Get (and Register) Colours from R, Viridis and More

## Themes, titles and labels

The default theme of
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md),
markdown in titles, and labelling functions for axes and data labels.

- [`theme_minimal2()`](https://msberends.github.io/plot2/reference/theme_minimal2.md)
  : An Even More Minimal Theme
- [`md_to_expression()`](https://msberends.github.io/plot2/reference/md_to_expression.md)
  : Convert Markdown to Plotmath Expression
- [`get_plot_title()`](https://msberends.github.io/plot2/reference/get_plot_title.md)
  : Get Plot Title
- [`euros()`](https://msberends.github.io/plot2/reference/labellers.md)
  [`dollars()`](https://msberends.github.io/plot2/reference/labellers.md)
  : Label Currencies
- [`label_taxonomy()`](https://msberends.github.io/plot2/reference/label_taxonomy.md)
  : Label Taxonomic Names in Italics

## Number formatting

The decimal mark and thousands separator, determined from the session or
set with options.

- [`dec_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md)
  [`big_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md)
  : Use Decimal Comma?

## Interactive use

Convert a plot to an interactive `plotly` widget, or build a plot in a
Shiny app.

- [`as_plotly()`](https://msberends.github.io/plot2/reference/plotly.md)
  [`plotly_style()`](https://msberends.github.io/plot2/reference/plotly.md)
  : Create Interactive Plotly

- [`create_interactively()`](https://msberends.github.io/plot2/reference/create_interactively.md)
  :

  Interactively Create a `plot2`

## Options

Package options that set defaults for all plots in a session.

- [`plot2-options`](https://msberends.github.io/plot2/reference/plot2-options.md)
  :

  [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
  Options

## Data sets

Example data sets used in the documentation.

- [`admitted_patients`](https://msberends.github.io/plot2/reference/admitted_patients.md)
  : Example Data Set with Admitted Patients
- [`netherlands`](https://msberends.github.io/plot2/reference/netherlands.md)
  : Example Geography Data Set: the Netherlands

## `dplyr` and `tidyselect` functions

These are functions from `dplyr` and `tidyselect` that can be used in
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
directly, without the need to load those packages first.

- [`reexports`](https://msberends.github.io/plot2/reference/reexports.md)
  [`%>%`](https://msberends.github.io/plot2/reference/reexports.md)
  [`n`](https://msberends.github.io/plot2/reference/reexports.md)
  [`n_distinct`](https://msberends.github.io/plot2/reference/reexports.md)
  [`case_when`](https://msberends.github.io/plot2/reference/reexports.md)
  [`recode_values`](https://msberends.github.io/plot2/reference/reexports.md)
  [`replace_values`](https://msberends.github.io/plot2/reference/reexports.md)
  [`everything`](https://msberends.github.io/plot2/reference/reexports.md)
  [`starts_with`](https://msberends.github.io/plot2/reference/reexports.md)
  [`ends_with`](https://msberends.github.io/plot2/reference/reexports.md)
  [`matches`](https://msberends.github.io/plot2/reference/reexports.md)
  [`where`](https://msberends.github.io/plot2/reference/reexports.md)
  [`first`](https://msberends.github.io/plot2/reference/reexports.md)
  [`last`](https://msberends.github.io/plot2/reference/reexports.md)
  [`all_of`](https://msberends.github.io/plot2/reference/reexports.md)
  [`any_of`](https://msberends.github.io/plot2/reference/reexports.md) :
  Objects exported from other packages

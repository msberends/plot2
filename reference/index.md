# Package index

## Creating Plots

Everything starts here. The
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
function is the heart of this package – just pass in your data and it
will figure out the best way to visualise it. Whether you have a data
frame, a matrix, a statistical model, or even geographic data,
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) knows
what to do. Want to layer extra elements on top? Use
[`add_type()`](https://msberends.github.io/plot2/reference/add_type.md)
and its convenient shortcuts to build up your plot step by step.

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

## Customising Your Plot

Already have a plot but want to fine-tune it? These functions let you
change which variables are mapped to visual properties like colour or
size, reorder the layers that make up your plot, or switch to a spider
(radar) chart layout. Small adjustments can make a big difference in how
clearly your data tells its story.

- [`add_mapping()`](https://msberends.github.io/plot2/reference/add_mapping.md)
  : Add Additional Mapping

- [`move_layer()`](https://msberends.github.io/plot2/reference/move_layer.md)
  :

  Move a `ggplot` Layer

- [`coord_spider()`](https://msberends.github.io/plot2/reference/coord_spider.md)
  [`CoordSpider`](https://msberends.github.io/plot2/reference/coord_spider.md)
  : Spider (Radar) Coordinate System for ggplot2

## Working with Colours

Colour is one of the most powerful tools in data visualisation. These
functions give you access to a wide range of colour palettes – including
colourblind-friendly options – and let you define and save your own
custom colours for consistent use across all your plots and projects.

- [`get_colour()`](https://msberends.github.io/plot2/reference/colour.md)
  [`register_colour()`](https://msberends.github.io/plot2/reference/colour.md)
  [`unregister_colour()`](https://msberends.github.io/plot2/reference/colour.md)
  [`add_white()`](https://msberends.github.io/plot2/reference/colour.md)
  :

  Get (and Register) Colours from R, Viridis and More

## Themes and Labels

Great plots deserve a clean, professional look.
[`theme_minimal2()`](https://msberends.github.io/plot2/reference/theme_minimal2.md)
gives your plots a modern, print-ready appearance with sensible
defaults. The other functions in this section help you use **bold**,
*italic*, and other rich formatting in your plot titles and axis labels,
and make it easy to extract or customise the title of any plot.

- [`theme_minimal2()`](https://msberends.github.io/plot2/reference/theme_minimal2.md)
  : An Even More Minimal Theme
- [`md_to_expression()`](https://msberends.github.io/plot2/reference/md_to_expression.md)
  : Convert Markdown to Plotmath Expression
- [`get_plot_title()`](https://msberends.github.io/plot2/reference/get_plot_title.md)
  : Get Plot Title
- [`euros()`](https://msberends.github.io/plot2/reference/labellers.md)
  [`dollars()`](https://msberends.github.io/plot2/reference/labellers.md)
  : Label Euro currencies

## Number Formatting

Numbers on axes and in labels should be easy to read, regardless of
where in the world your audience is. These helpers automatically format
numbers with the right decimal separator and thousands grouping for your
locale, and can display values as euros or dollars.

- [`dec_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md)
  [`big_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md)
  : Use Decimal Comma?

## Interactive Plots

Want your audience to hover, zoom, and click on your plots? Turn any
plot into an interactive widget with
[`as_plotly()`](https://msberends.github.io/plot2/reference/plotly.md),
or use
[`create_interactively()`](https://msberends.github.io/plot2/reference/create_interactively.md)
to build a plot from scratch using a point-and-click interface – no code
required.

- [`as_plotly()`](https://msberends.github.io/plot2/reference/plotly.md)
  [`plotly_style()`](https://msberends.github.io/plot2/reference/plotly.md)
  : Create Interactive Plotly

- [`create_interactively()`](https://msberends.github.io/plot2/reference/create_interactively.md)
  :

  Interactively Create a `plot2`

## Default Settings

Tired of setting the same options every time? Configure your preferred
defaults – such as your go-to colour scheme, font, or decimal format –
once, and every plot you create will automatically use them.

- [`plot2-options`](https://msberends.github.io/plot2/reference/plot2-options.md)
  :

  [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
  Options

## Example Datasets

These built-in datasets are ready to use for exploring
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) and
trying out different plot types. No need to load external files – just
type the dataset name and start plotting.

- [`admitted_patients`](https://msberends.github.io/plot2/reference/admitted_patients.md)
  : Example Data Set with Admitted Patients
- [`netherlands`](https://msberends.github.io/plot2/reference/netherlands.md)
  : Example Geography Data Set: the Netherlands

## Convenience Functions

A handful of commonly used helper functions from other packages, made
available directly through `plot2` so you do not need to load additional
packages for everyday tasks like counting rows or selecting columns.

- [`reexports`](https://msberends.github.io/plot2/reference/reexports.md)
  [`%>%`](https://msberends.github.io/plot2/reference/reexports.md)
  [`n`](https://msberends.github.io/plot2/reference/reexports.md)
  [`n_distinct`](https://msberends.github.io/plot2/reference/reexports.md)
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

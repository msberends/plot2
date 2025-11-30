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

#' Conveniently Create a New `ggplot`
#'
#' @description The [plot2()] function is a convenient wrapper around many [`ggplot2`][ggplot2::ggplot()] functions. By design, the `ggplot2` package requires users to use a lot of functions and manual settings, while the [plot2()] function does all the heavy lifting automatically and only requires users to define some arguments in one single function, greatly increases convenience.
#'
#' Moreover, [plot2()] allows for in-place calculation of `y`, all axes, and all axis labels, often preventing the need to use [`group_by()`][dplyr::group_by()], [`count()`][dplyr::count()], [`mutate()`][dplyr::mutate()], or [`summarise()`][dplyr::summarise()].
#' 
#' See [plot2-methods] for all implemented methods for different object classes.
#' @param .data Data to plot.
#' @param x Plotting 'direction' for the x axis. This can be:
#' 
#' - A single variable from `.data`, such as `x = column1`
#' 
#' - A [function] to calculate over one or more variables from `.data`, such as `x = format(column1, "%Y")`, or `x = ifelse(column1 == "A", "Group A", "Other")`
#' 
#' - Multiple variables from `.data`, such as `x = c(column1, column2, column2)`, or using [selection helpers][tidyselect::language] such as `x = where(is.character)` or `x = starts_with("var_")` *(only allowed and required for Sankey plots using `type = "sankey"`)*
#' @param y Values to use for plotting along the y axis. This can be:
#' 
#' - A single variable from `.data`, such as `y = column1`
#' 
#' - Multiple variables from `.data`, such as `y = c(column1, column2)` or `y = c(name1 = column1, "name 2" = column2)`, or using [selection helpers][tidyselect::language] such as `y = where(is.double)` or `y = starts_with("var_")` *(multiple variables only allowed if `category` is not set)*
#' 
#' - A [function] to calculate over `.data` returning a single value, such as `y = n()` for the row count, or based on other variables such as `y = n_distinct(person_id)`, `y = max(column1)`, or `y = median(column2) / column3`
#' 
#' - A [function] to calculate over `.data` returning multiple values, such as `y = quantile(column1, c(0.25, 0.75))` or `y = range(age)`  *(multiple values only allowed if `category` is not set)*
#' @param category,facet Plotting 'direction' (`category` is called 'fill' and 'colour' in `ggplot2`). This can be:
#' 
#' - A single variable from `.data`, such as `category = column1`
#' 
#' - A [function] to calculate over one or more variables from `.data`, such as `category = median(column2) / column3`, or `facet = ifelse(column1 == "A", "Group A", "Other")`
#' 
#' - Multiple variables from `.data`, such as `facet = c(column1, column2)` (use `sep` to control the separator character)
#'   
#' - One or more variables from `.data` using [selection helpers][tidyselect::language], such as `category = where(is.double)` or `facet = starts_with("var_")`
#' 
#'  The `category` can also be a date or date/time (class `Date` or `POSIXt`).
#' 
#' @param y_secondary Values to use for plotting along the secondary y axis. This functionality is poorly supported by `ggplot2` and might give unexpected results. Setting the secondary y axis will set the colour to the axis titles.
#' @param y_secondary.colour,y_secondary.colour_fill Colours to set for the secondary y axis, will be evaluated with [get_colour()].
#' @param type,y_secondary.type Type of visualisation to use. This can be:
#' 
#' - A `ggplot2` geom name or their abbreviation such as `"col"` and `"point"`. All geoms are supported (including [`geom_blank()`][ggplot2::geom_blank()]).
#' 
#'   Full function names can be used (e.g., `"geom_histogram"`), but they can also be abbreviated (e.g., `"h"`, `"hist"`). The following geoms can be abbreviated by their first character: area (`"a"`), boxplot (`"b"`), beeswarm (`"bs"`), column (`"c"`), histogram (`"h"`), jitter (`"j"`), line (`"l"`), point (`"p"`), ribbon (`"r"`), and violin (`"v"`).
#' 
#'   Please note: in `ggplot2`, 'bars' and 'columns' are equal, while it is common to many people that 'bars' are oriented horizontally and 'columns' are oriented vertically since Microsoft Excel has been using these terms this way for many years. For this reason, `type = "bar"` will set `type = "col"` and `horizontal = TRUE`.
#' 
#' - One of these additional types:
#' 
#'   - `"barpercent"` (short: `"bp"`), which is effectively a shortcut to set `type = "col"` and `horizontal = TRUE` and `x.max_items = 10` and `x.sort = "freq-desc"` and `datalabels.format = "%n (%p)"`.
#'   - `"linedot"` (short: `"ld"`), which sets `type = "line"` and adds two point geoms using [add_point()]; one with large white dots and one with smaller dots using the colours set in `colour`. This is essentially equal to base \R `plot(..., type = "b")` but with closed shapes.
#'   - `"upset"` or `"UpSet"` (short: `"u"`) creates an [UpSet plot](https://en.wikipedia.org/wiki/UpSet_plot), which requires `x` to contain multiple variables from `.data` that contain `0`/`1` or `FALSE`/`TRUE` values. It is also possible to provide `y`, on which then `summarise_function` will be applied.
#'   - `"dumbbell"` (short: `"d"`), which sets `type = "point"` and `horizontal = TRUE`, and adds a line between the points (using [`geom_segment()`][ggplot2::geom_segment()]). The line colour cannot be changed. This plot type is only possible when the `category` has two distinct values.
#'   - `"sankey"` (short: `"s"`) creates a [Sankey plot](https://en.wikipedia.org/wiki/Sankey_diagram) using `category` for the flows and requires `x` to contain multiple variables from `.data`. At default, it also sets `x.expand = c(0.05, 0.05)` and `y.limits = c(NA, NA)` and `y.expand = c(0.01, 0.01)`. The so-called 'nodes' (the 'blocks' with text) are considered the datalabels, so you can set the text size and colour of the nodes using `datalabels.size`, `datalabels.colour`, and `datalabels.colour_fill`. The transparency of the flows can be set using `sankey.alpha`, and the width of the nodes can be set using `sankey.node_width`. Sankey plots can also be flipped using `horizontal = TRUE`.
#' 
#' - Left blank. In this case, the type will be determined automatically: `"boxplot"` if there is no x axis or if the length of unique values per x axis item is at least 3, `"point"` if both the y and x axes are numeric, and the [option][options()] `"plot2.default_type"` otherwise (which defaults to `"col"`). Use `type = "blank"` or `type = "geom_blank"` to *not* add a geom.
#' @param title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title A title to use. This can be:
#' 
#' - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default)
#' - A [function] to calculate over `.data`, such as `title = paste("Based on n =", n_distinct(person_id), "individuals")` or `subtitle = paste("Total rows:", n())`, see *Examples*
#' - An [expression], e.g. using `parse(text = "...")`
#' 
#' The `category.title` defaults to `TRUE` if the legend items are numeric.
#' @param title.linelength Maximum number of characters per line in the title, before a linebreak occurs.
#' @param title.colour Text colour of the title.
#' @param subtitle.linelength Maximum number of characters per line in the subtitle, before a linebreak occurs.
#' @param subtitle.colour Text colour of the subtitle.
#' @param na.replace Character to put in place of `NA` values if `na.rm = FALSE`.
#' @param na.rm Remove `NA` values from showing in the plot.
#' @param facet.fixed_y A [logical] to indicate whether all y scales should have the same limits. Defaults to `TRUE` only if the coefficient of variation (standard deviation divided by mean) of the maximum values of y is less than 25%.
#' @param facet.fixed_x A [logical] to indicate whether all x scales should have the same breaks. This acts like the inverse of `x.drop`.
#' @param facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative Additional settings for the plotting direction `facet`.
#' @param x.date_breaks Breaks to use when the x axis contains dates, will be determined automatically if left blank. This accepts values such as `"1 day"` and `"2 years"`.
#' @param x.date_labels Labels to use when the x axis contains dates, will be determined automatically if left blank. This accepts 'Excel' date-language such as `"d mmmm yyyy"`.
#' @param x.date_remove_years A [logical] to indicate whether the years of all `x` values must be unified. This will set the years of all `x` values [to 1970](https://en.wikipedia.org/wiki/Unix_time) if the data does not contain a leap year, and to 1972 otherwise. This allows to plot years on the `category` while maintaining a date range on `x`. The default is `FALSE`, unless `category` contains all years present in `x`.
#' @param category.focus A value of `category` that should be highlighted, meaning that all other values in `category` will be greyed out. This can also be a numeric value between 1 and the length of unique values of `category`, e.g. `category.focus = 2` to focus on the second legend item.
#' @param colour Get_colour(s) to set, will be evaluated with [get_colour()] if set. This can also be one of the viridis colours with automatic implementation for any plot: `"viridis"`, `"magma"`, `"inferno"`, `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. Also, this can be a named vector to match values of `category`, see *Examples*. Using a named vector can be used to manually sort the values of `category`.
#' @param colour_fill Get_colour(s) to be used for filling, will be determined automatically if left blank and will be evaluated with [get_colour()].
#' @param colour_opacity Amount of opacity for `colour`/`colour_fill` (0 = solid, 1 = transparent).
#' @param x.lbl_angle Angle to use for the x axis in a counter-clockwise direction (i.e., a value of `90` will orient the axis labels from bottom to top, a value of `270` will orient the axis labels from top to bottom).
#' @param x.lbl_align Alignment for the x axis between `0` (left aligned) and `1` (right aligned).
#' @param x.lbl_italic [logical] to indicate whether the x labels should in in *italics*.
#' @param x.lbl_taxonomy A [logical] to transform all words of the `x` labels into italics that are in the [microorganisms][AMR::microorganisms] data set of the `AMR` package. This uses [md_to_expression()] internally and will set `x.labels` to parse expressions.
#' @param x.character A [logical] to indicate whether the values of the x axis should be forced to [character]. The default is `FALSE`, except for years (values between 2000 and 2050) and months (values from 1 to 12).
#' @param x.drop [logical] to indicate whether factor levels should be dropped.
#' @param x.complete,category.complete,facet.complete A value to complete the data. This makes use of [tidyr::full_seq()] and [tidyr::complete()]. For example, using `x.complete = 0` will apply `data |> complete(full_seq(x, ...), fill = list(x = 0))`. Using value `TRUE` (e.g., `x.complete = TRUE`) is identical to using value `0`.
#' @param x.mic [logical] to indicate whether the x axis should be formatted as [MIC values][AMR::as.mic()], by dropping all factor levels and adding missing factors of 2.
#' @param x.remove,y.remove A [logical] to indicate whether the axis labels and title should be removed.
#' @param y.24h a [logical] to indicate whether the y labels and breaks should be formatted as 24-hour sequences
#' @param y.age A [logical] to indicate whether the y labels and breaks should be formatted as ages in years.
#' @param y.scientific,y_secondary.scientific A [logical] to indicate whether the y labels should be formatted in scientific notation. Defaults to `TRUE` only if the range of the y values spans more than `10e5`.
#' @param y.percent,y_secondary.percent A [logical] to indicate whether the y labels should be formatted as percentages.
#' @param y.percent_break A value on which the y axis should have breaks.
#' @param x.breaks,y.breaks A breaks function or numeric vector to use for the axis.
#' @param x.n_breaks,y.n_breaks Number of breaks, only useful if `x.breaks` cq. `y.breaks` is `NULL`.
#' @param x.limits,y.limits Limits to use for the axis, can be length 1 or 2. Use `NA` for the highest or lowest value in the data, e.g. `y.limits = c(0, NA)` to have the y scale start at zero.
#' @param x.labels,y.labels,y_secondary.labels A labels function or character vector to use for the axis.
#' @param x.expand,y.expand [expansion][ggplot2::expansion] to use for the axis, can be length 1 or 2. `x.expand` defaults to 0.5 and `y.expand` defaults to `0.25`, except for sf objects (then both default to 0).
#' @param x.transform,y.transform,category.transform A transformation function to use, e.g. `"log2"`. This can be: `r paste0('\u0060"', sort(gsub("_trans$", "", ls(envir = asNamespace("scales"))[grepl("_trans$", ls(envir = asNamespace("scales")))])), '"\u0060', collapse = ", ")`.
#' @param x.position,y.position Position of the axis.
#' @param x.zoom,y.zoom A [logical] to indicate if the axis should be zoomed on the data, by setting `x.limits = c(NA, NA)` and `x.expand = 0` for the x axis, or `y.limits = c(NA, NA)` and `y.expand = 0` for the y axis.
#' @param category.type Type of the `category`, one or more of: `"colour"` (default), `"shape"`, `"size"`, `"linetype"`, `"linewidth"`, `"alpha"`. There is no need to set `"fill"`, as `plot2` handles colour-setting internally and determines automatically whether the `colour` or `fill` aesthetic must be used.
#' @param category.labels,category.percent,category.breaks,category.expand,category.midpoint Settings for the plotting direction `category`.
#' @param category.limits Limits to use for a numeric category, can be length 1 or 2. Use `NA` for the highest or lowest value in the data, e.g. `category.limits = c(0, NA)` to have the scale start at zero.
#' @param category.date_breaks Breaks to use when the category contains dates, will be determined automatically if left blank. This will be passed on to [`seq.Date(by = ...)`][seq.Date()] and thus can be: a number, taken to be in days, or a character string containing one of "day", "week", "month", "quarter" or "year" (optionally preceded by an integer and a space, and/or followed by "s").
#' @param category.date_labels Labels to use when the category contains dates, will be determined automatically if left blank. This accepts 'Excel' date-language such as `"d mmmm yyyy"`.
#' @param category.character A [logical] to indicate whether the values of the category should be forced to [character]. The default is `FALSE`, except for years (values between 2000 and 2050) and months (values from 1 to 12).
#' @param x.max_items,category.max_items,facet.max_items Number of maximum items to use, defaults to infinite. All other values will be grouped and summarised using the `summarise_function` function. **Please note:** the sorting will be applied first, allowing to e.g. plot the top *n* most frequent values of the x axis by combining `x.sort = "freq-desc"` with `x.max_items =` *n*.
#' @param x.max_txt,category.max_txt,facet.max_txt The text to use of values not included number of `*.max_items`. The placeholder `%n` will be replaced with the outcome of the `summarise_function` function, the placeholder `%p` will be replaced with the percentage.
#' @param x.sort,category.sort,facet.sort Sorting of the plotting direction, defaults to `TRUE`, except for continuous values on the x axis (such as dates and numbers). Applying one of the sorting methods will transform the values to an ordered [factor], which `ggplot2` uses to orient the data. Valid values are:
#' 
#' - A manual vector of values
#' - `TRUE`: sort [factor]s on their levels, otherwise sort ascending on alphabet, while maintaining numbers in the text (*numeric* sort)
#' - `FALSE`: sort according to the order in the data
#' - `NULL`: do not sort/transform at all
#' - `"asc"` or `"alpha"`: sort as `TRUE`
#' - `"desc"`: sort [factor]s on their [reversed][rev()] levels, otherwise sort descending on alphabet, while maintaining numbers in the text (*numeric* sort)
#' - `"order"` or `"inorder"`: sort as `FALSE`
#' - `"freq"` or `"freq-desc"`: sort descending according to the frequencies of `y` computed by `summarise_function` (highest value first)
#' - `"freq-asc"`: sort ascending according to the frequencies of `y` computed by `summarise_function` (lowest value first)
#' @param datalabels Values to show as datalabels, see also `datalabels.format`. This can be:
#' 
#' - Left blank. This will default to the values of `y` in column-type plots, or when plotting spatial 'sf' data, the values of the first column. It will print a maximum of 25 labels unless `datalabels = TRUE`.
#' - `TRUE` or `FALSE` to force or remove datalabels
#' - A function to calculate over `.data`, such as `datalabels = paste(round(column1), "\n", column2)`
#' @param datalabels.round Number of digits to round the datalabels, applies to both `"%n"` and `"%p"` for replacement (see `datalabels.format`).
#' @param datalabels.format Format to use for datalabels. This can be a function (such as [euros()]) or a text. For the text, `"%n"` will be replaced by the count number, and `"%p"` will be replaced by the percentage of the total count. Use `datalabels.format = NULL` to *not* transform the datalabels.
#' @param datalabels.colour,datalabels.colour_fill,datalabels.size,datalabels.angle,datalabels.lineheight Settings for the datalabels.
#' @param decimal.mark Decimal mark, defaults to [dec_mark()].
#' @param big.mark Thousands separator, defaults to [big_mark()].
#' @param summarise_function A [function] to use if the data has to be summarised, see *Examples*. This can also be `NULL`, which will be converted to `function(x) x`.
#' @param stacked A [logical] to indicate that values must be [stacked][ggplot2::position_stack()].
#' @param stacked_fill A [logical] to indicate that values must be [filled][ggplot2::position_fill()] (i.e., stacked to 100%).
#' @param horizontal A [logical] to turn the plot 90 degrees using [`coord_flip()`][ggplot2::coord_flip()]. This option also updates some theme options, so that e.g., `x.lbl_italic` will still apply to the original x axis.
#' @param reverse A [logical] to reverse the *values* of `category`. Use `legend.reverse` to reverse the *legend* of `category`.
#' @param smooth A [logical] to add a smooth. In histograms, this will add the density count as an overlaying line (default: `TRUE`). In all other cases, a smooth will be added using [`geom_smooth()`][ggplot2::geom_smooth()] (default: `FALSE`).
#' @param smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.linewidth,smooth.linetype,smooth.colour Settings for `smooth`.
#' @param size Size of the geom. Defaults to `2` for geoms [point][ggplot2::geom_point()], [jitter][ggplot2::geom_jitter()], and [beeswarm][ggbeeswarm::geom_beeswarm()]; `4` for a UpSet plots (using `type = "upset"`); `5` for a dumbbell plots (using `type = "dumbbell"`); `0.75` otherwise.
#' @param linetype Linetype of the geom, only suitable for geoms that draw lines. Possible options are:
#'   1. `"solid"` (default)
#'   2. `"dashed"`
#'   3. `"dotted"`
#'   4. `"dotdash"`
#'   5. `"longdash"`
#'   6. `"twodash"`
#'   
#'   Both numbers and text are supported, e.g., `linetype = 2` and `linetype = "dashed"` yield the same results.
#' @param linewidth Linewidth of the geom, only suitable for geoms that draw lines. Defaults to:
#' - `0.10` for [sf][ggplot2::geom_sf()]
#' - `0.25` for geoms that are continous and have fills (such as [area][ggplot2::geom_area()]), except for geoms [boxplot][ggplot2::geom_boxplot()] and [violin][ggplot2::geom_violin()]
#' - `1.0` for dumbbell plots (using `type = "dumbbell"`) and UpSet plots (using `type = "upset"`)
#' - `0.5` otherwise (such as geoms [line][ggplot2::geom_line()], [boxplot][ggplot2::geom_boxplot()], [violin][ggplot2::geom_violin()], and [histogram][ggplot2::geom_histogram()])
#' @param binwidth Width of bins (only useful for `geom = "histogram"`), can be specified as a numeric value or as a function that calculates width from `x`, see [`geom_histogram()`][ggplot2::geom_histogram()]. It defaults to approx. `diff(range(x))` divided by 12 to 22 based on the data.
#' @param width Width of the geom. Defaults to `0.66` for geoms [boxplot][ggplot2::geom_boxplot()], [violin][ggplot2::geom_violin()] and [jitter][ggplot2::geom_jitter()], and to `0.5` otherwise.
#' @param jitter_seed Seed (randomisation factor) to be set when using `type = "jitter"`.
#' @param violin_scale Scale to be set when using `type = "violin"`, can also be set to `"area"`.
#' @param legend.position Position of the legend, must be `"top"`, `"right"`, `"bottom"`, `"left"` or `"none"` (or `NA` or `NULL`), can be abbreviated. Defaults to `"right"` for numeric `category` values and 'sf' plots, and `"top"` otherwise.
#' @param legend.reverse,legend.barheight,legend.barwidth,legend.nbin,legend.italic,legend.nrow Other settings for the legend.
#' @param sankey.node_width Width of the vertical nodes in a Sankey plot.
#' @param sankey.node_whitespace Whitespace between the nodes in a Sankey plot.
#' @param sankey.alpha Alpha of the flows in a Sankey plot.
#' @param sankey.remove_axes Logical to indicate whether all axes must be removed in a Sankey plot.
#' @param zoom A [logical] to indicate if the plot should be scaled to the data, i.e., not having the x and y axes to start at 0. This will set `x.zoom = TRUE` and `y.zoom = TRUE`.
#' @param sep Separator character to use if multiple columns are given to either of the three directions: `x`, `category` and `facet`, e.g. `facet = c(column1, column2)`.
#' @param print A [logical] to indicate if the result should be [printed][print()] instead of just returned.
#' @param text_factor Text factor to use, which will apply to all texts shown in the plot.
#' @param font Font (family) to use, can be set with `options(plot2.font = "...")`. Can be any installed system font or any of the > 1400 font names from [Google Fonts](https://fonts.google.com). When using custom fonts in R Markdown, be sure to set the chunk option `fig.showtext = TRUE`, otherwise an informative error will be generated.
#' @param theme A valid `ggplot2` [theme][ggplot2::theme()] to apply, or `NULL` to use the default [`theme_grey()`][ggplot2::theme_grey()]. This argument accepts themes (e.g., `theme_bw()`), functions (e.g., `theme_bw`) and characters themes (e.g., `"theme_bw"`). The default is [theme_minimal2()], but can be set with `options(plot2.theme = "...")`.
#' @param background The background colour of the entire plot, can also be `NA` to remove it. Will be evaluated with [get_colour()]. Only applies when `theme` is not `NULL`.
#' @param markdown A [logical] to turn all labels and titles into [plotmath] expressions, by converting common markdown language using the [md_to_expression()] function (defaults to `TRUE`).
#' @param data Substitute for `.data`, used in formula notation, e.g., `plot2(hp ~ mpg, data = mtcars)`.
#' @param ... Any argument to give to the geom. This will override automatically-set settings for the geom.
#' @details The [plot2()] function is a convenient wrapper around many [`ggplot2`][ggplot2::ggplot()] functions such as [`ggplot()`][ggplot2::ggplot()], [`aes()`][ggplot2::aes()], [`geom_col()`][ggplot2::geom_col()], [`facet_wrap()`][ggplot2::facet_wrap()], [`labs()`][ggplot2::labs()], etc., and provides:
#'   - Writing as few lines of codes as possible
#'   - Easy plotting in three 'directions': `x` (the regular x axis), `category` (replaces 'fill' and 'colour') and `facet`
#'   - Automatic setting of these 'directions' based on the input data
#'   - Setting in-place calculations for all plotting directions and even `y`
#'   - Easy way for sorting data in many ways (such as on alphabet, numeric value, frequency, original data order), by setting a single argument for the 'direction': `x.sort`, `category.sort` and `facet.sort`
#'   - Easy limiting values, e.g. by setting `x.max_items = 5` or `category.max_items = 5`
#'   - Markdown support for any title text, with any theme
#'   - Integrated support for any Google Font and any installed system font
#'   - An extra clean, minimalistic theme with a lot of whitespace (but without unnecessary margins) that is ideal for printing: `theme_minimal2()`
#'   - Some conveniences from Microsoft Excel:
#'     - The y axis starts at 0 if possible
#'     - The y scale expands at the top to be better able to interpret all data points
#'     - Date breaks can be written in a human-readable format (such as "d mmm yyyy")
#'     - Labels with data values can easily be printed and are automatically determined
#'   - Support for any `ggplot2` extension based on [ggplot2::fortify()]
#'   
#' The `ggplot2` package in conjunction with the `tidyr`, `forcats` and `cleaner` packages can provide above functionalities, but the goal of the [plot2()] function is to generalise this into one function. The generic [plot2()] function currently has `r length(formals(plot2)) - 1` arguments, all with a default value. **Less typing, faster coding.**
#' @return a `ggplot` object
#' @importFrom ggplot2 ggplot labs
#' @export
#' @examples
#' options(plot2.colour = NULL, plot2.colour_sf_fill = NULL)
#' 
#' head(iris)
#' 
#' # no variables determined, so plot2() will try for itself -
#' # the type will be points since the first two variables are numeric
#' iris |>
#'   plot2()
#' 
#' # if x and y are set, no additional mapping will be set:
#' iris |> 
#'   plot2(Sepal.Width, Sepal.Length)
#' iris |> 
#'   plot2(Species, Sepal.Length)
#' 
#' # the arguments are in this order: x, y, category, facet
#' iris |> 
#'   plot2(Sepal.Length, Sepal.Width, Petal.Length, Species)
#' 
#' iris |> 
#'   plot2(Sepal.Length, Sepal.Width, Petal.Length, Species,
#'         colour = "viridis") # set the viridis colours
#'
#' # set your own colours
#' iris |> 
#'   plot2(Sepal.Length, Sepal.Width, Petal.Length, Species,
#'         colour = c("white", "red", "black"))
#'
#' # y can also be multiple (named) columns
#' iris |> 
#'   plot2(x = Sepal.Length,
#'         y = c(Length = Petal.Length, Width = Petal.Width),
#'         category.title = "Petal property")
#' iris |>
#'   # with included selection helpers such as where(), starts_with(), etc.:
#'   plot2(x = Species, y = where(is.double))
#'   
#' # the category type can be one or more aesthetics
#' iris |>
#'   plot2(zoom = TRUE,
#'         category.type = c("colour", "shape"),
#'         size = 3)
#' iris |>
#'   plot2(zoom = TRUE,
#'         category = Petal.Length,
#'         category.type = c("colour", "size"),
#'         colour = "viridis")
#' 
#' # easily add a smooth
#' iris |>
#'   plot2(zoom = TRUE,
#'         smooth = TRUE)
#' iris |>
#'   plot2(zoom = TRUE,
#'         smooth = TRUE,
#'         smooth.method = "lm")
#' 
#' # support for secondary y axis
#' mtcars |>
#'   plot2(x = mpg,
#'         y = hp,
#'         y_secondary = disp ^ 2, 
#'         y_secondary.scientific = TRUE,
#'         title = "Secondary y axis sets colour to the axis titles")
#' 
#' 
#' admitted_patients
#' 
#' # the arguments are in this order: x, y, category, facet
#' admitted_patients |>
#'   plot2(hospital, age)
#' 
#' admitted_patients |>
#'   plot2(hospital, age, gender)
#'   
#' admitted_patients |>
#'   plot2(hospital, age, gender, ward)
#'   
#' # or use any function for y
#' admitted_patients |>
#'   plot2(hospital, median(age), gender, ward)
#' admitted_patients |>
#'   plot2(hospital, n(), gender, ward)
#'
#' admitted_patients |>
#'   plot2(x = hospital,
#'         y = age,
#'         category = gender,
#'         colour = c("F" = "#3F681C", "M" = "#375E97"),
#'         colour_fill = "#FFBB00AA",
#'         linewidth = 1.25,
#'         y.age = TRUE)
#' 
#' admitted_patients |>
#'   plot2(age, type = "hist")
#' 
#' # even titles support calculations, including support for {glue}
#' admitted_patients |>
#'   plot2(age, type = "hist",
#'         title = paste("Based on n =", n_distinct(patient_id), "patients"),
#'         subtitle = paste("Total rows:", n()),
#'         caption = glue::glue("From {n_distinct(hospital)} hospitals"),
#'         x.title = paste("Age ranging from", paste(range(age), collapse = " to ")))
#'  
#' # the default type is column, datalabels are automatically
#' # set in non-continuous types:
#' admitted_patients |> 
#'   plot2(hospital, n(), gender)
#'   
#' admitted_patients |> 
#'   plot2(hospital, n(), gender,
#'         stacked = TRUE)
#'         
#' admitted_patients |> 
#'   plot2(hospital, n(), gender,
#'         stacked_fill = TRUE)
#' 
#' # two categories might benefit from a dumbbell plot:
#' admitted_patients |> 
#'   plot2(hospital, median(age), gender, type = "dumbbell")
#'  
#' # sort on any direction:
#' admitted_patients |> 
#'   plot2(hospital, n(), gender,
#'         x.sort = "freq-asc",
#'         stacked = TRUE)
#' 
#' admitted_patients |> 
#'   plot2(hospital, n(), gender,
#'         x.sort = c("B", "D", "A"), # missing values ("C") will be added
#'         category.sort = "alpha-desc",
#'         stacked = TRUE)
#'         
#' # support for Sankey plots
#' Titanic |> # a table from base R
#'   plot2(x = c(Age, Class, Survived),
#'         category = Sex,
#'         type = "sankey")
#'
#' # matrix support, such as for cor()
#' correlation_matrix <- cor(mtcars)
#' class(correlation_matrix)
#' head(correlation_matrix)
#' mtcars |> 
#'   cor() |>
#'   plot2()
#' 
#' mtcars |> 
#'   cor() |>
#'   plot2(colour = c("blue3", "white", "red3"),
#'         datalabels = TRUE,
#'         category.title = "*r*-value",
#'         title =  "Correlation matrix")
#' 
#' 
#' # plot2() supports all S3 extensions available through
#' # ggplot2::fortify() and broom::augment(), such as regression models:
#' lm(mpg ~ hp, data = mtcars) |> 
#'   plot2(x = mpg ^ -3,
#'         y = hp ^ 2,
#'         smooth = TRUE,
#'         smooth.method = "lm",
#'         smooth.formula = "y ~ log(x)",
#'         title = "Titles/captions *support* **markdown**",
#'         subtitle = "Axis titles contain the square notation: x^2")
#' 
#' # sf objects (geographic plots, 'simple features') are also supported
#' netherlands |> 
#'   plot2()
#' netherlands |> 
#'   plot2(colour_fill = "viridis", colour_opacity = 0.75) |>
#'   add_sf(netherlands, colour = "black", colour_fill = NA)
#' 
#' # support for any system or Google font
#' mtcars |>
#'   plot2(mpg, hp,
#'         font = "Rock Salt",
#'         text_factor = 1.25,
#'         title = "This plot uses a Google Font")
plot2 <- function(.data,
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
                  y_secondary.colour = colour,
                  y_secondary.colour_fill = colour_fill,
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
                  datalabels.lineheight = 1.0,
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
                  legend.title = NULL, # will become TRUE in numeric categories if left NULL
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
                  ...) {
  
  # no observations, return empty plot immediately
  if (NROW(.data) == 0) {
    # check if markdown is required
    markdown <- validate_markdown(markdown, x.title, y.title, c(category.title, legend.title), title, subtitle, tag, caption)
    plot2_warning("No observations, returning an empty plot")
    p <- ggplot() +
      validate_theme(theme = theme,
                     type = "",
                     background = background,
                     text_factor = text_factor,
                     font = font,
                     horizontal = horizontal,
                     x.remove = x.remove,
                     y.remove = y.remove,
                     x.lbl_angle = x.lbl_angle,
                     x.lbl_align = x.lbl_align,
                     x.lbl_italic = x.lbl_italic,
                     facet.fill = facet.fill,
                     facet.bold = facet.bold,
                     facet.italic = facet.italic,
                     facet.size = facet.size,
                     facet.margin = facet.margin,
                     legend.italic = legend.italic,
                     sankey.remove_axes = sankey.remove_axes,
                     title.colour = title.colour,
                     subtitle.colour = subtitle.colour,
                     has_y_secondary = FALSE,
                     col_y_primary = NULL,
                     col_y_secondary = NULL)
    if (!missing(x.title)) p <- p + labs(x = validate_title(x.title, markdown = markdown))
    if (!missing(y.title)) p <- p + labs(y = validate_title(y.title, markdown = markdown))
    if (!missing(title)) p <- p + labs(title = validate_title(title, markdown = markdown, max_length = title.linelength))
    if (!missing(subtitle)) p <- p + labs(subtitle = validate_title(subtitle, markdown = markdown, max_length = subtitle.linelength))
    if (!missing(tag)) p <- p + labs(tag = validate_title(tag, markdown = markdown))
    if (!missing(caption)) p <- p + labs(caption = validate_title(caption, markdown = markdown))
    if (isTRUE(print)) {
      print(p)
      return(invisible())
    } else {
      return(p)
    }
  }
  
  if (tryCatch(!inherits(.data, "sf") &&
               ((isTRUE("geometry" %in% colnames(.data)) && suppressWarnings(inherits(.data$geometry, "sfc")))
                || isTRUE(attributes(.data)$sf_column %in% colnames(.data))) &&
               "sf" %in% rownames(utils::installed.packages()),
               error = function(e) FALSE)) {
    # force calling plot2.sf() and its arguments, data will be transformed in that function:
    rlang::check_installed("sf")
    UseMethod("plot2", object = structure(data.frame(), class = "sf"))
  } else {
    UseMethod("plot2")
  }
}

#' @importFrom dplyr mutate vars group_by across summarise reframe select bind_cols filter as_tibble any_of all_of desc rowwise c_across left_join
#' @importFrom ggplot2 ggplot aes labs stat_boxplot scale_colour_manual scale_fill_manual coord_flip coord_cartesian geom_smooth geom_density guides guide_legend scale_x_discrete waiver ggplot_build after_stat scale_fill_continuous scale_fill_date scale_fill_datetime scale_fill_continuous scale_colour_date scale_colour_datetime scale_colour_continuous geom_segment scale_colour_discrete scale_fill_discrete scale_y_discrete
#' @importFrom tidyr pivot_longer pivot_wider
plot2_exec <- function(.data,
                       x,
                       y,
                       category,
                       facet,
                       type,
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
                       facet.fill,
                       facet.bold,
                       facet.italic,
                       facet.size,
                       facet.margin,
                       facet.repeat_lbls_x,
                       facet.repeat_lbls_y,
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
                       colour_opacity,
                       x.lbl_angle,
                       x.lbl_align,
                       x.lbl_italic,
                       x.lbl_taxonomy,
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
                       x.mic,
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
                       datalabels.colour,
                       datalabels.format,
                       datalabels.colour_fill,
                       datalabels.size,
                       datalabels.angle,
                       datalabels.lineheight,
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
                       sankey.node_width,
                       sankey.node_whitespace,
                       sankey.alpha,
                       sankey.remove_axes,
                       zoom,
                       sep,
                       print,
                       text_factor,
                       font,
                       theme,
                       background,
                       markdown,
                       ...) {
  
  dots <- list(...)
  dots_geom <- dots[names(dots) %unlike% "^_(label[.]|misses[.]|sf.column|summarise_fn_name|datalabels.centroid)"]
  if (length(dots_geom) > 0) {
    plot2_message(ifelse(length(dots_geom) == 1,
                         "This additional argument was given to the geom: ",
                         "These additional arguments were given to the geom: "),
                  paste0(font_blue(names(dots_geom), collapse = NULL), collapse = font_black(", ")))
  }
  
  # record missing arguments ----
  misses_x <- isTRUE(dots$`_misses.x`)
  misses_y <- isTRUE(dots$`_misses.y`)
  misses_category <- isTRUE(dots$`_misses.category`)
  misses_facet <- isTRUE(dots$`_misses.facet`)
  misses_datalabels <- isTRUE(dots$`_misses.datalabels`)
  misses_colour_fill <- isTRUE(dots$`_misses.colour_fill`)
  misses_x.title <- isTRUE(dots$`_misses.x.title`)
  misses_y.title <- isTRUE(dots$`_misses.y.title`)
  misses_title <- isTRUE(dots$`_misses.title`)
  misses_subtitle <- isTRUE(dots$`_misses.subtitle`)
  misses_tag <- isTRUE(dots$`_misses.tag`)
  misses_caption <- isTRUE(dots$`_misses.caption`)
  misses_x.zoom <- isTRUE(dots$`_misses.x.zoom`)
  misses_x.max_items <- isTRUE(dots$`_misses.x.max_items`)
  misses_y.percent <- isTRUE(dots$`_misses.y.percent`)
  misses_y.percent_break <- isTRUE(dots$`_misses.y.percent_break`)
  misses_facet.fixed_x <- isTRUE(dots$`_misses.facet.fixed_x`)
  misses_summarise_function <- isTRUE(dots$`_misses.summarise_function`)
  
  if (!misses_facet.fixed_x) {
    x.drop <- !isTRUE(facet.fixed_x)
  }
  
  # get titles based on raw data ----
  # compute contents of title arguments
  title <- validate_title({{ title }}, markdown = isTRUE(markdown), df = .data, max_length = title.linelength)
  subtitle <- validate_title({{ subtitle }}, markdown = isTRUE(markdown), df = .data, max_length = subtitle.linelength)
  caption <- validate_title({{ caption }}, markdown = isTRUE(markdown), df = .data)
  tag <- validate_title({{ tag }}, markdown = isTRUE(markdown), df = .data)
  x.title <- validate_title({{ x.title }}, markdown = isTRUE(markdown), df = .data)
  y.title <- validate_title({{ y.title }}, markdown = isTRUE(markdown), df = .data)
  legend.title <- validate_title({{ legend.title }}, markdown = isTRUE(markdown), df = .data)
  category.title <- validate_title({{ category.title }}, markdown = isTRUE(markdown), df = .data)
  # category.title and legend.title both exist for convenience, though going forward, legend.title will be used
  legend.title <- if (is.null(category.title)) legend.title else category.title
  if (tryCatch(!is.null(y_secondary), error = function(e) TRUE)) {
    y_secondary.title <- validate_title({{ y_secondary.title }}, markdown = isTRUE(markdown), df = .data)
  }
  
  if (is.null(summarise_function)) {
    summarise_function <- function(x) x
    dots$`_summarise_fn_name` <- "function(x) x"
  } else if (!is.function(summarise_function)) {
    stop("'summarise_function' must be a function")
  }
  dots$`_summarise_fn_name` <- gsub("^base::", "", dots$`_summarise_fn_name`)
  
  # pre-validate types and set type shortcuts ----
  if (!is_empty(type) && !is.character(type)) {
    stop("'type' must be a character", call. = FALSE)
  }
  if (!is.null(type)) {
    type <- tolower(type[1L])
    type_backup <- type
    if (type %like% "^(barpercent|bp)$") {
      type_backup <- "barpercent"
      if (misses_x.max_items) {
        x.max_items <- 10 # instead of the default Inf
      }
      x.sort <- "freq-desc"
      datalabels.format <- "%n (%p)"
      type <- "col"
      horizontal <- TRUE
    }
    if (type %like% "^(line.*dot|line.*point|dot.*line|point.*line|ld|lp|dl|pl)s?$") {
      # set line for here, dots will be added in the end
      type_backup <- "linedot"
      type <- "line"
    }
    if (type %like% "^(upset|u)$") {
      type_backup <- "upset"
      type <- "upset"
    }
    if (type %like% "^(dumbb?ell?|d|db)$") {
      # set point for here, segments will be added in the end
      type_backup <- "dumbbell"
      type <- "point"
      horizontal <- TRUE
    }
    if (type %like% "^(sankey|s)$") {
      type <- "line" # this is to give the flows the right colour; a continuous geom without a fill
      type_backup <- "sankey"
      datalabels <- FALSE
      if (is.null(x.expand)) {
        x.expand <- c(0.05, 0.05)
      }
      if (is.null(y.limits)) {
        y.limits <- c(NA, NA)
        plot2_env$y_lim <- y.limits
      }
      if (is.null(y.expand)) {
        y.expand <- c(0.01, 0.01)
      }
      x.title = ""
    }
    if (type %like% "bar") {
      type <- "col"
      horizontal <- TRUE
    }
  } else {
    type_backup <- ""
  }
  
  if (decimal.mark == big.mark) {
    big.mark <- " "
  }
  
  if (!all(category.type %in% c("colour", "color", "fill", "shape", "size", "linetype", "linewidth", "alpha"), na.rm = TRUE)) {
    stop(('`category.type` must be: "colour", "color", "fill", "shape", "size", "linetype", "linewidth", "alpha"'), call. = FALSE)
  } else {
    if ("color" %in% category.type) {
      category.type[category.type == "color"] <- "colour"
    }
    if (any(c("colour", "fill") %in% category.type)) {
      category.type <- c("colour", "fill", category.type)
    }
    category.type <- unique(category.type)
  }
  
  # set environment ----
  set_plot2_env(x = dots$`_label.x`,
                y = dots$`_label.y`,
                category = dots$`_label.category`,
                facet = dots$`_label.facet`,
                y_secondary = dots$`_label.y_secondary`,
                x_variable_names = tryCatch(suppressWarnings(.data |> select({{ x }}) |> colnames()), error = function(e) NULL))
  
  on.exit({
    throw_messages()
    clear_plot2_env()
  })
  
  plot2_env$facet.fixed_y <- facet.fixed_y
  
  # prepare data ----
  # IMPORTANT: in this part, the data for mapping will be generated anonymously, e.g. as `_var_x` and `_var_category`;
  # this is done for convenience - this is restored before returning the `ggplot` object in the end
  df <- .data |>
    # add the three directions, these functions also support tidyverse selections: `facet = where(is.character)`
    add_direction(direction = {{ x }},
                  var_name = "x",
                  var_label = dots$`_label.x`,
                  sep = sep) |> 
    add_direction(direction = {{ category }}, 
                  var_name = "category",
                  var_label = dots$`_label.category`,
                  sep = sep) |> 
    add_direction(direction = {{ facet }}, 
                  var_name = "facet",
                  var_label = dots$`_label.facet`,
                  sep = sep) |> 
    # add y (this will end in an ungrouped data.frame)
    { function(.data) {
      suppressWarnings(
        y_select <- tryCatch((.data |>
                                # no tibbles, data.tables, sf, etc. objects:
                                as.data.frame(stringsAsFactors = FALSE) |> 
                                select({{ y }})),
                             error = function(e) FALSE)
      )
      
      has_multiple_cols <- is.data.frame(y_select) && ncol(y_select) > 1
      if (isTRUE(has_multiple_cols)) {
        # e.g. for: df |> plot2(y = c(var1, var2))  
        if (has_category(.data)) {
          # check if category was not already set
          stop("if 'y' contains more than one variable, 'category' must not be set", call. = FALSE)
        }
        
        if (isTRUE(misses_summarise_function)) {
          summarise_function <<- function(x) x
          dots$`_summarise_fn_name` <<- "function(x) x"
          misses_summarise_function <<- FALSE
          # plot2_message("Assuming ", font_blue(paste0("summarise_function = ", dots$`_summarise_fn_name`)))
        }
        
        new_df <- .data |>
          # no tibbles, data.tables, sf, etc. objects:
          as.data.frame(stringsAsFactors = FALSE) |> 
          bind_cols(y_select[, colnames(y_select)[which(!colnames(y_select) %in% colnames(.data))], drop = FALSE]) |> 
          pivot_longer(c(colnames(y_select), -matches("^_var_"), -get_x_name(.data)), names_to = "_var_category", values_to = "_var_y") |>
          # apply summarise_function
          group_by(across(c(plot2_env$x_variable_names, get_x_name(.data), get_category_name(.data), get_facet_name(.data),
                            matches("_var_(x|category|facet)")))) |> 
          reframe(`_var_y` = summarise_function(`_var_y`)) |> 
          mutate(y = `_var_y`,
                 category = `_var_category`)
        
        if (!dots$`_summarise_fn_name` %in% c("NULL", "function(x) x")) {
          plot2_message("Summarising values using ",
                        font_blue(paste0("summarise_function = ", dots$`_summarise_fn_name`)),
                        ifelse(isTRUE(misses_summarise_function),
                               paste0(" (use ", font_blue("summarise_function = NULL"), font_black(" to prevent this)")),
                               ""))
        }
        
        if (!any(plot2_env$mapping_y %like% new_df$category)) {
          plot2_message("Using ", font_blue("y = c(", paste(unique(new_df$category), collapse = ", "), ")", collapse = NULL))
        }
        plot2_env$mapping_y <- "y"
        plot2_env$mapping_category <- "category"
        if (is.null(y.title) || isTRUE(y.title)) {
          # we just fabricated an y value, so remove the title if it says nothing (double arrow since we're in if()):
          y.title <<- NULL
        }
        # return the data
        new_df
      } else {
        
        suppressWarnings(
          tryCatch(y_precalc <- .data |>
                     # no tibbles, data.tables, sf, etc. objects:
                     as.data.frame(stringsAsFactors = FALSE) |> 
                     summarise(val = {{ y }}),
                   error = function(e) stop(format_error(e), call. = FALSE))
        )
        y_precalc <- y_precalc$val # will be NULL if y is missing
        
        if (is.null(y_precalc) && identical(type, "upset")) {
          plot2_message("Using ", font_blue(paste0("y = 1")))
          y_precalc <- rep(1, NROW(.data))
          if (isTRUE(y.title)) {
            y.title <<- "Intersection size"
          }
        }
        
        if (isTRUE(length(y_precalc) == 1) && NROW(.data) > 1) {
          # outcome of y is a single calculated value (by using e.g. mean(...) or n_distinct(...)),
          # so calculate it over all groups that are available
          # this will support e.g. `data |> plot2(y = n_distinct(id))`
          suppressWarnings(
            tryCatch(.data |> 
                       group_by(across(c(plot2_env$x_variable_names, get_x_name(.data), get_category_name(.data), get_facet_name(.data),
                                         matches("_var_(x|category|facet)")))) |>
                       summarise(`_var_y` = {{ y }},
                                 .groups = "drop"),
                     error = function(e) stop(format_error(e, replace = "_var_y", by = "y"), call. = FALSE))
          )
          
        } else if (!has_category(.data) &&
                   !is.null(y_precalc) &&
                   length(y_precalc) != NROW(.data)) {
          # outcome of y is a multi-length calculated value (by using e.g. range(...)),
          # so calculate it over all groups that are available and add a category
          # this will support e.g. `data |> plot2(y = range(age))`
          if (is.null(category.labels)) {
            plot2_caution("Categories were auto-generated since ", font_blue("y"), " is a vector - use ", font_blue("category.labels"), " to manually name them.")
          }
          plot2_env$mapping_category <- "category"
          # take the first call or function from what was given to y
          y_call <- as.character(str2lang(plot2_env$mapping_y))[1]
          suppressWarnings(
            tryCatch(.data |> 
                       group_by(across(c(get_x_name(.data), get_facet_name(.data),
                                         matches("_var_(x|facet)")))) |> 
                       summarise(`_var_y` = {{ y }},
                                 `_var_category` = paste0(y_call, " (", seq_len(length(y_precalc)), ")"),
                                 .groups = "drop"),
                     error = function(e) stop(format_error(e, replace = "_var_y", by = "y"), call. = FALSE))
          )
          
        } else {
          # don't recalculate, just add the calculated values to save time
          # don't do as.data.frame() here - sf plots will lose their structure
          suppressWarnings(
            tryCatch(.data |> 
                       mutate(`_var_y` = y_precalc),
                     error = function(e) stop(format_error(e, replace = "_var_y", by = "y"), call. = FALSE))
          )
        }
      }
    }}() |> 
    mutate(`_var_y_secondary` = {{ y_secondary }}) |>
    mutate(`_var_datalabels` = {{ datalabels }}) |> 
    # this part will transform the data as needed
    validate_data(misses_x = misses_x,
                  misses_category = misses_category,
                  decimal.mark = decimal.mark,
                  big.mark = big.mark,
                  y.percent = y.percent,
                  type = type,
                  type_backup = type_backup,
                  datalabels.round = datalabels.round,
                  datalabels.format = datalabels.format,
                  x.sort = x.sort,
                  category.sort = category.sort,
                  facet.sort = facet.sort,
                  x.complete = x.complete,
                  category.complete = category.complete,
                  facet.complete = facet.complete,
                  summarise_function = summarise_function,
                  summarise_fn_name = dots$`_summarise_fn_name`,
                  misses_summarise_function = misses_summarise_function,
                  horizontal = horizontal,
                  x.max_items = x.max_items,
                  x.max_txt = x.max_txt,
                  x.character = x.character,
                  x.drop = x.drop,
                  x.mic = x.mic,
                  x.date_remove_years = x.date_remove_years,
                  category.max_items = category.max_items,
                  category.max_txt = category.max_txt,
                  category.character = category.character,
                  facet.max_items = facet.max_items,
                  facet.max_txt = facet.max_txt,
                  na.rm = na.rm,
                  na.replace = na.replace,
                  ...)
  
  # create UpSet plot ----

  if (identical(type, "upset")) {
    
    size <- validate_size(size = size, type = "upset", type_backup = "upset")
    width <- validate_width(width = width, type = "geom_col")
    linewidth <- validate_linewidth(linewidth = linewidth, type = "upset", type_backup = "upset")
    
    if (!has_x(df)) {
      stop("`x` must be multiple columns for `type = \"upset\"`", call. = FALSE)
    }
    df_x <- df |> select(!!str2lang(get_x_name(df)))
    if (NCOL(df_x) < 2) {
      stop("`x` must be multiple columns for `type = \"upset\"`", call. = FALSE)
    }
    df_x <- df_x |>
      mutate(across(everything(), as.logical))
    vars_x <- colnames(df_x)
    df_original <- bind_cols(df_x,
                             df |> select(any_of(c(get_y_name(df), get_category_name(df), "_var_x", "_var_y", "_var_category"))))
    
    df_count_total <- df_original |>
      group_by(across(any_of(c(vars_x, get_category_name(df))))) |>
      reframe(n = n(),
              out = summarise_function(`_var_y`)) |>
      # we must not yet use tibble here
      as.data.frame()
    
    if (dots$`_misses.summarise_function` == TRUE) {
      plot2_message("Using ", font_blue(paste0("summarise_function = ", dots$`_summarise_fn_name`)), " for UpSet plot")
    }
    df_count <- df_original |>
      group_by(across(any_of(vars_x))) |>
      reframe(n = n(),
              out = summarise_function(`_var_y`)) |>
      rowwise() |>
      mutate(count = sum(c_across(vars_x))) |>
      # we must not yet use tibble here - we need row names later on
      as.data.frame()
    
    lower_left_df <- df_original |>
      summarise(across(all_of(vars_x), function(x) sum(x, na.rm = TRUE))) |>
      pivot_longer(all_of(vars_x), names_to = "y", values_to = "n") |>
      arrange(desc(n))
    y_sorted <- lower_left_df$y
    
    if (is.null(x.sort)) {
      x.sort <- "freq-desc"
    } else if (x.sort == TRUE) {
      x.sort <- "desc"
    }
    x.sort <- validate_sorting(x.sort, horizontal = FALSE)
    if (x.sort == "desc") {
      sort_cols <- c("count", y_sorted)
      ord_list <- lapply(df_count[sort_cols], function(x) -xtfrm(x))
      x_sorted <- do.call(order, ord_list)
    } else if (x.sort == "asc") {
      sort_cols <- c("count", y_sorted)
      ord_list <- lapply(df_count[sort_cols], function(x) xtfrm(x))
      x_sorted <- do.call(order, ord_list)
    } else if (x.sort == "freq-desc") {
      sort_cols <- c("out", y_sorted)
      ord_list <- lapply(df_count[sort_cols], function(x) -xtfrm(x))
      x_sorted <- do.call(order, ord_list)
    } else if (x.sort == "freq-asc") {
      sort_cols <- c("out", y_sorted)
      ord_list <- lapply(df_count[sort_cols], function(x) xtfrm(x))
      x_sorted <- do.call(order, ord_list)
    } else {
      x_sorted <- rownames(df_count)
    }
    

    
    df_grid <- expand.grid(x = seq_len(nrow(unique(df_count))),
                           y = colnames(df_count)) |>
      filter(!y %in% c("out", "n", "count")) |>
      mutate(value = as.logical(df_count[cbind(x, match(y, colnames(df_count)))]),
             x = as.factor(x)) |>
      as_tibble()

    df_count_total <- df_count_total |>
      left_join(df_grid |> pivot_wider(names_from = y, values_from = value),
                by = vars_x)
    
    upper_left <- ggplot(data.frame(x = 3, y = 2), aes(x, y)) +
      geom_text(label = if (isTRUE(y.title)) "Intersection size" else y.title, angle = 90) +
      scale_x_continuous(limits = c(0, 3)) +
      scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank()) +
      labs(tag = tag)
    
    upper_right <- df_count_total |>
      plot2(x = x,
            y = out,
            category = if (has_category(df)) str2lang(get_category_name(df)) else NULL,
            type = "col",
            stacked = TRUE,
            x.title = NULL,
            y.title = NULL,
            x.remove = TRUE,
            y.limits = y.limits,
            y.expand = y.expand,
            colour = colour,
            colour_fill = colour,
            title = title,
            subtitle = subtitle,
            x.sort = x_sorted,
            title.linelength = title.linelength,
            title.colour = title.colour,
            subtitle.linelength = subtitle.linelength,
            subtitle.colour = subtitle.colour,
            colour_opacity = colour_opacity,
            datalabels = FALSE,
            decimal.mark = decimal.mark,
            big.mark = big.mark,
            text_factor = text_factor,
            font = font,
            theme = theme,
            background = background,
            markdown = markdown,
            width = width
      )  +
      theme(axis.ticks.x = element_blank(),
            axis.line = element_blank())
    
    
    layers <- tryCatch(upper_right@layers, error = function(e) upper_right$layers)
    colour_layers <- vapply(FUN.VALUE = character(1),
                            layers,
                            function(x) as.character(ifelse(length(x$aes_params$colour) == 0, NA, x$aes_params$colour)))
    colour_layers <- colour_layers[!is.na(colour_layers)][1]
    if (is.na(colour_layers)) {
      colour_layers <- get_colour(getOption("plot2.colour_font_primary", colour))
    }
    
    suppressMessages(
      lower_left <- lower_left_df |>
        plot2(x = y,
              y = n,
              type = "col",
              colour = colour_layers,
              width = width,
              x.sort = y_sorted,
              horizontal = TRUE,
              datalabels = FALSE,
              x.title = NULL,
              y.title = NULL,
              x.remove = TRUE,
              text_factor = text_factor,
              font = font,
              theme = theme,
              background = background,
              markdown = markdown
              ) +
        scale_y_continuous(transform = "reverse",
                           name = if (isTRUE(x.title)) NULL else x.title,
                           expand = expansion(mult = c(0.25, 0))) +
        scale_x_discrete(expand = c(0.05, 0.05)) +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = margin(0, 0, 0, 0),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.line.y = element_blank())
    )
    
    lower_right_seqments_df <- df_grid |>
      filter(value == TRUE, y %in% y_sorted) |>
      group_by(x) |>
      summarise(y_min = min(match(y, y_sorted)),
                y_max = max(match(y, y_sorted))) |>
      mutate(y_min_label = y_sorted[y_min],
             y_max_label = y_sorted[y_max])
    lower_right_seqments_df$x <- factor(lower_right_seqments_df$x, levels = x_sorted, ordered = TRUE)
    
    df_grid$x <- factor(df_grid$x, levels = x_sorted, ordered = TRUE)
    df_grid$y <- factor(df_grid$y, levels = rev(y_sorted), ordered = TRUE)
    df_grid$point_colour <- ifelse(df_grid$value, colour_layers, "grey90")
    
    lower_right <- ggplot(df_grid,
                          aes(x = x,
                              y = y)) +
      geom_point(colour = df_grid$point_colour, size = size) +
      geom_segment(data = lower_right_seqments_df,
                   aes(x = x, xend = x, y = y_min_label, yend = y_max_label),
                   colour = colour_layers,
                   linewidth = linewidth,
                   inherit.aes = FALSE) +
      scale_y_discrete(expand = c(0.05, 0.05)) +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(0, 0, 0, 0),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(hjust = 0.5),
            axis.title = element_blank()) +
      labs(caption = caption)
    
    out <- patchwork::wrap_plots(
      upper_left,
      upper_right,
      lower_left,
      lower_right,
      ncol = 2,
      nrow = 2,
      widths = c(0.2, 0.8),
      heights = c(0.7, 0.3))
    
    if (isTRUE(print)) {
      print(out)
    } else {
      return(out)
    }
  }
  
  
  
  # apply taxonomic italics ----
  if (isTRUE(x.lbl_taxonomy) && isTRUE(markdown) && isTRUE("AMR" %in% rownames(utils::installed.packages()))) {
    df <- validate_taxonomy(df)
    if (all(get_x(df) %like% "^paste\\(")) {
      # so x has taxonomic values
      if (!is.null(x.labels)) {
        plot2_warning("Ignoring ", font_blue("x.labels"), " since ", font_blue("x.lbl_taxonomy = TRUE"))
      }
      x.labels <- function(l) parse(text = l)
    }
  }
  
  # validate type ----
  type <- validate_type(type = type, df = df) # this will automatically determine the type if is.null(type)
  if (geom_is_line_or_area(type) && type_backup != "linedot" && !is.null(size)) {
    plot2_warning("'size' has been replaced with 'linewidth' for line/area types, assuming ", font_blue("linewidth = ", size, collapse = NULL))
    linewidth <- size
  }
  if (has_y_secondary(df)) {
    y_secondary.type <- suppressMessages(validate_type(type = y_secondary.type, df = df))
  }
  # transform data if not a continuous geom but group sizes are > 1
  if (any(group_sizes(df) > 1) && !geom_is_continuous(type) && type_backup != "sankey") {
    if (identical(type_backup, "barpercent")) {
      plot2_message("Summarising values for ", font_blue("type = \"barpercent\""), " using ",
                    font_blue(paste0("summarise_function = ", dots$`_summarise_fn_name`)))
    }
    df <- summarise_data(df = df,
                         summarise_function = summarise_function,
                         decimal.mark = decimal.mark,
                         big.mark = big.mark,
                         datalabels.round = datalabels.round,
                         datalabels.format = datalabels.format,
                         y.percent = y.percent)
  }
  
  # various cleaning steps ----
  
  if (isTRUE(zoom)) {
    x.zoom <- TRUE
    y.zoom <- TRUE
    if (is.null(x.expand)) {
      x.expand <- c(0.15, 0.15)
    }
    if (is.null(y.expand)) {
      y.expand <- c(0.15, 0.15)
    }
  }
  
  # check if markdown is required
  markdown <- validate_markdown(markdown, x.title, y.title, legend.title, title, subtitle, tag, caption, df)
  
  # remove datalabels in continuous geoms
  if (has_datalabels(df) && isTRUE(misses_datalabels) &&
      (geom_is_continuous(type) | type %like% "path|line") &&
      !type %in% c("geom_sf", "geom_tile", "geom_raster", "geom_rect")) {
    df <- df |> select(-`_var_datalabels`)
  }
  if (!isTRUE(misses_y) && geom_is_continuous_x(type)) {
    plot2_message("Ignoring ", font_blue("y"), " for plot type ", font_blue(gsub("geom_", "", type)))
    df$`_var_y` <- df$`_var_x`
  }
  
  # remove x from sf geom
  if (type == "geom_sf") {
    df <- df |> select(-`_var_x`)
  }
  
  # check requirements for Sankey plots
  if (type_backup == "sankey") {
    if (isTRUE(misses_x) || (.data |> select({{ x }}) |> NCOL()) < 2 ) {
      stop("Sankey plots require an x axis of at least two variables", call. = FALSE)
    }
    if (!has_y(df)) {
      stop("Sankey plots require `y`", call. = FALSE)
    }
    if (!has_category(df)) {
      stop("Sankey plots require `category`, as that will indicate the flows", call. = FALSE)
    }
  }
  
  # keep only one of `stacked` and `stacked_fill`
  if (isTRUE(stacked) && isTRUE(stacked_fill)) {
    plot2_warning("Ignoring ", font_blue("stacked = TRUE"), " since ", font_blue("stacked_fill = TRUE"))
    stacked <- FALSE
  }
  
  # set default size and width ----
  size <- validate_size(size = size, type = type, type_backup = type_backup)
  width <- validate_width(width = width, type = type)
  linewidth <- validate_linewidth(linewidth = linewidth, type = type, type_backup = type_backup)
  
  # generate colour vectors ----
  # keep original ggplot2 colours if they have not been set
  if (type == "geom_sf") {
    original_colours <- identical(colour_fill, "ggplot2") || is.null(colour_fill)
  } else {
    original_colours <- (is.null(colour) || identical(colour, "ggplot2")) &&
      (identical(colour_fill, "ggplot2") || (is.null(colour_fill) && (is.null(colour) || identical(colour, "ggplot2"))))
  }
  if (!any(c("colour", "fill") %in% category.type)) {
    # "colour" and "fill" are not in category.type, so do not set colours regardless of the `colour` argument
    original_colours <- TRUE
  }
  
  if (has_category(df) && !is.null(category.focus)) {
    category.focus <- category.focus[1L]
    # check if value is actually in category
    if (!category.focus %in% get_category(df) && !is.numeric(category.focus)) {
      plot2_warning("Value \"", category.focus, "\" not found in ", font_blue("category"))
    } else {
      category_unique <- sort(unique(get_category(df)))
      if (is.numeric(category.focus)) {
        # support `category.focus = 3` to choose the third value
        category.focus <- category_unique[category.focus]
      }
      cols <- rep(as.character(get_colour("grey85")), length(category_unique))
      nms <- as.character(category_unique)
      cols[which(nms == category.focus)] <- get_colour(colour[1L])
      colour <- stats::setNames(cols, nms)
    }
  }
  if (has_y_secondary(df)) {
    y_secondary.colour <- get_colour(y_secondary.colour)[1L]
    y_secondary.colour_fill <- get_colour(y_secondary.colour_fill)[1L]
  }
  # Note that this will be not be used if colour == "ggplot2" or colour_fill == "ggplot2"
  cols <- validate_colour(df = df,
                          type = type,
                          type_backup = type_backup,
                          colour = colour,
                          colour_fill = colour_fill,
                          colour_opacity = colour_opacity,
                          misses_colour_fill = misses_colour_fill,
                          horizontal = horizontal)
  
  # generate mapping / aesthetics ----
  # IMPORTANT: in this part, the mapping will be generated anonymously, e.g. as `_var_x` and `_var_category`;
  # this is done for convenience - this is restored before returning the `ggplot` object in the end
  if (type != "geom_sf" && !geom_is_continuous_x(type)) {
    # histograms etc. have a continuous x variable, so only set y if not a histogram-like
    mapping <- aes(y = `_var_y`, group = 1)
  } else {
    mapping <- aes()
    if (misses_x.zoom) {
      # this also sets x.limits to c(NA, NA) for histograms in validate_x_scale()
      x.zoom <- TRUE
      x.expand <- 0
    }
  }
  if (has_x(df)) {
    mapping <- utils::modifyList(mapping, aes(x = `_var_x`, group = `_var_x`))
  }
  if (has_category(df)) {
    mapping <- utils::modifyList(mapping, aes(group = `_var_category`))
    mapping <- utils::modifyList(mapping, rlang::syms(stats::setNames(rep("_var_category", length(category.type)), category.type)))
    
    if (type == "geom_sf") {
      # no colour in sf's
      mapping <- utils::modifyList(mapping, aes(colour = NULL))
      # # and set sf column
      # mapping <- setup_aes(mapping, geometry = dots$`_sf.column`)
    }
  }
  if (geom_is_continuous(type) && !geom_is_line_or_area(type) && has_category(df)) {
    # remove the group from the mapping
    mapping <- utils::modifyList(mapping, aes(group = NULL))
  }
  if ((geom_is_line_or_area(type) || geom_is_continuous_x(type) || geom_has_only_colour(type)) && !has_category(df)) {
    # exception for line plots without colour/fill, force group = 1
    mapping <- utils::modifyList(mapping, aes(group = 1))
  }
  # manual setting for Sankey plots
  if (type_backup == "sankey") {
    mapping <- aes(x = `_sankey_x`,
                   id = `_sankey_id`,
                   split = `_sankey_split`,
                   value = `_var_y`)
  }
  
  # generate ggplot ----
  p <- ggplot(data = df, mapping = mapping)
  
  # generate geom ----
  if (type == "geom_boxplot") {
    # first add the whiskers, the actual boxplot will be added with `generate_geom()` below
    if (isTRUE(original_colours)) {
      p <- p +
        stat_boxplot(geom = "errorbar",
                     coef = 1.5, # 1.5 * IQR
                     width = width * ifelse(has_category(df), 1, 0.75),
                     linewidth = linewidth)
    } else {
      p <- p +
        stat_boxplot(geom = "errorbar",
                     coef = 1.5, # 1.5 * IQR
                     width = width * ifelse(has_category(df), 1, 0.75),
                     linewidth = linewidth,
                     colour = cols$colour)
    }
  }
  if (type_backup == "sankey") {
    rlang::check_installed("ggforce")
    p <- p +
      ggforce::geom_parallel_sets(aes(fill = `_var_category`),
                                  alpha = sankey.alpha,
                                  # whitespace between nodes
                                  sep = sankey.node_whitespace,
                                  # width for flows, i.e., category
                                  axis.width = sankey.node_width) +
      ggforce::geom_parallel_sets_axes(fill = get_colour(datalabels.colour_fill[1]),
                                       colour = get_colour(datalabels.colour[1]),
                                       # whitespace between nodes
                                       sep = sankey.node_whitespace,
                                       # width of nodes
                                       axis.width = sankey.node_width) +
      ggforce::geom_parallel_sets_labels(colour = get_colour(datalabels.colour[1]),
                                         size = datalabels.size,
                                         # whitespace between labels in nodes
                                         sep = sankey.node_whitespace,
                                         angle = ifelse(isTRUE(horizontal), 0, -90))
    if (is.null(sankey.remove_axes)) {
      # make the default TRUE, but give message so users will know which argument to set
      sankey.remove_axes <- TRUE
      plot2_message("Assuming ", font_blue("sankey.remove_axes = TRUE"))
    }
  } else {
    p <- p +
      generate_geom(type = type,
                    df = df,
                    stacked = stacked,
                    stacked_fill = stacked_fill,
                    horizontal = horizontal,
                    width = width,
                    size = if ("size" %in% category.type) NULL else size,
                    linetype = if ("linetype" %in% category.type) NULL else linetype,
                    linewidth = if ("linewidth" %in% category.type) NULL else linewidth,
                    reverse = reverse,
                    na.rm = na.rm,
                    violin_scale = violin_scale,
                    jitter_seed = jitter_seed,
                    binwidth = binwidth,
                    cols = cols,
                    original_colours = original_colours,
                    dots_geom = dots_geom)
    
    # add secondary y axis if available
    if (has_y_secondary(df)) {
      if (y_secondary.type == "geom_boxplot") {
        # first add the whiskers
        p <- p +
          stat_boxplot(geom = "errorbar",
                       mapping = utils::modifyList(mapping, aes(y = `_var_y_secondary`)),
                       coef = 1.5, # 1.5 * IQR
                       width = width * ifelse(has_category(df), 1, 0.75),
                       lwd = size,
                       colour = y_secondary.colour)
      }
      p <- p +
        generate_geom(type = y_secondary.type,
                      df = df,
                      stacked = stacked,
                      stacked_fill = stacked_fill,
                      horizontal = horizontal,
                      width = width,
                      size = size,
                      linetype = linetype,
                      linewidth = linewidth,
                      reverse = reverse,
                      na.rm = na.rm,
                      violin_scale = violin_scale,
                      jitter_seed = jitter_seed,
                      binwidth = binwidth,
                      cols = list(colour = y_secondary.colour,
                                  colour_fill = y_secondary.colour_fill),
                      original_colours = original_colours,
                      dots_geom = dots_geom,
                      mapping = utils::modifyList(mapping, aes(y = `_var_y_secondary`)))
    }
  }
  
  if (is.null(smooth) && type == "geom_histogram") {
    plot2_message("Assuming ", font_blue("smooth = TRUE"), " for ", font_blue("type = \"histogram\""))
    smooth <- TRUE
  }
  if (isTRUE(smooth)) {
    if (type == "geom_histogram") {
      # add a density count
      set_binwidth <- p$layers[[1]]$stat_params$binwidth
      p <- p +
        do.call(geom_density,
                c(list(mapping = aes(y = after_stat(count) * set_binwidth),
                       colour = ifelse(is.null(smooth.colour), get_colour(colour), get_colour(smooth.colour)),
                       linetype = ifelse(is.null(smooth.linetype), 1, smooth.linetype),
                       linewidth = ifelse(is.null(smooth.linewidth), 0.5, smooth.linewidth),
                       na.rm = na.rm)))
    } else if (is.null(smooth.colour)) {
      p <- p |>
        add_smooth(formula = smooth.formula,
                   se = smooth.se,
                   method = smooth.method,
                   level = smooth.level,
                   alpha = smooth.alpha,
                   linetype = smooth.linetype,
                   linewidth = smooth.linewidth,
                   move = -1)
    } else {
      p <- p |>
        add_smooth(colour = smooth.colour,
                   fill = smooth.colour,
                   formula = smooth.formula,
                   se = smooth.se,
                   method = smooth.method,
                   level = smooth.level,
                   alpha = smooth.alpha,
                   linetype = smooth.linetype,
                   linewidth = smooth.linewidth,
                   move = -1)
    }
  }
  # add axis labels ----
  if (has_category(df)) {
    labs_set <- as.list(stats::setNames(rep(get_category_name(df), length(category.type)), category.type))
  } else {
    labs_set <- list()
  }
  labs_set$x <- get_x_name(df)
  labs_set$y <- get_y_name(df)
  labs_set <- labs_set[names(labs_set) %in% names(p$mapping)]
  p <- p +
    do.call(labs, labs_set) # will return NULL if not available, so always works
  if (geom_is_continuous_x(type)) {
    if (type %like% "density") {
      p <- p +
        labs(y = "Density")
      if (misses_y.percent) {
        y.percent <- TRUE
      }
    } else {
      p <- p +
        labs(y = "Frequency")
    }
  }
  
  # add the right scales ----
  font <- validate_font(font)
  if (has_category(df) && (is.numeric(get_category(df)) || is_date(get_category(df)))) {
    p <- p +
      validate_category_scale(values = get_category(df),
                              type = type,
                              cols = cols,
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
                              stacked_fill = stacked_fill,
                              legend.nbin = legend.nbin,
                              legend.barheight = legend.barheight,
                              legend.barwidth = legend.barwidth,
                              legend.reverse = legend.reverse,
                              legend.nrow = legend.nrow,
                              legend.position = legend.position,
                              decimal.mark = decimal.mark,
                              big.mark = big.mark,
                              font = font,
                              colour_fill = colour_fill,
                              original_colours = original_colours)
  } else if (type != "geom_sf") {
    category_txt <- get_category(df)
    if (is.null(category.labels) &&
        any(category_txt %like% "[*]+.+[*]+" |
            category_txt %like% "[a-zA-Z0-9,.-]_[{].+[}]" |
            category_txt %like% "[a-zA-Z0-9,.-] ?\\^ ?[{].+[}]" |
            category_txt %like% "[a-zA-Z0-9,.-] ?\\^ ?[a-zA-Z0-9,._-]" |
            category_txt %like% "<sup>.+</sup>" |
            category_txt %like% "<sub>.+</sub>" |
            category_txt %like% "[$]", na.rm = TRUE)) {
      plot2_message("The ", font_blue("category"), " seems to contain markdown, assuming ", font_blue("category.labels = md_to_expression"))
      category.labels <- md_to_expression
    }
    
    for (cat_type in category.type) {
      if (original_colours == TRUE || !cat_type %in% c("colour", "fill")) {
        # these scale functions do not have 'values' set
        discrete_scale <- getExportedValue(paste0("scale_", cat_type, "_discrete"), ns = asNamespace("ggplot2"))
        p <- p +
          discrete_scale(labels = if (is.null(category.labels)) waiver() else category.labels,
                         limits = if (is.null(names(if (cat_type == "fill") cols$colour_fill else cols$colour))) {
                           NULL
                         } else {
                           # remove unneeded labels
                           base::force
                         })
      } else {
        manual_scale <- getExportedValue(paste0("scale_", cat_type, "_manual"), ns = asNamespace("ggplot2"))
        p <- p +
          manual_scale(values = if (cat_type == "fill") cols$colour_fill else cols$colour,
                       labels = if (is.null(category.labels)) waiver() else category.labels,
                       limits = if (is.null(names(cols$colour))) {
                         NULL
                       } else {
                         # remove unneeded labels
                         base::force
                       })
      }
    }
    # hack the possibility to print values as expressions
    if (identical(category.labels, md_to_expression)) {
      if (geom_has_only_colour(type)) {
        p$mapping <- utils::modifyList(p$mapping, aes(fill = NULL))
      } else {
        p$mapping <- utils::modifyList(p$mapping, aes(colour = NULL))
      }
    }
  }
  if (!type %in% c("geom_sf", "geom_tile", "geom_raster", "geom_rect")) {
    if (has_x(df)) {
      if (isTRUE(x.mic)) {
        rlang::check_installed("AMR")
        p <- p + AMR::scale_x_mic(drop = x.drop, mic_range = x.limits)
      } else {
        p <- p + 
          validate_x_scale(df,
                           values = get_x(df),
                           x.date_breaks = x.date_breaks,
                           x.date_labels = x.date_labels,
                           x.breaks = x.breaks,
                           x.expand = x.expand,
                           x.labels = x.labels,
                           x.n_breaks = x.n_breaks,
                           x.limits = x.limits,
                           x.position = x.position,
                           x.transform = x.transform,
                           x.drop = x.drop,
                           x.zoom = x.zoom,
                           facet.fixed_x = facet.fixed_x,
                           decimal.mark = decimal.mark,
                           big.mark = big.mark,
                           horizontal = horizontal,
                           type_backup = type_backup)    
      }
    } else {
      # no x
      p <- p +
        scale_x_discrete(labels = NULL, breaks = NULL, drop = x.drop)
    }
    if (has_y(df)) {
      p_added_y <- p +
        validate_y_scale(df = df,
                         type = type,
                         y.24h = y.24h,
                         y.age = y.age,
                         y.scientific = y.scientific,
                         y.breaks = y.breaks,
                         y.n_breaks = y.n_breaks,
                         y.expand = y.expand,
                         y.labels = y.labels,
                         y.limits = y.limits,
                         y.percent = y.percent,
                         y.percent_break = y.percent_break,
                         misses_y.percent_break = misses_y.percent_break,
                         y.position = y.position,
                         y.transform = y.transform,
                         y.zoom = y.zoom,
                         stacked = stacked,
                         stacked_fill = stacked_fill,
                         facet.fixed_y = facet.fixed_y,
                         decimal.mark = decimal.mark,
                         big.mark = big.mark,
                         add_y_secondary = FALSE)
    }
    if (has_y_secondary(df)) {
      # add a secondary y axis
      if (isTRUE(y_secondary.title)) {
        y_secondary.title <- validate_title(get_y_secondary_name(df), markdown = isTRUE(markdown), df = df)
      }
      p <- p +
        validate_y_scale(df = df,
                         type = type,
                         y.24h = y.24h,
                         y.age = y.age,
                         y.scientific = y.scientific,
                         y.breaks = y.breaks,
                         y.n_breaks = y.n_breaks,
                         y.expand = y.expand,
                         y.labels = y.labels,
                         y.limits = y.limits,
                         y.percent = y.percent,
                         y.percent_break = y.percent_break,
                         misses_y.percent_break = misses_y.percent_break,
                         y.position = y.position,
                         y.transform = y.transform,
                         y.zoom = y.zoom,
                         stacked = stacked,
                         stacked_fill = stacked_fill,
                         facet.fixed_y = facet.fixed_y,
                         decimal.mark = decimal.mark,
                         big.mark = big.mark,
                         add_y_secondary = TRUE,
                         # this get the breaks from the primary y axis (requires ggplot version >= 3.3.0):
                         y_secondary.breaks = ggplot_build(p_added_y)$layout$panel_params[[1]]$y$breaks,
                         # additional properties for secondary y axis:
                         y_secondary.title = y_secondary.title,
                         y_secondary.scientific = y_secondary.scientific,
                         y_secondary.percent = y_secondary.percent,
                         y_secondary.labels = y_secondary.labels,
                         markdown = markdown)
    } else {
      # add the y axis without secondary axis
      p <- p_added_y
    }
  }
  
  # validate theme and add markdown support ----
  p <- p + 
    validate_theme(theme = theme,
                   type = type,
                   background = background,
                   text_factor = text_factor,
                   font = font,
                   horizontal = horizontal,
                   x.remove = x.remove,
                   y.remove = y.remove,
                   x.lbl_angle = x.lbl_angle,
                   x.lbl_align = x.lbl_align,
                   x.lbl_italic = x.lbl_italic,
                   facet.fill = facet.fill,
                   facet.bold = facet.bold,
                   facet.italic = facet.italic,
                   facet.size = facet.size,
                   facet.margin = facet.margin,
                   legend.italic = legend.italic,
                   title.colour = title.colour,
                   subtitle.colour = subtitle.colour,
                   has_y_secondary = has_y_secondary(df),
                   has_category = has_category(df),
                   col_y_primary = cols$colour[1L],
                   col_y_secondary = y_secondary.colour,
                   sankey.remove_axes = sankey.remove_axes)
  
  # add titles ----
  if (!misses_title) p <- p + labs(title = title)
  if (!misses_subtitle) p <- p + labs(subtitle = subtitle)
  if (!misses_tag) p <- p + labs(tag = tag)
  if (!misses_caption) p <- p + labs(caption = caption)
  
  if (isTRUE(x.title)) {
    x.title <- validate_title(get_x_name(df), markdown = isTRUE(markdown), df = df)
  }
  p <- p + labs(x = x.title)
  
  if (isTRUE(y.title)) {
    if (type == "geom_histogram") {
      y.title <- "Count"
    } else {
      y.title <- validate_title(get_y_name(df), markdown = isTRUE(markdown), df = df)
    }
  }
  p <- p + labs(y = y.title)
  
  if (has_category(df)) {
    # legend
    if (is.null(legend.title) && data_is_numeric(get_category(df))) {
      legend.title <- TRUE
    }
    if (isTRUE(legend.title)) {
      legend.title <- validate_title(get_category_name(df), markdown = isTRUE(markdown), df = df)
    }
    for (cat in category.type) {
      if (cat %in% names(mapping)) {
        p <- p + do.call(labs, stats::setNames(list(legend.title), cat))
      }
    }
    if (type_backup == "sankey") {
      p <- p + labs(fill = legend.title)
    }
    if ("group" %in% names(mapping)) {
      p <- p + labs(group = legend.title)
    }
  }
  
  # set legend ----
  if (is.null(legend.position)) {
    if (has_category(df) && data_is_numeric(get_category(df))) {
      legend.position <- "right"
    } else {
      legend.position <- "top"
    }
  }
  legend.position <- validate_legend.position(legend.position)
  p <- p + ggplot2::theme(legend.position = legend.position)
  
  if (!(has_category(df) && is.numeric(get_category(df)))) {
    # only change this when there is no guide_colourbar(), see validate_category_scale()
    if (!is.null(legend.reverse)) {
      for (cat in category.type) {
        if (cat %in% names(mapping)) {
          p <- p +
            do.call(guides, stats::setNames(list(guide_legend(reverse = isTRUE(legend.reverse), nrow = legend.nrow)), cat))
        }
      }
    }
    if (isTRUE(horizontal)) {
      if (legend.position %in% c("top", "bottom") &&
          validate_sorting(category.sort, horizontal = horizontal) %unlike% "freq") {
        # reverse legend items when on top or bottom, but not when sorting is freq, freq-asc or freq-desc
        for (cat in category.type) {
          if (cat %in% names(mapping)) {
            p <- p +
              do.call(guides, stats::setNames(list(guide_legend(reverse = TRUE, nrow = legend.nrow)), cat))
          }
        }
      }
    }
  } else if (has_category(df) && !is.null(legend.nrow)) {
    for (cat in category.type) {
      if (cat %in% names(mapping)) {
        p <- p +
          do.call(guides, stats::setNames(list(guide_legend(nrow = legend.nrow)), cat))
      }
    }
  }
  
  # set facet ----
  if (has_facet(df)) {
    # this must follow after setting the y scale, since plot2_env$facet.fixed_y might have changed
    p <- p +
      validate_facet(df = df,
                     type = type,
                     facet.repeat_lbls_x = facet.repeat_lbls_x,
                     facet.repeat_lbls_y = facet.repeat_lbls_y,
                     facet.fixed_y = plot2_env$facet.fixed_y,
                     facet.relative = facet.relative,
                     facet.drop = facet.drop,
                     facet.nrow = facet.nrow,
                     facet.position = facet.position,
                     horizontal = horizontal)
  }
  
  # set datalabels ----
  if (has_datalabels(df) && type != "geom_blank") {
    p <- set_datalabels(p = p,
                        df = df,
                        type = type,
                        width = width,
                        stacked = stacked,
                        stacked_fill = stacked_fill,
                        datalabels.colour_fill = datalabels.colour_fill,
                        datalabels.colour = datalabels.colour,
                        datalabels.size = datalabels.size,
                        datalabels.angle = datalabels.angle,
                        datalabels.lineheight = datalabels.lineheight,
                        datalabels.centroid = dots$`_datalabels.centroid`,
                        font = font,
                        reverse = reverse,
                        horizontal = horizontal,
                        misses_datalabels = misses_datalabels,
                        markdown = markdown)
  }
  
  # turn plot horizontal if required ----
  # up until this point, a lot has been done already for `horizontal`.
  # such as switching some x and y axis properties of the theme
  if (type != "geom_sf" && (isTRUE(horizontal) || has_datalabels(df))) {
    x.limits <- x.limits %or% plot2_env$x_lim
    y.limits <- y.limits %or% plot2_env$y_lim
    put_clip_off <- has_datalabels(df) && type != "geom_sf" && (!is.null(p$labels$title) || !is.null(p$labels$subtitle) || has_facet(df))
    if (isTRUE(horizontal)) {
      p <- p +
        coord_flip(xlim = x.limits,
                   ylim = y.limits,
                   expand = TRUE,
                   # add off-clipping since datalabels might be outside plotted range
                   clip = ifelse(put_clip_off, "off", "on"))
    } else if (put_clip_off && has_datalabels(df)) {
      p <- p +
        coord_cartesian(xlim = x.limits,
                        ylim = y.limits,
                        expand = TRUE,
                        default = TRUE,
                        # add off-clipping since datalabels might be outside plotted range
                        clip = "off")
    }
  }
  
  # additional geoms for certain types ----
  # linedot
  if (type_backup == "linedot") {
    p <- p |> 
      add_point(colour = ifelse(!is.null(background), background, "white"), size = size * 5) |> 
      add_point(size = size * 2)
  }
  # dumbbell
  if (type_backup == "dumbbell") {
    if (!has_category(df) || n_distinct(get_category(df)) != 2) {
      stop("Dumbbell plots can only be used if the category ",
           ifelse(has_category(df), paste0("(", get_category_name(df), ") "), "") ,
           "is set with two distinct values",
           ifelse(has_category(df), paste0(" - it now has ", digit_to_text(n_distinct(get_category(df))), "."), ""),
           call. = FALSE)
    }
    dumbbell_segment <- df |> 
      group_by(across(c(
        # x must get a name to be used in geom_segment(aes(...))
        x_axis = get_x_name(df),
        # facet must not be named here so that it will be identical to the current facet name,
        # which is required to make facets work with dumbbell plots
        get_facet_name(df))),
        .drop = FALSE) |>
      summarise(y_min = suppressWarnings(min(`_var_y`, na.rm = TRUE)),
                y_max = suppressWarnings(max(`_var_y`, na.rm = TRUE)),
                .groups = "drop") |> 
      filter(!is.infinite(abs(y_min)) & !is.infinite(abs(y_max)))
    
    p <- p +
      geom_segment(data = dumbbell_segment,
                   mapping = aes(x = x_axis, xend = x_axis,
                                 y = y_min, yend = y_max),
                   linewidth = linewidth,
                   linetype = linetype,
                   colour = "grey80",
                   inherit.aes = FALSE)
    # move the segment one layer down, so geom_point will be on top
    p <- p |> 
      move_layer(layer = length(p$layers), move = -1)
  }
  
  # restore mapping to original names ----
  # this will replace e.g. `_var_x` and `_var_category` in the mapping and remove them from the data
  p <- restore_mapping(p = p,
                       df = df)
  if (type_backup == "sankey") {
    p$data <- p$data |> select(-get_x_name(df))
  }
  # be sure to end with a tibble - the as_tibble() function also has some consistency checks built-in
  # but only when it's nothing more than a data.frame without row names
  if (identical(class(p$data), "data.frame") && identical(rownames(p$data), as.character(seq_len(nrow(p$data))))) {
    p$data <- as_tibble(p$data)
  }
  
  # return plot ----
  if (isTRUE(print)) {
    print(p)
  } else {
    p
  }
}

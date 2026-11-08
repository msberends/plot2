# plot2 1.99.0.9000

* Fewer hard dependencies: `broom` and `cleaner` moved to Suggests (`broom` is needed only to plot model objects that `ggplot2::fortify()` cannot handle) and `crayon` was dropped in favour of `cli`
* `cli`, `rlang`, `scales`, `grid` and `grDevices` are now listed in Imports, as plot2 always uses them; `stringr` is no longer needed for sorting
* Messages and warnings of `plot2()` are now R conditions. Messages (class `plot2_message`, and `plot2_caution` for cautions) are still printed only in interactive sessions, now grouped at the end of the call in the order they occurred, and can be caught or hidden with `suppressMessages()`; `options(plot2.silent = TRUE)` now works. Warnings (such as "No observations" or values that could not be plotted) are now R warnings of class `plot2_warning`, also in scripts, R Markdown and Quarto
* Fixed: internal state of one `plot2()` call could leak into the next call, or be wiped by a nested call (back-to-back and UpSet plots), which could give wrong axis titles and messages that belonged to an earlier plot
* `x.zoom` (and `zoom`) no longer sets numeric limits on a discrete x axis
* `x.zoom`, `y.zoom` and `zoom` now zoom to the data with ggplot2's usual 5% expansion (previously 50% for x and 25% for y), `zoom = TRUE` is identical to `x.zoom = TRUE, y.zoom = TRUE`, and it no longer expands beyond `y.limits`
* The help pages of `legend.position` and of the package options (`?plot2-options`) now state the actual defaults
* Main vignette: corrected several explanations that did not match their example code
* README: corrected the installation instructions (not on CRAN yet) and two examples that failed or did not do what the text said
* Help pages of `move_layer()`, the `plot2()` methods, `register_colour()` and `unregister_colour()` now have examples and return values; the `plot2()` methods page lists how each method changes the defaults
* `type = "beeswarm"` without the ggbeeswarm package now gives an install hint instead of 'geom_beeswarm() does not exist', and all type aliases and sorting values are documented
* Corrected typos and factual errors in the help pages of plot2(), add_type(), md_to_expression(), the package options and the netherlands data set
* The vignettes build when suggested packages (ggplot2movies, ggbeeswarm, ggforce, sf, patchwork, viridisLite) are not installed; the affected examples are then skipped.
* CI now also checks R 4.1 (the declared minimum) and a set-up with only the hard dependencies
* `x.complete`, `category.complete` and `facet.complete` now work for character and factor values (crossed with the other directions), and complete date-times at the observed period instead of per second
* The colour palette `"ggplot2"` (the default) now gives exactly the colours ggplot2 itself uses, also for more than one colour
* The help page of `get_colour()` no longer changes at every documentation run
* Scientific axis labels now use a multiplication sign (×) that renders on every device, instead of a console bullet; `euros()` and `dollars()` no longer use scientific notation for large values
* Violin plots no longer fail when groups have a single observation
* Without `y` and without a numeric column, `plot2()` now counts rows (`y = n()`) instead of stopping; Sankey plots can be drawn from raw rows
* `type = "barpercent"` without `y` now counts rows instead of summing the first numeric column
* `options(plot2.default_type = )` now accepts the same values as `type`, such as `"line"` and `"l"`
* `colour_opacity` now also applies to the default colours and to all built-in R palettes
* Subsetting a colour vector with `[` or `[[` now keeps the `colour` class
* `theme_minimal2()` text sizes are now plain numbers instead of grid units
* `theme_minimal2(...)` now changes only the given properties, like `+ theme(...)`
* `coord_spider()` radial labels now follow `start` and the theme's font family and colour
* `add_smooth(legend.value = )` now works on non-date x axes
* `move_layer()` with a positive `move` no longer duplicates or drops layers
* `add_errorbar()` now works for plots without a category, and with facets
* `plot2()` on sf data with `x` or `y` now gives the intended caution instead of an error
* `y_secondary.colour` and `y_secondary.colour_fill` now apply also when `colour` is not set
* `plot2(df, y = col)` no longer fails when the data have no other column to use as x
* `datalabels` given with an explicit point or line type are no longer silently dropped
* Numeric x and y with identical values now give a scatter plot instead of a histogram; a histogram is only chosen when x and y are the same column
* UpSet plots no longer use a soft-deprecated tidyselect idiom
* Box plots no longer use the `fatten` argument, which is deprecated in ggplot2 4.0.0
* The returned plot data no longer contain internal columns starting with `_`; datalabels and Sankey helper columns get readable names (`datalabels`, `sankey_x`, ...)
* Character and factor vectors given as a long expression, or named `y`, can now be plotted
* Sankey plots without two x variables or without `y` now give a clear error instead of a cryptic one, and UpSet plots accept a manual `x.sort` vector
* Density plots now get `Density` as y axis title instead of the name of the x variable
* `category.focus` now also works with the default colours; the focused category keeps its own colour
* Long titles with accented, Greek, Cyrillic or CJK characters are now wrapped like other titles
* `x.limits` now works for a discrete x axis: `x.limits = c("b", "a")` shows only these items, in this order
* `x.lbl_angle` and `x.lbl_align` now also apply to horizontal plots, where the x axis labels are on the vertical axis
* `summarise_function = NULL` now keeps all values instead of failing for duplicate x values, as documented
* `summarise_function` must now return a single value per group, with a clear error otherwise; groups whose summary is `NA` are no longer dropped silently
* A column named `n` in `x`, `category` or `facet` no longer breaks the summarising of `y`
* Fixed: back-to-back plots with small counts failed, and `y = c(2, 3)` was read as column positions
* Fixed: back-to-back plots failed when the facet was given as an expression, such as `facet = tolower(sex)`
* Fixed: a date-time (POSIXct) category produced thousands of nonsense legend breaks; a category with only a few dates now shows one legend entry per date
* Fixed: a factor y with data labels failed with an `nchar()` error
* Fixed: a category or datalabels column of only 0s or 1s was mistaken for `FALSE`/`TRUE`, and columns with missing values crashed `plot2()`
* Fixed: area plots with a category were shifted sideways; unstacked areas are now overlaid at their x values
* Fixed: giving fewer colours than categories failed with "Insufficient values in manual scale"; the colours are now recycled with a warning
* Fixed: a histogram of constant values was empty because the automatic binwidth was 0
* Fixed: `y.transform = "reverse"` applied `y.expand` to the wrong sides and in the wrong units
* Fixed: `category.percent = TRUE` failed for numeric categories
* Fixed: a logarithmic `category.transform` failed when the automatic colour scale started at 0
* Fixed: transformation objects such as `scales::transform_log10()` in `x.transform`, `y.transform` and `category.transform` caused errors
* Fixed: `x.mic = TRUE` failed with current versions of the AMR package
* Fixed: logarithmic and other transformed x axes got linear breaks
* Fixed: `horizontal = TRUE` with a numeric x removed all data points
* Fixed: `x.limits` on a numeric x announced `x.expand = 0` but still padded the axis, so it did not end at the given limits
* Fixed: an explicit 0 in `y.limits` was silently replaced by the lowest (negative) data value
* Fixed: percentage axes (and age and 24-hour axes) with negative values had breaks at odd values such as -28%, -18%, -8%
* Fixed: axes and colour bars for small whole numbers had no breaks below zero
* Error messages no longer show internal function calls
* Fixed: several short hex colours (such as `"#f0a"`) were all replaced by the first one, and invalid hex codes could pass validation
* Fixed: an invalid `dec_mark` or `big_mark` option no longer becomes the decimal or big mark; it gives an R warning and falls back to the default
* Fixed: UpSet plots with `print = TRUE` raised an error after printing; `print = TRUE` now always returns the plot invisibly
* Fixed: types that merely contain "bar" (such as `"errorbar"` and `"crossbar"`) were turned into horizontal columns
* Fixed: `theme = theme_bw` (a theme function instead of its name) failed in several plot types
* Fixed: `plot2()` no longer changes the random-number stream (`.Random.seed`); the tip about `options(plot2.silent = TRUE)` is now shown once per session
* Fixed: the message about rows removed with `na.rm = TRUE` was garbled ("rowss")
* Fixed: date-time (POSIXct) columns could be chosen as the automatic y variable, which crashed `plot2()`
* Support for ggplot2 4.0.0
* Support for dplyr 1.2.0
* Added Shiny app (`plot2::create_interactively()`), with file upload
* Added `upload_tab` argument to `create_interactively()`: a full-featured Upload tab supporting CSV, TSV, Excel (XLS/XLSX/ODS/XLSB), SPSS, Stata, SAS, RDS, Parquet, Feather, JSON, YAML, XML, and more, each with format-specific options (delimiter, sheet selection, encoding, NA strings, etc.). Added internal `plot2:::install_shiny_deps()` for administrators to install all Shiny dependencies at once.
* Support `y.transform = "reverse"`
* Support `legend.nrow`
* Support back-to-back (= tornado, = butterfly) plots
* Support spider (= radar) plots with `type = "spider"`, using the new coordinate system `coord_spider()`
* Support beeswarm plots with `type = "beeswarm"` (requires the ggbeeswarm package)
* Added `add_point()`, `add_errorbar()`, `add_smooth()`, `add_sf()` and `add_mapping()`; `add_type()` and the other `add_*()` functions support facets
* Added `colour_opacity` to set the opacity of the colours

# plot2 1.0.0

* Project rename from [our previous iteration](https://github.com/certe-medical-epidemiology/certeplot2).

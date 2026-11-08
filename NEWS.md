# plot2 1.99.0.9000

* `size`, `width` and `linewidth` now give a clear error when they are not a non-negative number, instead of failing only when the plot is printed.
* `type` with more than one value now warns that only the first is used, instead of silently using it.
* In back-to-back plots, each category has the same colour on both sides, also when a category is missing on one side, and the legend is shown once.
* Back-to-back plots now draw both halves on the same value scale, so equal bar lengths mean equal values; a user-set `y.limits` is kept.
* In `x.max_txt` and friends, `%p` is now the share of the total of `y` in the rest item (it was the share of rows); the documentation now states that `x.max_items` includes the rest item and that `%n` is the number of merged values
* create_interactively() has a new argument run: run = FALSE returns the app object, e.g. for deployment on a server; see the new section 'Deployment' in ?create_interactively
* create_interactively() refuses exports above 40 megapixels, and checks export sizes on the server
* The copy button of create_interactively() copies in the browser, so it also works when the app runs on a server; the app no longer needs the clipr and fansi packages
* create_interactively() shows plot2's messages also on a server, and shows the actual error message
* create_interactively() has inputs for the secondary axis
* create_interactively(): vectors such as y.limits = c(0, 10), formulas and label functions can be entered, and text that looks like a number stays text
* create_interactively() refers to passed data by its name in the generated code, handles data frames with non-syntactic names, and offers data frames from the global environment only in an interactive session
* create_interactively() no longer runs text from the browser as R code: the plot2() call is built from checked values, and evaluated apart from the global environment
* Uploading in create_interactively(): the encoding is chosen from a list of valid values, and .txt files are read with the chosen delimiter
* create_interactively() no longer accepts R serialisation files (.rds, .rda) for upload, since reading them can run code; pass such data with the data argument. With upload_tab = FALSE (the default) the app offers no way to upload files at all
* create_interactively() serves only a copy of the logo, no longer the whole folder that contains it
* Column names are no longer parsed as R code in back-to-back and UpSet plots, so data with unusual column names (such as `my col`) work, and column names can never run code
* `label_taxonomy()` italicises species names only after a genus or an abbreviated genus, so ordinary words stay upright
* `?plot2` has a new section 'Automatic choices' that lists every decision plot2 makes by itself, its rule and the argument that overrides it
* Much faster on large data: datalabels are formatted only for the rows that are drawn, e.g. 200,000 rows now take about 0.2 s instead of 1.5 to 3 s
* Arguments renamed to follow one naming rule (see 'Argument names' in `?plot2`): `colour_opacity` is now `colour_transparency` (and `get_colour(opacity)` is `transparency`), `x.lbl_angle`, `x.lbl_align` and `x.lbl_italic` are `x.label_*`, `facet.repeat_lbls_*` are `facet.repeat_labels_*`, `facet.fill` is `facet.colour_fill`, `sankey.*` are `sankey_*`, and the options `dec_mark` and `big_mark` are `plot2.decimal.mark` and `plot2.big.mark`. The old argument names give an error that names the new one
* New: `legend = FALSE` hides the legend; `stacked = "fill"` stacks to 100%; `label_taxonomy()` italicises microorganism names in any labels, e.g. `x.labels = label_taxonomy` (replacing `x.lbl_taxonomy`)
* `x.mic` was removed; add `+ AMR::scale_x_mic()` to the plot instead
* Giving both `category.title` and `legend.title` is now an error, since they are the same setting
* Automatic defaults are now `NULL` in the usage, and explicit values mean what they say: e.g. `datalabels = TRUE` always shows all labels, while the default `NULL` shows at most 25; `summarise_function = FALSE` (no longer `NULL`) turns off summarising; an explicit `y.percent_break` is kept as given
* `plot2()` on a matrix now says that `x`, `y` and `category` are ignored, instead of ignoring them silently
* In functions and loops, default titles show the column name instead of code such as `{{ col }}` or `.data[[v]]`
* The arguments of `plot2()` are now defined once; the methods take them from the generic, so they can no longer drift apart
* Spider plots (`type = "spider"`) now support facets, and keep their spider shape when datalabels are shown together with a title or facets
* `plot2()` on a plain numeric matrix now plots all columns (as categories) instead of only the first, and uses the names of named dimensions as titles
* `x.limits`, `y.limits` and `category.limits` must now be of length 2 (use `NA` for a limit that follows the data); a single value previously gave a zero-width axis without any data
* Numeric category scales now use the data range rounded to pretty values (e.g. 0 to 100 for values from 12 to 87) instead of an undocumented heuristic; with three colours and `category.midpoint`, the scale is symmetric around the midpoint
* `add_sf()` now uses `datalabels.colour` for its labels, works with any name of the geometry column, and no longer shifts labels by default (`datalabels.nudge_y = 0`; the former 2500 only suited the Dutch RD New projection)
* An invalid colour in `colour` now gives a warning and is replaced by grey on its own, keeping the other colours and their names (previously the whole vector became one grey); `register_colour()` gives an error for invalid colours, and registered colour names are matched case-insensitively
* `add_type()` now adds the layer when `data` is given to a plot without top-level data, warns when there is no data at all (instead of silently returning the plot unchanged), and supports beeswarm types
* Plot types without dedicated support (such as `"step"`) no longer receive arguments they do not accept, so ggplot2 no longer warns about unknown parameters; the notice that such a type is 'only loosely supported' is now a message instead of a warning
* `text_factor` now scales every text element by exactly that factor, also in themes with relative sizes such as `theme_bw()`
* A partial theme, such as `theme = theme(panel.background = element_blank())`, now works: it is added on top of `theme_minimal2()` and its elements take precedence
* Axis labels now switch to scientific notation automatically only from one billion (`1e9`) or for values below `0.0001`; large counts such as 2 500 000 are shown with big marks, and small values get more decimals instead of scientific notation
* With facets, the automatic choice between free and fixed y (and x) scales is now always reported in a message and documented, since it depends on the data
* `legend.position` now also accepts `FALSE` (no legend), `"inside"`, and two numbers between 0 and 1 to place the legend inside the plot, e.g. `legend.position = c(0.95, 0.95)`
* The secondary series of a column plot (`y_secondary`) is now a line with points by default, instead of a second set of bars drawn over the first
* Secondary y axis: the secondary series is now actually drawn on the scale of its axis (previously it could be drawn with its original values), an all-zero series no longer errors, and negative values are mapped correctly
* New argument `datalabels.percent_by`: `"%p"` datalabels are now percentages within each x value (so stacked bars sum to 100% per bar), or within each category, facet, or the total; previously they were always relative to the grand total of the plot. Without a category, the default gives percentages within each facet
* Numeric x values that look like years (2000 to 2050) or months (1 to 12) now stay numeric for continuous types such as `"point"` and `"line"`, and are only treated as years or months when they are whole numbers; columns of years without `type` work as before
* An unknown or ambiguous `type` now gives an error with suggestions ("did you mean 'boxplot'?") instead of silently using the first geom whose name contains the text; abbreviations such as `"hist"` must be the start of a geom name
* `plot2("name")` now looks up a data frame of that name only in the calling environment, not along the search path or in plot2's namespace; `plot2(numbers, y = values)` now uses the vector as x
* Passing `data =` to `plot2()` with anything other than a formula now gives a clear error instead of 'argument matches multiple formal arguments'
* The formula interface now accepts computed terms such as `log(mpg) ~ hp` or `I(hp + 1)`, a facet after `|` (`y ~ x + category | facet`), and works after a pipe: `mtcars |> plot2(mpg ~ wt)`
* Titles and category values with apostrophes ("Children's *E. coli*") no longer fail with markdown, a single dollar sign ("Costs in $") or currency amounts no longer turn a title into plotmath (plotmath needs `$...$` as in Pandoc), and markdown is no longer detected across separate titles
* Column names and values containing regular expression characters (such as `rate (%)` or `n[1`) no longer break `plot2()`, and case-insensitive matching no longer changes the meaning of patterns such as `\D`
* Dates in legends and on the x axis now follow the system locale instead of Dutch; the new option `plot2.language` (`"en"` or `"nl"`) sets the language of month names and of the words plot2 generates (automatic titles, `y.24h`, `y.age`), which no longer depend on the machine's locale. Changes of the time locale are always restored
* Date formats such as `x.date_labels = "Week ww"` keep words intact (previously "%Veek"), text in single quotes is kept literally, quarters (`q`, `qq`) also work on the x axis, month formats return text like all other formats, and time zones are kept when years are unified
* `lubridate` is no longer needed
* Custom fonts: Google Fonts are now downloaded into plot2's user cache folder (`tools::R_user_dir("plot2", "cache")`) instead of into the installed showtextdb package, and fonts not used for 90 days are removed from it. A custom font no longer switches on showtext for the whole session: it is used only while that plot is drawn, so other plots are unaffected. The packages `showtext`, `showtextdb` and `sysfonts` moved to Suggests and are needed only for custom fonts
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

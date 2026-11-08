# plot2 1.99.0.9000

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

# plot2 1.0.0

* Project rename from [our previous iteration](https://github.com/certe-medical-epidemiology/certeplot2).

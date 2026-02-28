# plot2 1.99.0.9000

* Support for ggplot2 4.0.0
* Support for dplyr 1.2.0
* Added Shiny app (`plot2::create_interactively()`), with file upload
* Added `upload_tab` argument to `create_interactively()`: a full-featured Upload tab supporting CSV, TSV, Excel (XLS/XLSX/ODS/XLSB), SPSS, Stata, SAS, RDS, Parquet, Feather, JSON, YAML, XML, and more, each with format-specific options (delimiter, sheet selection, encoding, NA strings, etc.). Added internal `plot2:::install_shiny_deps()` for administrators to install all Shiny dependencies at once.
* Support `y.transform = "reverse"`
* Support `legend.nrow`
* Support back-to-back (= tornado, = butterfly) plots

# plot2 1.0.0

* Project rename from [our previous iteration](https://github.com/certe-medical-epidemiology/certeplot2).

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

#' Interactively Create a `plot2`
#' 
#' This Shiny app allows for interactive creation of a `plot2`.
#' @param data A data set to load, offered as 'Provided' in the data set selector. The generated code refers to it by its name, e.g. `df |> plot2()` for `create_interactively(data = df)`. Not strictly required: in an interactive session, all data frames in the global environment are offered as well, together with the data sets of base R, plot2, ggplot2 and dplyr. Outside an interactive session (such as on a server), data frames in the global environment are not offered, so that the app does not expose what the hosting script has loaded.
#' @param css_code Additional CSS code to load in the app.
#' @param logo_path Path to the logo shown in the app. Default to the `plot2` logo. Use `NULL` to not use a logo.
#' @param pretty_labels A logical to switch to pretty, readable labels, instead of argument names in code style.
#' @param hide_generated_code A logical to hide generated code.
#' @param hide_export_buttons A logical to hide export buttons and functionality. `TRUE` will hide the elements completely, `NULL` will show a clickable text to expand buttons (default), `FALSE` will show the expanded buttons.
#' @param run A logical to run the app (default in an interactive session). Use `run = FALSE` to get the app object instead, for example to deploy it on a server, see *Deployment*.
#' @param upload_tab A logical to allow uploading data files. When `TRUE`, an **Upload** tab is added to the sidebar, with format-specific options for text (CSV, TSV, TXT), spreadsheet (Excel, OpenDocument), statistical software (SPSS, Stata, SAS, Minitab), Parquet, Feather, JSON, YAML, XML and HTML files; the data set selector then also offers "Upload data set...". R serialisation files (`.rds`, `.rda`) are not accepted, since reading them can run code: pass such data with the `data` argument instead. When `FALSE` (default), the app offers no way to upload files. Uploading requires the `readxl` package, and per format the package that reads it; administrators can install all of them with `plot2:::install_shiny_deps()`.
#' @details
#' ![Shiny app example](create_interactively.jpg)
#'
#' The app builds a [plot2()] call from the chosen settings, shows the plot and the code, and explains in a panel below the code what [plot2()] decided by itself.
#' @section Deployment:
#' The app can run on a server for several users (e.g. Shiny Server or Posit Connect), with an `app.R` such as:
#'
#' ```r
#' library(plot2)
#' create_interactively(run = FALSE)
#' ```
#'
#' On a server, the app treats everything a visitor types or sends as untrusted:
#' - Settings are converted to values or checked expressions; nothing typed is run as R code. Calculations in the column selectors may only use a fixed list of computing functions (such as `log()`, `mean()` and `n_distinct()`), and the call is evaluated apart from the global environment, so it cannot reach other objects of the R process.
#' - Data frames in the global environment are offered only in an interactive session; pass data with `data` to offer it on a server.
#' - Uploads are only possible with `upload_tab = TRUE`; R serialisation files are never accepted. The maximum upload size is Shiny's own (option `shiny.maxRequestSize`, 5 MB by default).
#' - Each session keeps its own plot, and raster exports are limited to 40 megapixels.
#'
#' Each plot is computed in the R process of the app, so a visitor can still ask for plots that take long to compute, e.g. with very large data or a very small `binwidth`. Use the limits of the hosting platform (e.g. timeouts and the number of processes) as for any Shiny app.
#' @return With `run = TRUE`, nothing (the app runs until it is closed). With `run = FALSE`, an app object of class `shiny.appobj` that can be run with [shiny::runApp()] or deployed.
#' @export
#' @examples
#' \dontrun{
#'
#' create_interactively()
#'
#' iris |> create_interactively()
#'
#' # With the upload tab for importing external data files:
#' create_interactively(upload_tab = TRUE)
#'
#' # The app object, e.g. for app.R on a server:
#' app <- create_interactively(run = FALSE)
#' }
create_interactively <- function(data = NULL,
                                 css_code = NULL,
                                 logo_path = system.file("logo.svg", package = "plot2"),
                                 pretty_labels = FALSE,
                                 hide_generated_code = FALSE,
                                 hide_export_buttons = NULL,
                                 upload_tab = FALSE,
                                 run = interactive()) {
  
  if (!is.null(logo_path) &&
      !(is.character(logo_path) && length(logo_path) == 1 && isTRUE(file.exists(logo_path)) &&
        grepl("[.](svg|jpe?g|png|gif)$", logo_path, ignore.case = TRUE))) {
    stop("Logo file does not exist or is not of type SVG, JPG, PNG, GIF: ", paste(logo_path, collapse = ", "),
         ". Use `logo_path = NULL` to remove the logo.", call. = FALSE)
  }
  
  rlang::check_installed("shiny")
  rlang::check_installed("pillar")
  rlang::check_installed("rio")
  if (isTRUE(upload_tab)) {
    rlang::check_installed("readxl")
  }
  
  # serve a copy of the logo from a folder of its own, never the folder of the logo (#125)
  if (!is.null(logo_path)) {
    logo_dir <- tempfile("plot2logo")
    dir.create(logo_dir)
    logo_file <- paste0("logo.", tolower(tools::file_ext(logo_path)))
    file.copy(logo_path, file.path(logo_dir, logo_file))
    shiny::addResourcePath("plot2res", logo_dir)
  }
  
  # Data sets (#126): a registry of objects, looked up by the value of the selector
  # with `[[`, never by evaluating text. `code` is how the generated code refers to it.
  data_registry <- list()
  register_data <- function(group, value, label, code, object) {
    if (is.null(object) || is.null(dim(object)) || value %in% names(data_registry)) {
      return(NULL)
    }
    data_registry[[value]] <<- list(group = group,
                                    label = paste0(label, " (", NROW(object), " x ", NCOL(object), ")"),
                                    code = code,
                                    object = object)
  }
  package_data <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      return(character(0))
    }
    as.character(utils::data(package = pkg)$results[, "Item"])
  }
  exported_data <- function(pkg, name) {
    tryCatch(getExportedValue(pkg, name), error = function(e) NULL)
  }

  # data passed to the app, referred to by its name when given as a name
  if (!is.null(data)) {
    data_expr <- substitute(data)
    register_data("Provided", "Provided", if (is.symbol(data_expr)) as.character(data_expr) else "Provided",
                  if (is.symbol(data_expr)) deparse1(data_expr, backtick = TRUE) else "your_df",
                  data)
  }
  # data frames in the global environment, only in an interactive session: on a
  # server they would expose whatever the hosting script has loaded
  if (rlang::is_interactive()) {
    for (nm in sort(ls(envir = globalenv()))) {
      obj <- get(nm, envir = globalenv(), inherits = FALSE)
      if (is.data.frame(obj)) {
        register_data("Global environment", nm, nm, deparse1(as.name(nm), backtick = TRUE), obj)
      }
    }
  }
  for (nm in c("airquality", "iris", "mtcars", "pressure", "Titanic")) {
    register_data("Base R", nm, nm, nm, exported_data("datasets", nm))
  }
  for (nm in package_data("plot2")) {
    register_data("plot2 package", nm, nm, nm, exported_data("plot2", nm))
  }
  for (nm in package_data("ggplot2")) {
    register_data("ggplot2 package", paste0("ggplot2::", nm), nm, paste0("ggplot2::", nm), exported_data("ggplot2", nm))
  }
  for (nm in setdiff(package_data("dplyr"), c("band_instruments", "band_instruments2", "band_members"))) {
    register_data("dplyr package", paste0("dplyr::", nm), nm, paste0("dplyr::", nm), exported_data("dplyr", nm))
  }

  # the choices of the selector: groups of value = label; without the upload tab
  # there is no way to upload (#123)
  data_groups <- unique(vapply(data_registry, function(x) x$group, character(1)))
  data_sets <- lapply(stats::setNames(data_groups, data_groups), function(g) {
    in_group <- Filter(function(x) x$group == g, data_registry)
    stats::setNames(names(in_group), vapply(in_group, function(x) x$label, character(1)))
  })
  if (isTRUE(upload_tab)) {
    data_sets <- append(data_sets, list("Upload data" = c("Upload data set..." = "import")),
                        after = sum(data_groups %in% c("Provided", "Global environment")))
  }
  default_dataset <- if ("Provided" %in% names(data_registry)) "Provided" else "iris"

  # Inputs (#136): one explicit specification of every argument of plot2(), see
  # shiny_input_spec(). The starting value of each input stands for plot2's default;
  # an argument appears in the code only when its input differs from it.
  spec <- shiny_input_spec()
  safe_env <- shiny_safe_env()
  plot2_formals <- formals(plot2)
  spec_inputs <- lapply(seq_len(nrow(spec)), function(i) {
    make_input(spec[i, , drop = FALSE], plot2_formals[[spec$arg[i]]], pretty_labels)
  })
  names(spec_inputs) <- spec$arg
  initial_values <- lapply(spec_inputs, function(x) x$initial)
  tab_inputs <- function(tab) {
    lapply(spec_inputs[spec$tab == tab & !spec$widget %in% c("main", "none")], function(x) x$tag)
  }

  # ---- Upload tab: supported formats ----------------------------------------
  upload_formats <- c(
    # Text
    "CSV (.csv)"                       = "csv",
    "TSV (.tsv)"                       = "tsv",
    "Plain text (.txt)"                = "txt",
    # Spreadsheet
    "Excel (.xlsx)"                    = "xlsx",
    "Excel legacy (.xls)"              = "xls",
    "OpenDocument Spreadsheet (.ods)"  = "ods",
    "Excel binary (.xlsb)"             = "xlsb",
    # Statistical software
    "SPSS (.sav)"                      = "sav",
    "SPSS compressed (.zsav)"          = "zsav",
    "Stata (.dta)"                     = "dta",
    "SAS (.sas7bdat)"                  = "sas7bdat",
    "SAS transport (.xpt)"             = "xpt",
    "Minitab (.mtp)"                   = "mtp",
    # Modern column-oriented
    "Parquet (.parquet)"               = "parquet",
    "Feather / Arrow (.feather)"       = "feather",
    # Text-based structured
    "JSON (.json)"                     = "json",
    "Newline-delimited JSON (.ndjson)" = "ndjson",
    "YAML (.yaml / .yml)"              = "yaml",
    "XML (.xml)"                       = "xml",
    "HTML table (.html)"               = "html"
  )
  # the encodings the readers accept (#132): fread() of data.table for text, haven for the others
  upload_encodings_text <- c("UTF-8" = "UTF-8", "Latin-1 (ISO-8859-1)" = "Latin-1", "Unknown" = "unknown")
  upload_encodings_stat <- c("As stored in the file" = "", "UTF-8" = "UTF-8", "Latin-1 (ISO-8859-1)" = "latin1", "Windows-1252" = "CP1252")
  upload_delims <- c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab" = "\t", "Pipe (|)" = "|", "Space" = " ", "Colon (:)" = ":")
  upload_quotes <- c('Double quote (")' = '"', "Single quote (')" = "'", "None" = "")
  upload_packages <- c(sav = "haven", zsav = "haven", dta = "haven", sas7bdat = "haven", xpt = "haven",
                       feather = "arrow", parquet = "nanoparquet", json = "jsonlite", ndjson = "jsonlite",
                       yaml = "yaml", xml = "xml2", html = "xml2", ods = "readODS",
                       xlsx = "readxl", xls = "readxl", xlsb = "readxl")
  upload_accept <- paste(
    c(".csv", ".tsv", ".txt",
      ".xls", ".xlsx", ".ods", ".xlsb",
      ".sav", ".zsav", ".dta", ".sas7bdat", ".xpt", ".mtp",
      ".parquet", ".feather", ".arrow",
      ".json", ".ndjson", ".yaml", ".yml", ".xml", ".html"),
    collapse = ","
  )

  ui <- shiny::fluidPage(
    title = "Generate plot2",
    theme = bslib::bs_theme(version = 5, preset = "united"),
    
    if (!is.null(logo_path)) { 
      shiny::div(
        id = "logo-container",
        shiny::img(src = file.path("plot2res", logo_file), height = "100px")
      )
    },
    
    shiny::tags$style(paste0(
      "html, body { height: 100%; margin: 0; padding: 0; overflow: hidden; }",
      ".container-fluid { height: 100%; display: flex; flex-direction: column; }",
      ".row { flex: 1; }",
      ".sidebarLayout { height: 100%; display: flex; }",
      ".sidebarPanel { height: 100%; overflow-y: auto; }",
      ".mainPanel, .well { height: 100%; overflow-y: auto; }",
      "#settings_tabs a { padding-left: 5px !important; padding-right: 5px !important; }",
      "#sidebar { overflow-y: auto; background-color: #f9f4f2; border-radius: 0; border: none; }",
      "#logo-container { position: absolute; bottom: 10px; right: 10px; z-index: -1; }",
      "#error_msg { color: red; }",
      ".container-fluid { overflow-x: hidden; padding-left: 0; }",
      "#settings_tabs { margin-bottom: 20px; font-size: 14px; }",
      "#settings_tabs a { padding-left: 10px; padding-right: 10px; }",
      "#y_calc, #y_calc * { margin-left: 5px; }",
      " #plot2_msgs * { font-size: 0.9rem; }",
      ".selectize-dropdown-content { max-height: 400px; }",
      ".short .selectize-dropdown-content { max-height: 275px; }",
      ".optgroup-header { color: var(--bs-primary) !important; }",
      "#colour .selectize-dropdown-content, #colour_fill .selectize-dropdown-content { max-height: 200px; }",
      ".copy-container { position: relative; }",
      "#copy_btn { position: absolute; top: 5px; right: 5px; background-color: var(--bs-primary); border-color: var(--bs-primary); }",
      ifelse(pretty_labels, "", ".settings * { margin-bottom: 2px; }"),
      ".bslib-input-switch input { width: 3.5em !important; height: 30px !important; }",
      ".bslib-input-switch, .bslib-input-switch input { cursor: pointer; }",
      "hr { border-top: 0.5px solid; margin-left: 25%; margin-right: 25%; }",
      "code, .code, pre, .pre { font-family: FiraCode, \"Fira Code\", SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace; }",
      ".link-copyright { position: absolute; left: 0px; bottom: 0px; margin-left: 5px; margin-bottom: 5px; font-size: 10px; }",
      ".export-right { border-left: 1px solid var(--bs-primary); }",
      "#export_pdf, #export_svg, #export_png, #export_jpg { width: 100%; margin-top: 32px; }",
      ifelse(!is.null(css_code), paste0(css_code, collapse = "\n"), ""),
      ifelse(hide_generated_code, ".generated-code { display: none; }", ""),
      ifelse(isTRUE(hide_export_buttons), "#export, .show-export { display: none; }", ""),
      ifelse(isFALSE(hide_export_buttons), "#show-export, .show-export { display: none; }", ""),
      "#upload_preview { font-size: 0.8rem; }",
      "#upload_preview table { font-size: 0.8rem; }",
      "#upload_preview .dataTables_wrapper { overflow-x: auto; max-height: 220px; overflow-y: auto; }",
      ".upload-section-title { font-weight: 600; font-size: 0.85rem; color: var(--bs-primary); margin: 6px 0 4px; }",
      ".upload-options { background: #f9f4f2; border-radius: 4px; padding: 8px; margin-bottom: 6px; }")),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 5,
        id = "sidebar",
        shinyjs::useShinyjs(),
        
        shiny::p(
          class = "link-copyright",
          shiny::HTML(paste0(
            "From ",
            shiny::a(
              href = "https://msberends.github.io/plot2/",
              target = "_blank",
              shiny::HTML(paste0("<code>plot2</code> v", utils::packageVersion("plot2")))
            ),
            ". Open source, ",
            shiny::a(
              href = "https://msberends.github.io/plot2/LICENSE",
              target = "_blank",
              "licenced"
            ),
            " under GNU GPL v2.0."
          ))
        ),
        
        shiny::div(
          id = "settings_tabs_container",
        shiny::tabsetPanel(
          type = "tabs",
          id = "settings_tabs",
          selected = ifelse(isTRUE(upload_tab), "Upload", "Main"),

          # --- Upload tab, only with upload_tab = TRUE (#123)
          if (isTRUE(upload_tab)) shiny::tabPanel("Upload",
            shiny::p(
              "Upload your data file to get started, or visit the",
              shiny::strong("Main"), "tab to explore built-in example data sets.",
              style = "color: var(--bs-secondary); font-size: 0.9rem; margin-bottom: 12px;"
            ),
            shiny::fileInput(
              "upload_file", "Choose file:",
              accept = upload_accept,
              width = "100%",
              multiple = FALSE
            ),
            shiny::selectInput(
              "upload_format", "File format:",
              choices = c("Auto-detect from extension" = "auto", upload_formats),
              width = "100%"
            ),

            # --- Text format options (CSV / TSV / TXT)
            shiny::conditionalPanel(
              condition = "['csv', 'tsv', 'txt'].includes(input.upload_format)",
              shiny::div(class = "upload-options",
                shiny::p(class = "upload-section-title", "Text file options"),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::selectInput("upload_delim", "Delimiter:",
                      choices = upload_delims,
                      width = "100%")
                  ),
                  shiny::column(6,
                    shiny::selectInput("upload_quote", "Quote character:",
                      choices = upload_quotes,
                      width = "100%")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::checkboxInput("upload_header", "First row is header", value = TRUE)
                  ),
                  shiny::column(6,
                    shiny::numericInput("upload_skip", "Skip rows:", value = 0, min = 0, width = "100%")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::selectInput("upload_encoding", "Encoding:", choices = upload_encodings_text, selected = "UTF-8", width = "100%")
                  ),
                  shiny::column(6,
                    shiny::textInput("upload_comment", "Comment character:", value = "", width = "100%")
                  )
                ),
                shiny::textInput("upload_na_strings", "NA strings (comma-separated):",
                  value = "NA", width = "100%")
              )
            ),

            # --- Excel / ODS options
            shiny::conditionalPanel(
              condition = "['xlsx', 'xls', 'ods', 'xlsb'].includes(input.upload_format)",
              shiny::div(class = "upload-options",
                shiny::p(class = "upload-section-title", "Spreadsheet options"),
                shiny::uiOutput("upload_sheet_ui"),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::checkboxInput("upload_xl_header", "First row is header", value = TRUE)
                  ),
                  shiny::column(6,
                    shiny::numericInput("upload_xl_skip", "Skip rows:", value = 0, min = 0, width = "100%")
                  )
                ),
                shiny::textInput("upload_xl_range", "Cell range (optional, e.g. A1:D20):",
                  value = "", width = "100%")
              )
            ),

            # --- Statistical software options (SPSS / Stata / SAS)
            shiny::conditionalPanel(
              condition = "['sav', 'zsav', 'dta', 'sas7bdat'].includes(input.upload_format)",
              shiny::div(class = "upload-options",
                shiny::p(class = "upload-section-title", "Statistical software options"),
                shiny::selectInput("upload_stat_encoding", "Encoding:", choices = upload_encodings_stat, width = "100%"),
                shiny::conditionalPanel(
                  condition = "['sav', 'zsav'].includes(input.upload_format)",
                  shiny::checkboxInput("upload_user_na",
                    "Preserve user-defined NA values (SPSS)", value = FALSE)
                )
              )
            ),

            # --- JSON options
            shiny::conditionalPanel(
              condition = "['json', 'ndjson'].includes(input.upload_format)",
              shiny::div(class = "upload-options",
                shiny::p(class = "upload-section-title", "JSON options"),
                shiny::checkboxInput("upload_json_simplify",
                  "Simplify to data frame", value = TRUE)
              )
            ),

            shiny::br(),
            shiny::actionButton("upload_import_btn", "Upload data",
              class = "btn-primary", width = "100%"
            ),
            shiny::br(), shiny::br(),
            shiny::uiOutput("upload_status"),
            DT::dataTableOutput("upload_preview")
          ),

          # --- Main tab
          shiny::tabPanel("Main",
                          shiny::fluidRow(
                            shiny::column(width = 6,
                                          shiny::selectizeInput(
                                            "dataset", "Data set",
                                            choices = data_sets,
                                            selected = default_dataset,
                                            options = list(create = FALSE),
                                            width = "100%"
                                          )
                            ),
                            shiny::column(width = 6,
                                          shiny::selectInput("type", "Plot type",
                                                             choices = shiny_type_choices(),
                                                             width = "100%")
                            ),
                          ),
                          shiny::hr(),
                          shiny::fluidRow(
                            shiny::column(width = 6,
                                          shiny::selectizeInput("x", "X-axis", choices = NULL, multiple = TRUE, width = "100%"),
                                          shiny::selectizeInput("category", "Category", choices = NULL, multiple = TRUE, width = "100%"),
                                          shiny::selectizeInput("facet", "Facet", choices = NULL, multiple = TRUE, width = "100%"),
                            ),
                            shiny::column(width = 6,
                                          shiny::selectizeInput("y", "Y-axis", choices = NULL, multiple = TRUE, width = "100%"),
                                          shiny::radioButtons(
                                            inputId = "y_calc",
                                            label = "Y-axis transformation:",
                                            choiceNames = lapply(names(shiny_y_calc_choices()), function(nm) {
                                              if (nm == "None") nm else shiny::HTML(ifelse(pretty_labels, nm, paste0("<code>", shiny_y_calc_choices()[[nm]], "</code>")))
                                            }),
                                            choiceValues = unname(shiny_y_calc_choices()),
                                            selected = "None"
                                          )
                            ),
                          ),
                          shiny::hr(),
                          tab_inputs("Main")
          ),

          # --- X settings
          shiny::tabPanel("X-axis", shiny::div(class = "settings", tab_inputs("X-axis"))),
          
          # --- Y settings
          shiny::tabPanel("Y-axis", shiny::div(class = "settings", tab_inputs("Y-axis"))),
          
          # --- Category
          shiny::tabPanel("Category", shiny::div(class = "settings", tab_inputs("Category"))),
          
          # --- Facet
          shiny::tabPanel("Facet", shiny::div(class = "settings", tab_inputs("Facet"))),
          
          # --- Datalabels
          shiny::tabPanel("Datalabels", shiny::div(class = "settings", tab_inputs("Datalabels"))),
          
          # --- Titles
          shiny::tabPanel("Titles", shiny::div(class = "settings", tab_inputs("Titles"))),
          
          # --- Legend
          shiny::tabPanel("Legend", shiny::div(class = "settings", tab_inputs("Legend"))),
          
          # --- Smooth
          shiny::tabPanel("Smooth", shiny::div(class = "settings", tab_inputs("Smooth"))),
          
          # --- Other
          shiny::tabPanel("Other", shiny::div(class = "settings", tab_inputs("Other")))
        ) # end tabsetPanel
        ) # end settings_tabs_container div
      ),

      shiny::mainPanel(
        width = 7,
        shiny::br(),
        shiny::tabsetPanel(
          shiny::tabPanel("Plot",
                          shiny::br(),
                          shiny::fluidRow(shiny::plotOutput("plot")),
                          shiny::tags$div(
                            class = "generated-code",
                            shiny::p("Generated code:"),
                            shiny::tags$div(
                              class = "copy-container",
                              # copied in the browser, not on the server (#129)
                              shiny::actionButton("copy_btn", "Copy", class = "copy-button", onclick = shiny_copy_js()),
                              shiny::verbatimTextOutput("code"),
                              shiny::uiOutput("plot2_msgs")
                            ),
                          ),
                          shiny::textOutput("error_msg"),
                          shiny::br(class = "show-export"),
                          shiny::actionLink("showexport", "Export plot >", class = "show-export"),
                          shiny::tags$div(
                            id = "export",
                            shiny::br(),
                            shiny::fluidRow(
                              shiny::column(width = 3, shiny::numericInput("export_width_cm",  "Width (cm)",  value = 15, min = 1, max = 100, step = 0.5, width = "100%")),
                              shiny::column(width = 3, shiny::numericInput("export_height_cm", "Height (cm)", value = 10, min = 1, max = 100, step = 0.5, width = "100%")),
                              shiny::column(width = 3, shiny::downloadButton("export_pdf", "PDF", class = "btn-primary", icon = NULL, width = "100%")),
                              shiny::column(width = 3, shiny::downloadButton("export_svg", "SVG", class = "btn-primary", icon = NULL)),
                            ),
                            shiny::fluidRow(
                              shiny::column(width = 3, shiny::numericInput("export_dpi",  "DPI", value = 300, min = 1, max = 1200, width = "100%")),
                              shiny::column(width = 3, shiny::uiOutput("export_px_preview")),
                              shiny::column(width = 3, shiny::downloadButton("export_png", "PNG", class = "btn-primary", icon = NULL)),
                              shiny::column(width = 3, shiny::downloadButton("export_jpg", "JPG", class = "btn-primary", icon = NULL)),
                            ),
                          ),
          ),
          shiny::tabPanel("Data",
                          shiny::br(),
                          DT::dataTableOutput("datatable")
          )
        )
      )
    ),
  )
  
  server <- function(input, output, session) {
    if (is.null(hide_export_buttons)) {
      shinyjs::hide("export")
    }

    imported_data <- shiny::reactiveVal(NULL)
    preview_data  <- shiny::reactiveVal(NULL)  # Upload tab staging area
    
    # Uploading exists only with upload_tab = TRUE (#123); all values sent by the
    # browser are checked against the choices of the inputs (#132), and reading
    # errors become notifications (#127)
    if (isTRUE(upload_tab)) {
      shiny::observeEvent(input$dataset, {
        if (identical(input$dataset, "import")) {
          # reset the selector so it does not stay on "import"
          shiny::updateSelectizeInput(session, "dataset",
                                      selected = if (!is.null(imported_data())) "imported" else default_dataset)
          shiny::updateTabsetPanel(session, "settings_tabs", selected = "Upload")
        }
      })

      # --- Upload tab: auto-detect format when a file is chosen ----------------
      shiny::observeEvent(input$upload_file, {
        ext <- upload_extension(input$upload_file$name)
        if (ext %in% upload_formats) {
          shiny::updateSelectInput(session, "upload_format", selected = ext)
        }
      })

      # --- Upload tab: dynamic sheet selector for Excel / ODS ------------------
      output$upload_sheet_ui <- shiny::renderUI({
        shiny::req(input$upload_file)
        ext <- upload_extension(input$upload_file$name)
        if (ext %in% c("xlsx", "xls", "ods", "xlsb")) {
          sheets <- tryCatch(
            if (ext == "ods") readODS::list_ods_sheets(input$upload_file$datapath) else readxl::excel_sheets(input$upload_file$datapath),
            error = function(e) NULL
          )
          if (!is.null(sheets) && length(sheets) > 0) {
            shiny::selectInput("upload_sheet", "Sheet:",
                               choices = stats::setNames(seq_along(sheets), sheets),
                               width = "100%")
          } else {
            shiny::numericInput("upload_sheet", "Sheet (number):",
                                value = 1, min = 1, width = "100%")
          }
        }
      })
    }

    # --- Upload tab: two-phase logic -----------------------------------------
    # Phase 1, do_preview(): reads the file with the current options and stores
    # the result in preview_data only. Nothing in the wider app changes yet.
    do_preview <- function() {
      file <- input$upload_file
      if (!isTRUE(upload_tab) || is.null(file) || is.null(file$datapath)) {
        return(invisible(NULL))
      }
      fail <- function(...) {
        shiny::showNotification(paste0(...), type = "error", duration = 10)
        preview_data(NULL)
        invisible(NULL)
      }
      fmt <- upload_choice(input$upload_format, c("auto", upload_formats))
      if (fmt == "auto") {
        fmt <- upload_extension(file$name)
      }
      if (!fmt %in% upload_formats) {
        return(fail("Files of this type cannot be uploaded: ", file$name))
      }
      pkg <- unname(upload_packages[fmt])
      if (!is.na(pkg) && !requireNamespace(pkg, quietly = TRUE)) {
        return(fail("Reading this file requires the R package '", pkg, "', which is not installed."))
      }

      # arguments for rio::import(); .txt is read as delimited text with the chosen delimiter
      import_args <- list(file = file$datapath, format = if (fmt == "txt") "csv" else fmt)
      if (fmt %in% c("csv", "tsv", "txt")) {
        import_args$sep        <- upload_choice(input$upload_delim, upload_delims)
        import_args$header     <- !isFALSE(input$upload_header)
        import_args$encoding   <- upload_choice(input$upload_encoding, upload_encodings_text)
        import_args$skip       <- upload_count(input$upload_skip)
        na_str                 <- trimws(strsplit(paste0(input$upload_na_strings, ""), ",")[[1]])
        import_args$na.strings <- na_str[nzchar(na_str)]
        comment_char           <- paste0(input$upload_comment, "")
        if (nzchar(comment_char)) import_args$comment.char <- substr(comment_char, 1, 1)
        import_args$quote      <- upload_choice(input$upload_quote, upload_quotes)
      } else if (fmt %in% c("xlsx", "xls", "ods", "xlsb")) {
        if (!is.null(input$upload_sheet)) import_args$which <- max(1L, upload_count(input$upload_sheet))
        import_args$col_names <- !isFALSE(input$upload_xl_header)
        import_args$skip      <- upload_count(input$upload_xl_skip)
        cell_range            <- paste0(input$upload_xl_range, "")
        if (nzchar(cell_range)) import_args$range <- cell_range
      } else if (fmt %in% c("sav", "zsav", "dta", "sas7bdat")) {
        encoding <- upload_choice(input$upload_stat_encoding, upload_encodings_stat)
        if (nzchar(encoding)) import_args$encoding <- encoding
        if (fmt %in% c("sav", "zsav")) import_args$user_na <- isTRUE(input$upload_user_na)
      } else if (fmt %in% c("json", "ndjson")) {
        import_args$simplifyVector <- !isFALSE(input$upload_json_simplify)
      }

      data <- tryCatch({
        out <- do.call(rio::import, import_args)
        if (!is.data.frame(out)) out <- as.data.frame(out)
        out
      }, error = function(e) {
        fail("Read failed: ", conditionMessage(e))
      })
      preview_data(data) # NULL on failure, which clears the preview table
    }

    if (isTRUE(upload_tab)) {
      # Debounced reactive over all upload inputs: fires do_preview() only,
      # so the wider app is untouched until the user presses the button.
      upload_trigger <- shiny::reactive({
        shiny::req(input$upload_file)
        list(
          input$upload_format,
          input$upload_delim,     input$upload_quote,
          input$upload_header,    input$upload_skip,
          input$upload_encoding,  input$upload_comment,  input$upload_na_strings,
          input$upload_sheet,
          input$upload_xl_header, input$upload_xl_skip,  input$upload_xl_range,
          input$upload_stat_encoding, input$upload_user_na,
          input$upload_json_simplify
        )
      })
      upload_trigger_d <- shiny::debounce(upload_trigger, 600)

      shiny::observeEvent(upload_trigger_d(), {
        do_preview()
      })

      # Phase 2, the "Upload data" button: commits preview_data to the app.
      shiny::observeEvent(input$upload_import_btn, {
        # re-read in case the options changed and the debounce has not fired yet
        do_preview()
        shiny::req(preview_data())

        data       <- preview_data()
        file_label <- input$upload_file$name

        imported_data(data)
        choices <- data_sets
        choices$`Upload data` <- c(
          stats::setNames("import", "Upload another data set..."),
          stats::setNames("imported",
                          paste0(file_label, " (",
                                 paste(dim(data), collapse = " \u00d7 "), ")"))
        )
        shiny::updateSelectizeInput(session, "dataset",
                                    selected = "imported", choices = choices)
        shiny::updateTabsetPanel(session, "settings_tabs", selected = "Main")
        shiny::showNotification(
          paste0("Uploaded \u201c", file_label, "\u201d: ",
                 nrow(data), " rows \u00d7 ", ncol(data), " columns."),
          type = "message", duration = 4
        )
      })
    }

    # --- Upload tab: status label and preview table --------------------------
    # These use preview_data, not imported_data, so nothing commits to the
    # wider app until the button is pressed.
    output$upload_status <- shiny::renderUI({
      d <- preview_data()
      if (!is.null(d)) {
        shiny::tagList(
          shiny::p(
            shiny::strong(paste0(nrow(d), " rows \u00d7 ", ncol(d), " columns")),
            " \u2014 adjust options above if needed, then press",
            shiny::strong("Upload data"), "to use this data set.",
            style = "font-size: 0.85rem; margin-bottom: 4px;"
          )
        )
      }
    })
    output$upload_preview <- DT::renderDataTable({
      shiny::req(preview_data())
      utils::head(preview_data(), 5)
    }, options = list(dom = "t", scrollX = TRUE, pageLength = 5),
       rownames = FALSE)
    
    # the selected data set, looked up by key (#126): the object as it is used in
    # the call, its columns for the selectors, and how the code refers to it
    current_data <- shiny::reactive({
      key <- input$dataset
      if (!is.character(key) || length(key) != 1) {
        return(NULL)
      }
      if (identical(key, "imported")) {
        object <- imported_data()
        code <- "your_df"
      } else if (key %in% names(data_registry)) {
        object <- data_registry[[key]]$object
        code <- data_registry[[key]]$code
      } else {
        return(NULL)
      }
      if (is.null(object)) {
        return(NULL)
      }
      columns <- if (is.data.frame(object)) object else tryCatch(fortify_df(object)$new_df, error = function(e) NULL)
      if (is.null(columns)) {
        return(NULL)
      }
      list(object = object, columns = columns, code = code)
    })

    # a new data set gives new choices in the column selectors
    shiny::observeEvent(current_data(), {
      tryCatch({
        d <- current_data()$columns
        if (inherits(d, "sf")) {
          shinyjs::disable("x")
          shinyjs::disable("y")
        } else {
          shinyjs::enable("x")
          shinyjs::enable("y")
        }
        vars <- names(d)
        types <- vapply(FUN.VALUE = character(1), vars, function(x) pillar::type_sum(d[[x]]))
        vars <- vars[types %unlike% "polygon"]
        types <- types[types %unlike% "polygon"]
        items <- mapply(function(v, t) {
          list(column = v, type = t, optgroup = "col_names")
        }, vars, types, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        opt <- list(
          create = TRUE,
          persist = FALSE,
          closeAfterSelect = TRUE,
          valueField = "column",
          labelField = "column",
          searchField = "column",
          optgroupField = "optgroup",
          optgroups = list(
            list(value = "fns", label = "Functions"),
            list(value = "col_names", label = "Data variables")
          ),
          delimiter = ";",
          options = items,
          render = I(shiny_selectize_render())
        )
        opt_y <- opt
        opt_y$options <- c(list(list(column = "n()", type = "func", optgroup = "fns")), items)
        for (id in c("x", "category", "facet")) {
          shiny::updateSelectizeInput(session, id, choices = NULL, options = opt, selected = "")
        }
        for (id in c("y", "y_secondary")) {
          shiny::updateSelectizeInput(session, id, choices = NULL, options = opt_y, selected = "")
        }
      }, error = function(e) {
        shiny::showNotification(paste0("The data set could not be loaded: ", conditionMessage(e)), type = "error")
      })
    })

    # The call (#18, #130): every input becomes an R value or a checked expression,
    # and the call is built as a language object. Nothing typed or sent by the browser
    # is parsed and run as code. An argument is included only when its input differs
    # from its starting value (#124, #135).
    plot_call <- shiny::reactive({
      cd <- current_data()
      shiny::req(cd)
      cols <- names(cd$columns)
      args <- list()
      problems <- character()
      type <- input$type
      if (is.character(type) && length(type) == 1 && type %in% setdiff(unlist(shiny_type_choices()), "auto")) {
        args$type <- type
      }
      for (nm in c("x", "y", "category", "facet")) {
        res <- shiny_direction(input[[nm]], cols)
        problems <- c(problems, if (length(res$problems) > 0) paste0(nm, ": ", res$problems))
        if (!is.null(res$expr)) {
          expr <- res$expr
          calc <- input$y_calc
          if (nm == "y" && is.character(calc) && length(calc) == 1 && calc %in% setdiff(shiny_y_calc_choices(), "None")) {
            expr <- call(sub("()", "", calc, fixed = TRUE), expr)
          }
          args[[nm]] <- expr
        }
      }
      settings <- spec[!spec$widget %in% c("main", "none"), , drop = FALSE]
      settings <- settings[order(settings$arg), , drop = FALSE]
      for (i in seq_len(nrow(settings))) {
        nm <- settings$arg[i]
        res <- shiny_arg_value(settings[i, , drop = FALSE], input[[nm]], initial_values[[nm]], cols)
        if (!is.null(res$problem)) {
          problems <- c(problems, paste0(nm, ": ", res$problem))
        }
        if (isTRUE(res$include)) {
          args[nm] <- list(res$value)
        }
      }
      list(args = args, problems = problems, data = cd)
    })

    call_string <- shiny::reactive({
      pc <- plot_call()
      shiny_code_text(pc$data$code, pc$args)
    })

    # The plot, with plot2's messages, warnings and errors (#133). Evaluated in an
    # environment that only reaches base R and a few functions for calculations.
    plot_result <- shiny::reactive({
      pc <- plot_call()
      msgs <- character()
      error <- NULL
      env <- new.env(parent = safe_env)
      env$.plot2_data <- pc$data$object
      cl <- as.call(c(list(plot2, quote(.plot2_data)), pc$args))
      p <- withCallingHandlers(
        tryCatch({
          out <- eval(cl, env)
          if (!inherits(out, "patchwork")) {
            # errors and messages that only come when the plot is built
            invisible(ggplot2::ggplot_build(out))
          }
          out
        }, error = function(e) {
          error <<- trimws(cli::ansi_strip(conditionMessage(e)))
          NULL
        }),
        message = function(m) {
          msgs <<- c(msgs, trimws(cli::ansi_strip(conditionMessage(m))))
          invokeRestart("muffleMessage")
        },
        warning = function(w) {
          msgs <<- c(msgs, paste("Warning:", trimws(cli::ansi_strip(conditionMessage(w)))))
          invokeRestart("muffleWarning")
        }
      )
      list(plot = p, messages = unique(msgs[nzchar(msgs)]), error = c(pc$problems, error))
    })

    output$code <- shiny::renderText({
      call_string()
    })

    output$plot <- shiny::renderPlot({
      p <- plot_result()$plot
      if (is.null(p)) {
        p <- ggplot2::ggplot() + ggplot2::theme_void()
      }
      suppressWarnings(suppressMessages(print(p)))
      invisible(NULL)
    }, res = 100)

    output$error_msg <- shiny::renderText({
      paste(plot_result()$error, collapse = "\n")
    })

    # as text, so that column names or values with markup are shown, not rendered
    output$plot2_msgs <- shiny::renderUI({
      msgs <- plot_result()$messages
      if (length(msgs) > 0) {
        shiny::tagList(lapply(msgs, shiny::div))
      }
    })

    shiny::observeEvent(input$copy_result, {
      if (identical(input$copy_result, "ok")) {
        shiny::showNotification("Code copied to clipboard.", duration = 2, closeButton = FALSE, type = "message")
      } else {
        shiny::showNotification("The browser did not allow copying. Select the code and copy it by hand.", duration = 5, type = "warning")
      }
    })

    # the plot of this session (#124)
    export_plot <- function() {
      p <- plot_result()$plot
      if (is.null(p)) {
        export_refuse("There is no plot to export.")
      }
      p
    }
    # the size of an export, checked on the server (#134): the limits of the inputs
    # in the browser can be bypassed, and a raster image must stay within a budget
    export_refuse <- function(msg) {
      shiny::showNotification(msg, type = "error", duration = 8)
      stop(msg, call. = FALSE)
    }
    export_size <- function(raster) {
      size <- shiny_export_size(input$export_width_cm, input$export_height_cm,
                                if (raster) input$export_dpi else NULL)
      if (!is.null(size$problem)) {
        export_refuse(size$problem)
      }
      size
    }
    output$export_pdf <- shiny::downloadHandler(
      filename = function() paste0("plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
      content = function(file) {
        ggplot2::ggsave(file,
                        plot   = export_plot(),
                        device = "pdf",
                        units  = "cm",
                        width  = export_size(FALSE)$width,
                        height = export_size(FALSE)$height)
      }
    )
    output$export_svg <- shiny::downloadHandler(
      filename = function() paste0("plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg"),
      content = function(file) {
        ggplot2::ggsave(file,
                        plot   = export_plot(),
                        device = "svg",
                        units  = "cm",
                        width  = export_size(FALSE)$width,
                        height = export_size(FALSE)$height)
      }
    )
    output$export_png <- shiny::downloadHandler(
      filename = function() paste0("plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
      content = function(file) {
        # Using units = "cm" keeps text/point sizes physically constant;
        # DPI only controls pixel density.
        ggplot2::ggsave(file,
                        plot   = export_plot(),
                        device = "png",
                        units  = "cm",
                        dpi    = export_size(TRUE)$dpi,
                        width  = export_size(TRUE)$width,
                        height = export_size(TRUE)$height)
      }
    )
    output$export_jpg <- shiny::downloadHandler(
      filename = function() paste0("plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg"),
      content = function(file) {
        ggplot2::ggsave(file,
                        plot   = export_plot(),
                        device = "jpeg",
                        units  = "cm",
                        dpi    = export_size(TRUE)$dpi,
                        width  = export_size(TRUE)$width,
                        height = export_size(TRUE)$height)
      }
    )

    shiny::observeEvent(input$showexport, {
      shinyjs::toggle("export")
    })

    # Show the output pixel dimensions below the DPI selector so the user
    # always knows exactly how large the raster file will be.
    output$export_px_preview <- shiny::renderUI({
      shiny::req(input$export_width_cm, input$export_height_cm, input$export_dpi)
      size <- shiny_export_size(input$export_width_cm, input$export_height_cm, input$export_dpi)
      shiny::p(
        if (!is.null(size$problem)) {
          size$problem
        } else {
          paste0("PDF and SVG are lossless formats. For JPG and PNG, the image size will be ",
                 format(size$px[1], big.mark = ","), " \u00d7 ", format(size$px[2], big.mark = ","), " px.")
        },
        style = "color: var(--bs-secondary); font-size: 0.82rem; margin-top: -6px; margin-bottom: 6px; width: 100%;"
      )
    })

    output$datatable <- DT::renderDataTable({
      current_data()$columns
    })
  }
  
  app <- shiny::shinyApp(ui, server)
  if (isTRUE(run)) {
    suppressMessages(shiny::runApp(app))
    invisible(NULL)
  } else {
    app
  }
}

# Inputs of the app (#136) ------------------------------------------------------

# One row per argument of plot2(): the tab it is shown in, its widget, its label, and
# for the widget its range ("min,max" or "min,max,step"), its set of choices (see
# shiny_choice_set()) or the placeholder of a text field. Widgets and the value they
# give:
# - switch: TRUE/FALSE; auto: NULL (automatic, left out), TRUE or FALSE
# - select, function: one of a fixed set of values; multi: several of a fixed set
# - text: a string; number and slider: a number within the range
# - values: R constants such as c(0, 10) or "a", parsed but never run
# - labels: values, or a function from shiny_label_functions()
# - formula: a formula with checked terms; colour: a colour name or set
# - column: column names or checked calculations; main: made in the Main tab
# - none: not in the app
# A test checks that every argument of plot2() has exactly one row.
shiny_input_spec <- function() {
  spec <- utils::read.table(sep = "|", header = TRUE, strip.white = TRUE, quote = "",
                            stringsAsFactors = FALSE, na.strings = character(0), comment.char = "", text = "
arg                     | tab        | widget   | label                      | opts
.data                   | Main       | none     |                            |
data                    | Main       | none     |                            |
print                   | Other      | none     |                            |
legend.title            | Legend     | none     |                            |
type                    | Main       | main     | Plot type                  |
x                       | Main       | main     | X-axis                     |
y                       | Main       | main     | Y-axis                     |
category                | Main       | main     | Category                   |
facet                   | Main       | main     | Facet                      |
colour                  | Main       | colour   | Colour                     |
colour_fill             | Main       | colour   | Fill colour                |
stacked                 | Main       | select   | Stacked                    | stacked
stacked_fill            | Main       | switch   | Stacked to 100%            |
summarise_function      | Main       | function | Summarise with             | summarise
x.title                 | X-axis     | text     | Title                      |
x.position              | X-axis     | select   | Position                   | position_x
x.sort                  | X-axis     | select   | Sort                       | sort_auto
x.max_items             | X-axis     | number   | Max items                  | 1,1000
x.max_txt               | X-axis     | text     | Text for the rest          |
x.character             | X-axis     | auto     | As text                    | auto
x.complete              | X-axis     | values   | Complete with              | 0
x.drop                  | X-axis     | switch   | Drop unused levels         |
x.remove                | X-axis     | switch   | Remove axis                |
x.zoom                  | X-axis     | auto     | Zoom                       | auto
x.limits                | X-axis     | values   | Limits                     | c(0, NA)
x.breaks                | X-axis     | values   | Breaks                     | c(0, 5, 10)
x.n_breaks              | X-axis     | number   | Number of breaks           | 1,50
x.labels                | X-axis     | labels   | Labels                     | c(\"a\", \"b\") or toupper
x.expand                | X-axis     | values   | Expand                     | c(0, 0)
x.transform             | X-axis     | select   | Transformation             | transform
x.label_angle           | X-axis     | slider   | Label angle                | 0,360,15
x.label_align           | X-axis     | number   | Label alignment            | 0,1
x.label_italic          | X-axis     | switch   | Labels in italics          |
x.date_breaks           | X-axis     | text     | Date breaks                | 1 month
x.date_labels           | X-axis     | text     | Date labels                | mmm yyyy
x.date_remove_years     | X-axis     | auto     | Remove years from dates    | auto
y.title                 | Y-axis     | text     | Title                      |
y.position              | Y-axis     | select   | Position                   | position_y
y.limits                | Y-axis     | values   | Limits                     | c(0, NA)
y.breaks                | Y-axis     | values   | Breaks                     | c(0, 5, 10)
y.n_breaks              | Y-axis     | number   | Number of breaks           | 1,50
y.labels                | Y-axis     | labels   | Labels                     | c(\"a\", \"b\") or toupper
y.expand                | Y-axis     | values   | Expand                     | c(0, 0)
y.transform             | Y-axis     | select   | Transformation             | transform
y.percent               | Y-axis     | auto     | Percentages                | auto
y.percent_break         | Y-axis     | number   | Percentage break           | 0,1
y.scientific            | Y-axis     | auto     | Scientific notation        | auto
y.24h                   | Y-axis     | switch   | 24-hour clock              |
y.age                   | Y-axis     | switch   | Age                        |
y.remove                | Y-axis     | switch   | Remove axis                |
y.zoom                  | Y-axis     | switch   | Zoom                       |
y_secondary             | Y-axis     | column   | Secondary axis             |
y_secondary.type        | Y-axis     | select   | Secondary axis: type       | types
y_secondary.title       | Y-axis     | text     | Secondary axis: title      |
y_secondary.colour      | Y-axis     | text     | Secondary axis: colour     |
y_secondary.colour_fill | Y-axis     | text     | Secondary axis: fill       |
y_secondary.scientific  | Y-axis     | auto     | Secondary axis: scientific | auto
y_secondary.percent     | Y-axis     | switch   | Secondary axis: percentages|
y_secondary.labels      | Y-axis     | labels   | Secondary axis: labels     | c(\"a\", \"b\") or toupper
category.title          | Category   | text     | Title                      |
category.type           | Category   | multi    | Type                       | category_type
category.sort           | Category   | select   | Sort                       | sort
category.max_items      | Category   | number   | Max items                  | 1,1000
category.max_txt        | Category   | text     | Text for the rest          |
category.focus          | Category   | values   | Focus on                   | a value of the category
category.labels         | Category   | labels   | Labels                     | c(\"a\", \"b\") or toupper
category.percent        | Category   | switch   | Percentages                |
category.breaks         | Category   | values   | Breaks                     | c(0, 5, 10)
category.limits         | Category   | values   | Limits                     | c(0, NA)
category.expand         | Category   | number   | Expand                     | 0,10
category.midpoint       | Category   | number   | Midpoint                   |
category.transform      | Category   | select   | Transformation             | transform
category.character      | Category   | auto     | As text                    | auto
category.complete       | Category   | values   | Complete with              | 0
category.date_breaks    | Category   | text     | Date breaks                | 1 month
category.date_labels    | Category   | text     | Date labels                | mmm yyyy
facet.position          | Facet      | select   | Position                   | position_facet
facet.sort              | Facet      | select   | Sort                       | sort
facet.max_items         | Facet      | number   | Max items                  | 1,100
facet.max_txt           | Facet      | text     | Text for the rest          |
facet.nrow              | Facet      | number   | Number of rows             | 1,50
facet.fixed_x           | Facet      | auto     | Fixed x-axis               | auto
facet.fixed_y           | Facet      | auto     | Fixed y-axis               | auto
facet.relative          | Facet      | switch   | Relative                   |
facet.drop              | Facet      | switch   | Drop unused levels         |
facet.complete          | Facet      | values   | Complete with              | 0
facet.repeat_labels_x   | Facet      | switch   | Repeat x labels            |
facet.repeat_labels_y   | Facet      | auto     | Repeat y labels            | auto
facet.bold              | Facet      | switch   | Bold                       |
facet.italic            | Facet      | switch   | Italic                     |
facet.size              | Facet      | slider   | Text size                  | 4,30,1
facet.margin            | Facet      | slider   | Margin                     | 0,30,1
facet.colour_fill       | Facet      | text     | Fill colour                |
datalabels              | Datalabels | auto     | Show datalabels            | auto
datalabels.format       | Datalabels | text     | Format                     |
datalabels.round        | Datalabels | number   | Decimals                   | 0,10
datalabels.percent_by   | Datalabels | select   | Percentages within         | percent_by
datalabels.colour       | Datalabels | text     | Colour                     |
datalabels.colour_fill  | Datalabels | text     | Fill colour                |
datalabels.size         | Datalabels | number   | Size                       | 0,30
datalabels.angle        | Datalabels | slider   | Angle                      | 0,360,15
datalabels.lineheight   | Datalabels | slider   | Line height                | 0,3,0.1
title                   | Titles     | text     | Title                      |
subtitle                | Titles     | text     | Subtitle                   |
caption                 | Titles     | text     | Caption                    |
tag                     | Titles     | text     | Tag                        |
title.linelength        | Titles     | number   | Title line length          | 10,500
title.colour            | Titles     | text     | Title colour               |
subtitle.linelength     | Titles     | number   | Subtitle line length       | 10,500
subtitle.colour         | Titles     | text     | Subtitle colour            |
markdown                | Titles     | switch   | Markdown in texts          |
legend                  | Legend     | auto     | Show legend                | auto
legend.position         | Legend     | select   | Position                   | position_legend
legend.reverse          | Legend     | switch   | Reverse                    |
legend.nrow             | Legend     | number   | Number of rows             | 1,50
legend.italic           | Legend     | switch   | Italic                     |
legend.barheight        | Legend     | slider   | Bar height                 | 1,30,0.5
legend.barwidth         | Legend     | slider   | Bar width                  | 0.5,10,0.5
legend.nbin             | Legend     | number   | Number of bins             | 2,1000
smooth                  | Smooth     | auto     | Add smooth                 | auto
smooth.method           | Smooth     | select   | Method                     | smooth_method
smooth.formula          | Smooth     | formula  | Formula                    | y ~ x
smooth.se               | Smooth     | switch   | Standard error             |
smooth.level            | Smooth     | slider   | Confidence level           | 0.5,0.99,0.01
smooth.alpha            | Smooth     | number   | Transparency               | 0,1
smooth.linewidth        | Smooth     | number   | Line width                 | 0,20
smooth.linetype         | Smooth     | select   | Line type                  | linetype_auto
smooth.colour           | Smooth     | text     | Colour                     |
horizontal              | Other      | switch   | Horizontal                 |
reverse                 | Other      | auto     | Reverse categories         | auto
zoom                    | Other      | switch   | Zoom                       |
size                    | Other      | number   | Size                       | 0,50
linewidth               | Other      | number   | Line width                 | 0,20
linetype                | Other      | select   | Line type                  | linetype
width                   | Other      | number   | Width                      | 0,10
binwidth                | Other      | number   | Bin width                  | 0,1e9
jitter_seed             | Other      | number   | Jitter seed                | 0,1e9
violin_scale            | Other      | select   | Violin scale               | violin
na.rm                   | Other      | switch   | Remove missing             |
na.replace              | Other      | text     | Replace missing            |
sep                     | Other      | text     | Separator character        |
decimal.mark            | Other      | text     | Decimal mark               |
big.mark                | Other      | text     | Big mark                   |
colour_transparency     | Other      | slider   | Colour transparency        | 0,1,0.05
sankey_node_width       | Other      | slider   | Sankey: node width         | 0,1,0.01
sankey_node_whitespace  | Other      | slider   | Sankey: node whitespace    | 0,0.5,0.01
sankey_alpha            | Other      | slider   | Sankey: transparency       | 0,1,0.05
sankey_remove_axes      | Other      | auto     | Sankey: remove axes        | auto
text_factor             | Other      | slider   | Text size factor           | 0.5,3,0.1
font                    | Other      | text     | Font                       |
theme                   | Other      | select   | Theme                      | theme
background              | Other      | text     | Background colour          |
")
  spec$opts <- gsub('\\"', '"', spec$opts, fixed = TRUE)
  spec
}

shiny_type_choices <- function() {
  list(
    "Automatic detection" = c("Automatic" = "auto"),
    "Common types" = c("Column" = "col", "Bar (horizontal)" = "bar", "Line" = "line",
                       "Point" = "point", "Area" = "area", "Histogram" = "histogram"),
    "Categorical types" = c("Boxplot" = "boxplot", "Violin" = "violin",
                            "Beeswarm" = "beeswarm", "Jitter" = "jitter"),
    "Advanced types" = c("Line-dot" = "linedot", "Dumbbell" = "dumbbell", "Back-to-back" = "back-to-back",
                         "Sankey" = "sankey", "Spider/radar" = "spider", "UpSet" = "upset",
                         "Bar percent" = "barpercent"),
    "Blank" = c("Blank (no geom)" = "blank")
  )
}

shiny_y_calc_choices <- function() {
  c("None" = "None", "Unique count" = "n_distinct()", "Minimum" = "min()",
    "Maximum" = "max()", "Mean" = "mean()", "Median" = "median()")
}

# Choices of the select widgets, as R code of their values (except 'category_type',
# whose values are plain text). The server accepts only these values.
shiny_choice_set <- function(name) {
  q <- function(x) paste0('"', x, '"')
  linetypes <- stats::setNames(as.character(1:6), paste0(1:6, " (", c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), ")"))
  types <- unlist(unname(shiny_type_choices()[-1]))
  switch(name,
         auto = c("Automatic" = "NULL", "Yes" = "TRUE", "No" = "FALSE"),
         stacked = c("No" = "FALSE", "Yes" = "TRUE", "To 100%" = q("fill")),
         summarise = c("Automatic (sum)" = "NULL", "Do not summarise" = "FALSE", "Sum" = "sum", "Mean" = "mean",
                       "Median" = "median", "Minimum" = "min", "Maximum" = "max", "Count" = "length",
                       "Unique count" = "n_distinct"),
         types = c("Automatic" = "NULL", stats::setNames(q(types), names(types))),
         sort_auto = c("Automatic" = "NULL", shiny_choice_set("sort")),
         sort = c("Factor levels, then A-Z" = "TRUE", "Ascending A-Z" = q("asc"), "Descending A-Z" = q("desc"),
                  "Ascending count" = q("freq-asc"), "Descending count" = q("freq-desc"),
                  "Order of data" = "FALSE"),
         position_x = c("Bottom" = q("bottom"), "Top" = q("top")),
         position_y = c("Left" = q("left"), "Right" = q("right")),
         position_facet = c("Top" = q("top"), "Bottom" = q("bottom"), "Left" = q("left"), "Right" = q("right")),
         position_legend = c("Automatic" = "NULL", "Top" = q("top"), "Right" = q("right"), "Bottom" = q("bottom"),
                             "Left" = q("left"), "Inside" = q("inside"), "None" = q("none")),
         transform = c("None" = q("identity"), "log10" = q("log10"), "log2" = q("log2"), "log" = q("log"),
                       "log1p" = q("log1p"), "sqrt" = q("sqrt"), "pseudo_log" = q("pseudo_log"),
                       "reverse" = q("reverse"), "reciprocal" = q("reciprocal"), "exp" = q("exp")),
         percent_by = c("x" = q("x"), "category" = q("category"), "facet" = q("facet"), "total" = q("total")),
         violin = c("Count" = q("count"), "Area" = q("area"), "Width" = q("width")),
         smooth_method = c("Automatic" = "NULL", "Linear (lm)" = q("lm"), "LOESS" = q("loess"),
                           "GLM" = q("glm"), "GAM" = q("gam")),
         linetype = linetypes,
         linetype_auto = c("Automatic" = "NULL", linetypes),
         category_type = c("colour/fill" = "colour", "shape" = "shape", "size" = "size",
                           "linetype" = "linetype", "linewidth" = "linewidth", "alpha" = "alpha"),
         theme = stats::setNames(q(c("theme_minimal2", "theme_minimal", "theme_bw", "theme_classic", "theme_light",
                                     "theme_linedraw", "theme_grey", "theme_dark", "theme_void")),
                                 c("theme_minimal2 (plot2)", "theme_minimal", "theme_bw", "theme_classic", "theme_light",
                                   "theme_linedraw", "theme_grey", "theme_dark", "theme_void")),
         stop("unknown choice set: ", name, call. = FALSE))
}

# functions that may be given as labels
shiny_label_functions <- function() {
  c("toupper", "tolower", "label_taxonomy")
}

# functions that may be used in calculations typed in the column selectors and in
# formulas; they compute on values and cannot reach files, the system or other objects
shiny_safe_functions <- function() {
  c("c", "(", "+", "-", "*", "/", "^", "%%", "%/%", "==", "!=", "<", ">", "<=", ">=", "&", "|", "!", "~",
    "abs", "sqrt", "exp", "log", "log2", "log10", "log1p", "round", "signif", "floor", "ceiling",
    "mean", "median", "min", "max", "sum", "length", "sd", "IQR", "is.na", "rev",
    "as.character", "as.numeric", "as.integer", "factor", "toupper", "tolower", "nchar",
    "n", "n_distinct", "I", "poly")
}

# the environment in which the app evaluates its call: base R (not the global
# environment or other packages) and the functions above
shiny_safe_env <- function() {
  env <- new.env(parent = baseenv())
  env$n <- dplyr::n
  env$n_distinct <- dplyr::n_distinct
  env$median <- stats::median
  env$sd <- stats::sd
  env$IQR <- stats::IQR
  env$poly <- stats::poly
  env$label_taxonomy <- label_taxonomy
  env
}

# whether an expression only uses names, constants and calls to shiny_safe_functions()
shiny_is_safe_expr <- function(e, fns = shiny_safe_functions()) {
  if (is.symbol(e) || (is.atomic(e) && length(e) == 1)) {
    return(TRUE)
  }
  if (is.call(e)) {
    return(is.symbol(e[[1]]) && as.character(e[[1]]) %in% fns &&
             all(vapply(as.list(e)[-1], shiny_is_safe_expr, logical(1), fns = fns)))
  }
  FALSE
}

# text as R constants: a number, string, logical, NA, Inf, or c() of those
shiny_literal <- function(text) {
  e <- tryCatch(str2lang(text), error = function(e) NULL)
  is_constant <- function(x) {
    (is.atomic(x) && length(x) == 1) || is.null(x) ||
      (is.symbol(x) && as.character(x) %in% c("Inf", "NaN")) ||
      (is.call(x) && length(x) == 2 && as.character(x[[1]]) %in% c("-", "+") && is.numeric(x[[2]]))
  }
  ok <- is_constant(e) ||
    (is.call(e) && identical(e[[1]], as.name("c")) && all(vapply(as.list(e)[-1], is_constant, logical(1))))
  if (!isTRUE(ok) || (is.null(e) && !identical(trimws(text), "NULL"))) {
    return(list(ok = FALSE))
  }
  list(ok = TRUE, value = eval(e, baseenv()))
}

# the expression of one item of a column selector: a column, n(), or a checked calculation
shiny_column_expr <- function(item, cols) {
  if (item %in% cols) {
    return(as.name(item))
  }
  if (nchar(item) > 500) {
    return(NULL)
  }
  e <- tryCatch(str2lang(item), error = function(e) NULL)
  if (!is.null(e) && shiny_is_safe_expr(e)) e else NULL
}

# the expression of a column selector: one item, or c() of several
shiny_direction <- function(values, cols) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]
  exprs <- lapply(values, shiny_column_expr, cols = cols)
  bad <- vapply(exprs, is.null, logical(1))
  problems <- if (any(bad)) paste0("'", values[bad], "' is not a column or a supported calculation") else character(0)
  exprs <- exprs[!bad]
  expr <- if (length(exprs) == 0) NULL else if (length(exprs) == 1) exprs[[1]] else as.call(c(as.name("c"), exprs))
  list(expr = expr, problems = problems)
}

# the value of a choice, from its R code
shiny_choice_value <- function(code) {
  e <- str2lang(code)
  if (is.call(e)) eval(e, baseenv()) else e
}

# the range of a number or slider widget
shiny_range <- function(opts) {
  r <- suppressWarnings(as.numeric(strsplit(opts, ",", fixed = TRUE)[[1]]))
  c(min = if (length(r) >= 1) r[1] else NA, max = if (length(r) >= 2) r[2] else NA, step = if (length(r) >= 3) r[3] else NA)
}

# The value of an input in the call. Returns list(include, value, problem): include is
# TRUE only when the input differs from its starting value and gives a valid value.
shiny_arg_value <- function(row, value, initial, cols) {
  omit <- list(include = FALSE)
  if (is.null(value) || length(value) == 0 || (length(value) == 1 && is.na(value))) {
    return(omit)
  }
  widget <- row$widget
  problem <- function(txt) list(include = FALSE, problem = txt)
  text <- if (is.character(value) || is.numeric(value) || is.logical(value)) paste0(value[1], "") else ""
  if (widget == "switch") {
    v <- isTRUE(as.logical(value[1]))
    if (identical(v, isTRUE(initial))) omit else list(include = TRUE, value = v)
  } else if (widget %in% c("select", "auto", "function")) {
    choices <- shiny_choice_set(row$opts)
    if (!text %in% choices) {
      problem("not one of the choices")
    } else if (identical(text, initial)) {
      omit
    } else {
      list(include = TRUE, value = shiny_choice_value(text))
    }
  } else if (widget == "multi") {
    v <- as.character(value)
    if (!all(v %in% shiny_choice_set(row$opts))) {
      problem("not one of the choices")
    } else if (setequal(v, initial)) {
      omit
    } else {
      list(include = TRUE, value = v)
    }
  } else if (widget %in% c("number", "slider")) {
    v <- suppressWarnings(as.numeric(value[1]))
    r <- shiny_range(row$opts)
    if (is.na(v)) {
      omit
    } else if (!is.finite(v) || (!is.na(r[["min"]]) && v < r[["min"]]) || (!is.na(r[["max"]]) && v > r[["max"]])) {
      problem(paste0("must be a number", if (!is.na(r[["min"]])) paste0(" from ", r[["min"]], " to ", r[["max"]])))
    } else if (is.numeric(initial) && !is.na(initial) && isTRUE(all.equal(v, initial))) {
      omit
    } else {
      list(include = TRUE, value = v)
    }
  } else if (widget %in% c("text", "colour", "values", "labels", "formula", "column")) {
    if (widget == "column") {
      res <- shiny_direction(value, cols)
      out <- if (is.null(res$expr)) omit else list(include = TRUE, value = res$expr)
      if (length(res$problems) > 0) out$problem <- paste(res$problems, collapse = "; ")
      return(out)
    }
    if (!nzchar(trimws(text)) || identical(text, initial)) {
      return(omit)
    }
    if (nchar(text) > 1000) {
      return(problem("too long"))
    }
    if (widget == "text") {
      return(list(include = TRUE, value = text))
    }
    if (widget == "formula") {
      e <- tryCatch(str2lang(text), error = function(e) NULL)
      if (is.call(e) && identical(e[[1]], as.name("~")) && shiny_is_safe_expr(e)) {
        return(list(include = TRUE, value = e))
      }
      return(problem("not a supported formula, such as y ~ x or y ~ poly(x, 2)"))
    }
    if (widget == "labels" && trimws(text) %in% shiny_label_functions()) {
      return(list(include = TRUE, value = as.name(trimws(text))))
    }
    lit <- shiny_literal(text)
    if (isTRUE(lit$ok)) {
      list(include = TRUE, value = lit$value)
    } else if (widget %in% c("values", "labels", "colour") && !grepl("[(){}]", text)) {
      # plain text, such as a value of the category or a colour name
      list(include = TRUE, value = text)
    } else {
      problem("not understood; use values such as c(0, 10) or \"text\"")
    }
  } else {
    omit
  }
}

# the evaluated default of an argument, or NULL when it depends on other arguments
shiny_default_value <- function(default) {
  if (is.call(default) || is.symbol(default)) {
    tryCatch(eval(default, envir = asNamespace("plot2")), error = function(e) NULL)
  } else {
    default
  }
}

# The input of one argument: its tag, and its starting value, which stands for
# plot2's default (in the app's own terms: a choice code, text, a number, ...)
make_input <- function(row, default, pretty_labels) {
  name <- row$arg
  widget <- row$widget
  dv <- shiny_default_value(default)
  out <- function(tag, initial, slider = FALSE, class = NULL) {
    list(tag = create_field(tag, name, row$label, pretty_labels, slider = slider, class = class),
         initial = initial)
  }
  if (widget %in% c("main", "none")) {
    return(list(tag = NULL, initial = NULL))
  }
  if (widget == "switch") {
    initial <- isTRUE(dv)
    return(out(bslib::input_switch(name, NULL, value = initial), initial))
  }
  if (widget %in% c("select", "auto", "function")) {
    choices <- shiny_choice_set(row$opts)
    code <- if (widget == "auto" || is.null(dv)) "NULL" else paste(deparse(dv), collapse = "")
    initial <- if (code %in% choices) code else unname(choices[1])
    return(out(shiny::selectInput(name, NULL, choices = choices, selected = initial, width = "100%"), initial))
  }
  if (widget == "multi") {
    initial <- as.character(dv)
    return(out(shiny::selectInput(name, NULL, choices = shiny_choice_set(row$opts), selected = initial,
                                  multiple = TRUE, width = "100%"), initial))
  }
  if (widget == "number") {
    initial <- if (is.numeric(dv) && length(dv) == 1 && is.finite(dv)) dv else NA_real_
    r <- shiny_range(row$opts)
    return(out(shiny::numericInput(name, NULL, value = initial, min = r[["min"]], max = r[["max"]], width = "100%"), initial))
  }
  if (widget == "slider") {
    r <- shiny_range(row$opts)
    initial <- if (is.numeric(dv) && length(dv) == 1 && is.finite(dv)) dv else r[["min"]]
    return(out(shiny::sliderInput(name, NULL, value = initial, min = r[["min"]], max = r[["max"]], step = r[["step"]], width = "100%"),
               initial, slider = TRUE))
  }
  if (widget == "colour") {
    initial <- if (is.character(dv) && length(dv) == 1) dv else ""
    registered <- names(Filter(function(x) length(x) > 1, plot2_env$reg_cols))
    if (length(registered) == 1) {
      registered <- list(registered)
    }
    tag <- shiny::selectizeInput(name, NULL, selected = initial,
                                 choices = list("plot2 (registered colour sets)" = registered,
                                                "viridis" = viridisLite_colours,
                                                "Base R" = grDevices::palette.pals()),
                                 width = "100%",
                                 options = c(list(create = TRUE, placeholder = "Automatic"),
                                             # without a starting value, select nothing instead of the first choice
                                             if (!nzchar(initial)) list(onInitialize = I('function() { this.setValue(""); }'))))
    return(out(tag, initial, class = "short"))
  }
  if (widget == "column") {
    return(out(shiny::selectizeInput(name, NULL, choices = NULL, multiple = TRUE, width = "100%"), NULL))
  }
  # text, values, labels, formula
  initial <- if (widget == "text" && is.character(dv) && length(dv) == 1) dv else ""
  out(shiny::textInput(name, NULL, value = initial, width = "100%",
                       placeholder = if (nzchar(row$opts)) row$opts else NULL),
      initial)
}

create_field <- function(inputTag, name, label, pretty_labels, slider = FALSE, class = NULL) {
  shiny::div(
    class = class,
    style = ifelse(slider, 
                   "display: flex; align-items: center; margin-bottom: -10px;",
                   "display: flex; align-items: center; margin-bottom: 4px;"),
    shiny::tags$label(if (isTRUE(pretty_labels)) label else shiny::code(name),
                      style = ifelse(slider,
                                     "flex: 0 0 50%; margin: 0; padding-right: 4px; margin-bottom: 10px;",
                                     "flex: 0 0 50%; margin: 0; padding-right: 4px;")),
    shiny::div(style = "flex: 1;", inputTag)
  )
}

# The generated code: the data set piped into plot2(), one argument per line
shiny_code_text <- function(data_code, args) {
  first <- if (identical(data_code, "your_df")) "your_df |>  # replace this with your data set" else paste0(data_code, " |>")
  txt <- vapply(seq_along(args), function(i) {
    paste0(names(args)[i], " = ", paste(deparse(args[[i]], width.cutoff = 500L), collapse = " "))
  }, character(1))
  if (length(txt) == 0) {
    paste0(first, "\n  plot2()")
  } else if (length(txt) == 1) {
    paste0(first, "\n  plot2(", txt, ")")
  } else {
    paste0(first, "\n  plot2(\n    ", paste(txt, collapse = ",\n    "), "\n  )")
  }
}

# The size of an export in cm, and for raster images the DPI and pixels. Returns
# list(width, height, dpi, px, problem); problem is set when a value is outside the
# limits of the inputs or a raster image would exceed the pixel budget (#134).
shiny_export_size <- function(width, height, dpi = NULL, max_pixels = 40e6) {
  number <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 1 && is.finite(x)) x else NA_real_
  }
  width <- number(width)
  height <- number(height)
  out <- list(width = width, height = height, dpi = NULL, px = NULL, problem = NULL)
  if (is.na(width) || is.na(height) || width < 1 || width > 100 || height < 1 || height > 100) {
    out$problem <- "Width and height must be between 1 and 100 cm."
    return(out)
  }
  if (!is.null(dpi)) {
    dpi <- number(dpi)
    if (is.na(dpi) || dpi < 1 || dpi > 1200) {
      out$problem <- "DPI must be between 1 and 1200."
      return(out)
    }
    out$dpi <- round(dpi)
    out$px <- floor(c(width, height) / 2.54 * out$dpi)
    if (prod(out$px) > max_pixels) {
      out$problem <- paste0("At ", format(out$px[1], big.mark = ","), " \u00d7 ", format(out$px[2], big.mark = ","),
                            " px, the image would exceed the maximum of ", max_pixels / 1e6,
                            " megapixels. Lower the size or the DPI.")
    }
  }
  out
}

# JavaScript of the copy button: copies the generated code in the browser and reports
# to the server whether that worked (#129)
shiny_copy_js <- function() {
  paste0(
    "(function() {",
    "  var text = document.getElementById('code').innerText;",
    "  var report = function(ok) { Shiny.setInputValue('copy_result', ok ? 'ok' : 'failed', {priority: 'event'}); };",
    "  var fallback = function() {",
    "    var area = document.createElement('textarea');",
    "    area.value = text; area.style.position = 'fixed'; area.style.opacity = '0';",
    "    document.body.appendChild(area); area.select();",
    "    var ok = false; try { ok = document.execCommand('copy'); } catch (e) {}",
    "    document.body.removeChild(area); return ok;",
    "  };",
    "  if (navigator.clipboard && window.isSecureContext) {",
    "    navigator.clipboard.writeText(text).then(function() { report(true); }, function() { report(fallback()); });",
    "  } else {",
    "    report(fallback());",
    "  }",
    "})();")
}

# how the column selectors show items: the type of each column as a coloured label
shiny_selectize_render <- function() {
  selectize_style <- "
    function(type) {
      var colors = {
        'dbl': 'var(--bs-blue)',
        'int': 'var(--bs-cyan)',
        'chr': 'var(--bs-red)',
        'ab': 'var(--bs-red)',
        'mo': 'var(--bs-red)',
        'fct': 'var(--bs-green)',
        'ord': 'var(--bs-teal)',
        'sir': 'var(--bs-teal)',
        'lgl': 'var(--bs-yellow)',
        'date': 'var(--bs-indigo)',
        'datetime': 'var(--bs-purple)',
        'hms': 'var(--bs-purple)',
        'list': 'var(--bs-dark)',
        'unknown': 'var(--bs-default)',
        'func': 'transparent'
      };
      var col = colors[type] || colors['unknown'];
      return '<span style=\"' +
        'background:' + col + ';' +
        'color:white;' +
        'padding:2px 7px;' +
        'margin-bottom:3px;' +
        'border-radius:12px;' +
        'font-size:11px;' +
        'display:inline-block;' +
        'width:40px;' +
        'text-align:center;' +
        '\">' +
        escape(type) +
        '</span>';
    }"
  sprintf("
    {
      option: function(data, escape) {
        var type_label = %s;
        return '<div>&nbsp;&nbsp;&nbsp;' +
               (data.type !== 'func' ? type_label(data.type) + '&nbsp;&nbsp;' : '') +
               escape(data.column) +
               '</div>';
      }
    }", selectize_style)
}

# file extension of an upload, with aliases normalised
upload_extension <- function(name) {
  ext <- tolower(tools::file_ext(paste0(name, "")))
  switch(ext, yml = "yaml", arrow = "feather", ext)
}

# a value sent by the browser, if it is one of the choices of its input
upload_choice <- function(value, choices, default = unname(choices[1])) {
  if (length(value) == 1 && !is.na(value) && value %in% choices) value else default
}

# a non-negative whole number sent by the browser, or 0
upload_count <- function(value) {
  value <- suppressWarnings(as.integer(value))
  if (length(value) == 1 && !is.na(value) && value >= 0) value else 0L
}

#' Install All Shiny App Dependencies
#'
#' A convenience function for administrators to check and install all packages
#' required by [create_interactively()], including those needed for the upload
#' tab (`upload_tab = TRUE`). This function is not intended for end users; run
#' it once in an admin session to prepare the environment:
#'
#' ```r
#' plot2:::install_shiny_deps()
#' ```
#' @keywords internal
install_shiny_deps <- function() {
  rlang::check_installed(c(
    # Core Shiny app
    "bslib",
    "DT",
    "pillar",
    "rio",
    "shiny",
    "shinyjs",
    # Upload tab: spreadsheet backends
    "readxl",   # XLS, XLSX (also used for sheet detection)
    "readODS",  # ODS
    # Upload tab: statistical software backends
    "haven",    # SPSS (.sav, .zsav), Stata (.dta), SAS (.sas7bdat, .xpt)
    # Upload tab: columnar / big-data backends
    "arrow",       # Feather / Arrow
    "nanoparquet", # Parquet
    # Upload tab: text-structured backends
    "jsonlite", # JSON, NDJSON
    "yaml",     # YAML
    "xml2"      # XML, HTML tables
  ))
}

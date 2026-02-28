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
#' @param data A data set to load. Not strictly required, since all data sets in the global environment will be shown.
#' @param css_code Additional CSS code to load in the app.
#' @param logo_path Path to the logo shown in the app. Default to the `plot2` logo. Use `NULL` to not use a logo.
#' @param pretty_labels A logical to switch to pretty, readable labels, instead of argument names in code style.
#' @param hide_generated_code A logical to hide generated code.
#' @param hide_export_buttons A logical to hide export buttons and functionality. `TRUE` will hide the elements completely, `NULL` will show a clickable text to expand buttons (default), `FALSE` will show the expanded buttons.
#' @param upload_tab A logical to show a dedicated **Upload** tab in the sidebar. When `TRUE`, a full-featured data-import tab is added with format-specific options for many file types (CSV, TSV, Excel, SPSS, Stata, SAS, RDS, JSON, Parquet, Feather, YAML, XML, and more). The "Import data set..." dropdown item will redirect to this tab instead of opening a modal. Requires the `readxl` package in addition to the packages checked for `upload_tab = FALSE`. Administrators can ensure all dependencies are present by running `plot2:::install_shiny_deps()` before launching the app.
#' @details
#' ![Shiny app example](create_interactively.jpg)
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
#' }
create_interactively <- function(data = NULL,
                                 css_code = NULL,
                                 logo_path = system.file("logo.svg", package = "plot2"),
                                 pretty_labels = FALSE,
                                 hide_generated_code = FALSE,
                                 hide_export_buttons = NULL,
                                 upload_tab = FALSE) {
  
  logo_path <- tryCatch({
    # might return an error:
    is.null(logo_path) || (file.exists(logo_path) && grepl("[.](svg|jpg|png|gif)$", logo_path, ignore.case = TRUE))
    logo_path
  }, error = function(e) NULL)
  if (!is.null(logo_path) && !file.exists(logo_path)) {
    stop("Logo file does not exist or is not of type SVG, JPG, PNG, GIF: ", logo_path, ". Use `logo_path = NULL` to remove the logo.")
  }
  
  rlang::check_installed("shiny")
  rlang::check_installed("clipr")
  rlang::check_installed("pillar")
  rlang::check_installed("rio")
  if (isTRUE(upload_tab)) {
    rlang::check_installed("readxl")
  }
  
  rstudio_viewer <- identical(Sys.getenv("RSTUDIO"), "1") && interactive()
  rstudio_viewer <- FALSE # turn off for now
  max_height <- 900
  if (rstudio_viewer && .Platform$OS.type == "windows") {
    res <- system("wmic path Win32_VideoController get CurrentHorizontalResolution,CurrentVerticalResolution", intern = TRUE)
    res <- trimws(unlist(strsplit(res, " ")))
    res <- as.numeric(res[res %like% "^[0-9]+$"])
    max_height <- min(res[length(res)] - 400, max_height)
  }
  
  # in server or before shiny::runGadget()
  if (!is.null(logo_path)) {
    shiny::addResourcePath("plot2res", dirname(logo_path))
  }
  
  # Collect datasets
  plot2_datasets <- as.character(data(package = "plot2")$results[, "Item"])
  
  globalenv_datasets <- ls(envir = globalenv())[vapply(
    ls(envir = globalenv()),
    FUN.VALUE = logical(1),
    function(x) is.data.frame(get(x, envir = globalenv()))
  )]
  
  ggplot2_datasets <- paste0("ggplot2::", data(package = "ggplot2")$results[, "Item"])
  
  dplyr_datasets <- paste0("dplyr::", data(package = "dplyr")$results[, "Item"])
  dplyr_datasets <- dplyr_datasets[dplyr_datasets %unlike% "band_"]
  
  # If a dataset is provided as argument, add it
  if (!is.null(data)) {
    data_name <- deparse(substitute(data))
    if (isTRUE(tryCatch(get(data_name), error = function(e) TRUE))) {
      data_name <- "Provided"
      assign(x = data_name, value = data)
    }
    globalenv_datasets <- c(data_name, globalenv_datasets)
  }
  
  # Ensure some defaults are always there
  base_datasets <- c("iris", "mtcars", "Titanic")
  
  globalenv_labels <- stats::setNames(globalenv_datasets,
                                      vapply(FUN.VALUE = character(1),
                                             globalenv_datasets,
                                             label_with_dims))
  base_labels <- stats::setNames(base_datasets,
                                 vapply(FUN.VALUE = character(1),
                                        base_datasets,
                                        label_with_dims))
  plot2_labels <- stats::setNames(plot2_datasets,
                                  vapply(FUN.VALUE = character(1),
                                         plot2_datasets,
                                         label_with_dims))
  ggplot2_labels <- stats::setNames(ggplot2_datasets,
                                    vapply(FUN.VALUE = character(1),
                                           ggplot2_datasets,
                                           label_with_dims))
  dplyr_labels <- stats::setNames(dplyr_datasets,
                                  vapply(FUN.VALUE = character(1),
                                         dplyr_datasets,
                                         label_with_dims))
  
  # Grouped list
  data_sets <- list(
    "Global environment" = globalenv_labels,
    "Import data"        = c("Import data set..." = "import"),
    "Base R"             = base_labels,
    "plot2 package"      = plot2_labels,
    "ggplot2 package"    = ggplot2_labels,
    "dplyr package"      = dplyr_labels
  )
  
  plot2_formals <- formals(plot2::plot2)
  plot2_formals <- plot2_formals[order(names(plot2_formals))]
  
  plot2_env$shiny_defaults <- list()
  
  # grab formals from plot2
  main_args <- plot2_formals[names(plot2_formals) %in% c("colour", "colour_fill", "stacked", "stacked_fill")]
  main_inputs <- lapply(names(main_args), function(nm) {
    make_input(nm, main_args[[nm]], pretty_labels)
  })
  x_args <- plot2_formals[grep("^x\\.", names(plot2_formals))]
  x_inputs <- lapply(names(x_args), function(nm) {
    make_input(nm, x_args[[nm]], pretty_labels)
  })
  y_args <- plot2_formals[grep("^y\\.", names(plot2_formals))]
  y_inputs <- lapply(names(y_args), function(nm) {
    make_input(nm, y_args[[nm]], pretty_labels)
  })
  category_args <- plot2_formals[grep("^(category\\.|category.type)", names(plot2_formals))]
  category_inputs <- lapply(names(category_args), function(nm) {
    make_input(nm, category_args[[nm]], pretty_labels)
  })
  facet_args <- plot2_formals[grep("^facet\\.", names(plot2_formals))]
  facet_inputs <- lapply(names(facet_args), function(nm) {
    make_input(nm, facet_args[[nm]], pretty_labels)
  })
  datalabels_args <- plot2_formals[grep("^datalabels", names(plot2_formals))]
  datalabels_inputs <- lapply(names(datalabels_args), function(nm) {
    make_input(nm, datalabels_args[[nm]], pretty_labels)
  })
  legend_args <- plot2_formals[grep("^legend\\.", names(plot2_formals))]
  legend_args <- legend_args[names(legend_args) != "legend.title"]
  legend_inputs <- lapply(names(legend_args), function(nm) {
    make_input(nm, legend_args[[nm]], pretty_labels)
  })
  title_args <- plot2_formals[grep("(title|subtitle|tag|caption)", names(plot2_formals))]
  title_args <- title_args[!names(title_args) %in% c(names(x_args), names(y_args), names(category_args), names(facet_args))]
  title_args <- title_args[!names(title_args) %like% "legend|y_secondary"]
  title_inputs <- lapply(names(title_args), function(nm) {
    make_input(nm, title_args[[nm]], pretty_labels)
  })
  smooth_args <- plot2_formals[grep("^smooth", names(plot2_formals))]
  smooth_inputs <- lapply(names(smooth_args), function(nm) {
    make_input(nm, smooth_args[[nm]], pretty_labels)
  })
  other_args <- plot2_formals[!names(plot2_formals) %in% c(".data", "x", "y", "category", "facet", "type", "data", "...", "print", names(main_args), names(x_args), names(y_args), names(category_args), names(facet_args), names(datalabels_args), names(legend_args), names(title_args), names(smooth_args))]
  other_args <- other_args[!names(other_args) %like% "y_secondary|legend[.]title"]
  other_inputs <- lapply(names(other_args), function(nm) {
    make_input(nm, other_args[[nm]], pretty_labels)
  })
  
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
    # R formats
    "R serialised object (.rds)"       = "rds",
    "R data file (.rda / .rdata)"      = "rda",
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
  upload_accept <- paste(
    c(".csv", ".tsv", ".txt",
      ".xls", ".xlsx", ".ods", ".xlsb",
      ".sav", ".zsav", ".dta", ".sas7bdat", ".xpt", ".mtp",
      ".rds", ".rda", ".rdata",
      ".parquet", ".feather", ".arrow",
      ".json", ".ndjson", ".yaml", ".yml", ".xml", ".html"),
    collapse = ","
  )

  ui <- shiny::fluidPage(
    title = "Generate plot2",
    theme = bslib::bs_theme(version = 5, preset = "united"),
    
    shiny::tags$style(paste0(
      "html, body { height: 100%; margin: 0; padding: 0; overflow: hidden; }",
      ".container-fluid { height: 100%; display: flex; flex-direction: column; }",
      ".row { flex: 1; }",
      ".sidebarLayout { height: 100%; display: flex; }",
      ".sidebarPanel { height: 100%; overflow-y: auto; }",
      ".mainPanel, .well { height: 100%; overflow-y: auto; }",
      "#sidebar { overflow-y: auto; ", ifelse(rstudio_viewer, paste0("height: ", max_height, "px; "), ""), "background-color: #f9f4f2; border-radius: 0; border: none; }",
      "#logo-container { position: absolute; bottom: 10px; right: 10px; }",
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
      "#export_pdf, #export_svg, #export_png, #export_jpg { width: 49% ; }",
      # "#export button { margin-left: 10px; }",
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
              href = "https://msberends.github.io/plot2",
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
        
        shiny::tabsetPanel(
          type = "tabs",
          id = "settings_tabs",
          
          # --- Main tab
          shiny::tabPanel("Main",
                          shiny::fluidRow(
                            shiny::column(width = 6,
                                          shiny::selectizeInput(
                                            "dataset", "Data set",
                                            choices = data_sets,
                                            selected = ifelse(is.null(data), "iris", names(globalenv_labels)[1]),
                                            options = list(create = FALSE),
                                            width = "100%"
                                          )
                            ),
                            shiny::column(width = 6,
                                          shiny::selectInput("type", "Plot type", 
                                                             choices = list(
                                                               "Automatic detection" = c("Automatic" = "auto"),
                                                               "Common types" = c(
                                                                 "Column"    = "col",
                                                                 "Bar (horizontal)" = "bar",
                                                                 "Line"      = "line",
                                                                 "Point"     = "point",
                                                                 "Area"      = "area",
                                                                 "Histogram" = "histogram"
                                                               ),
                                                               "Categorical types" = c(
                                                                 "Boxplot"   = "boxplot",
                                                                 "Violin"    = "violin",
                                                                 "Beeswarm"  = "beeswarm",
                                                                 "Jitter"    = "jitter"
                                                               ),
                                                               "Advanced types" = c(
                                                                 "Line-dot"     = "linedot",
                                                                 "Dumbbell"     = "dumbbell",
                                                                 "Back-to-back" = "back-to-back",
                                                                 "Sankey"       = "sankey",
                                                                 "Spider/radar" = "spider",
                                                                 "UpSet"        = "upset",
                                                                 "Bar percent"  = "barpercent"
                                                               ),
                                                               "Blank" = c("Blank (no geom)" = "blank")
                                                             ),
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
                                            choiceNames = list(
                                              "None",
                                              shiny::HTML(ifelse(pretty_labels, "Unique count", "<code>n_distinct()</code>")),
                                              shiny::HTML(ifelse(pretty_labels, "Minimum", "<code>min()</code>")),
                                              shiny::HTML(ifelse(pretty_labels, "Maximum", "<code>max()</code>")),
                                              shiny::HTML(ifelse(pretty_labels, "Mean", "<code>mean()</code>")),
                                              shiny::HTML(ifelse(pretty_labels, "Median", "<code>median()</code>"))
                                            ),
                                            choiceValues = c(
                                              "None",
                                              "n_distinct()",
                                              "min()",
                                              "max()",
                                              "mean()",
                                              "median()"
                                            ),
                                            selected = "None"
                                          )
                            ),
                          ),
                          shiny::hr(),
                          main_inputs
          ),
          
          # --- Upload tab (optional, only when upload_tab = TRUE)
          if (isTRUE(upload_tab)) shiny::tabPanel("Upload",
            shiny::br(),
            shiny::fileInput(
              "upload_file", "Choose file:",
              accept = upload_accept,
              width = "100%",
              multiple = FALSE
            ),
            shiny::selectInput(
              "upload_format", "File format:",
              choices = c("Auto-detect from extension" = "", upload_formats),
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
                      choices = c("Comma (,)" = ",", "Semicolon (;)" = ";",
                                  "Tab" = "\t", "Pipe (|)" = "|",
                                  "Space" = " ", "Colon (:)" = ":"),
                      width = "100%")
                  ),
                  shiny::column(6,
                    shiny::selectInput("upload_quote", "Quote character:",
                      choices = c('Double quote (")' = '"', "Single quote (')" = "'",
                                  "None" = ""),
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
                    shiny::textInput("upload_encoding", "Encoding:", value = "UTF-8", width = "100%")
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
              condition = "['sav', 'zsav', 'dta', 'sas7bdat', 'xpt', 'mtp'].includes(input.upload_format)",
              shiny::div(class = "upload-options",
                shiny::p(class = "upload-section-title", "Statistical software options"),
                shiny::textInput("upload_stat_encoding", "Encoding:", value = "UTF-8", width = "100%"),
                shiny::conditionalPanel(
                  condition = "['sav', 'zsav'].includes(input.upload_format)",
                  shiny::checkboxInput("upload_user_na",
                    "Preserve user-defined NA values (SPSS)", value = FALSE)
                ),
                shiny::conditionalPanel(
                  condition = "input.upload_format === 'dta'",
                  shiny::checkboxInput("upload_dta_labels",
                    "Convert value labels to factors (Stata)", value = TRUE)
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
            shiny::actionButton("upload_import_btn", "Import data",
              class = "btn-primary", width = "100%"
            ),
            shiny::br(), shiny::br(),
            shiny::uiOutput("upload_status"),
            DT::dataTableOutput("upload_preview")
          ),

          # --- X settings
          shiny::tabPanel("X-axis", shiny::div(class = "settings", x_inputs)),
          
          # --- Y settings
          shiny::tabPanel("Y-axis", shiny::div(class = "settings", y_inputs)),
          
          # --- Category
          shiny::tabPanel("Category", shiny::div(class = "settings", category_inputs)),
          
          # --- Facet
          shiny::tabPanel("Facet", shiny::div(class = "settings", facet_inputs)),
          
          # --- Datalabels
          shiny::tabPanel("Datalabels", shiny::div(class = "settings", datalabels_inputs)),
          
          # --- Titles
          shiny::tabPanel("Titles", shiny::div(class = "settings", title_inputs)),
          
          # --- Datalabels
          shiny::tabPanel("Legend", shiny::div(class = "settings", legend_inputs)),
          
          # --- Smooth
          shiny::tabPanel("Smooth", shiny::div(class = "settings", smooth_inputs)),
          
          # --- Other
          shiny::tabPanel("Other", shiny::div(class = "settings", other_inputs))
        )
      ),
      
      shiny::mainPanel(
        width = 7,
        shiny::br(),
        shiny::fluidRow(shiny::plotOutput("plot")),
        shiny::tags$div(
          class = "generated-code",
          shiny::hr(),
          shiny::p("Generated code:"),
          shiny::tags$div(
            class = "copy-container",
            shiny::actionButton("copy_btn", "Copy", class = "copy-button"),
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
          # shiny::p("Export as:"),
          shiny::fluidRow(
            # fields
            shiny::column(width = 5,
                          class = "export-left",
                          shiny::p(shiny::strong("Vector graphic (scalable)")),
                          shiny::fluidRow(
                            shiny::column(width = 6, shiny::numericInput("export_height_cm", "Height (cm)", value = 10, min = 1, width = "100%")),
                            shiny::column(width = 6, shiny::numericInput("export_width_cm", "Width (cm)", value = 15, min = 1, width = "100%")),
                          ),
            ),
            shiny::column(width = 7,
                          class = "export-right",
                          shiny::p(shiny::strong("Raster graphic")),
                          shiny::fluidRow(
                            shiny::column(width = 4, shiny::numericInput("export_height_px", "Height (px)", value = 500, min = 1, width = "100%")),
                            shiny::column(width = 4, shiny::numericInput("export_width_px", "Width (px)", value = 750, min = 1, width = "100%")),
                            shiny::column(width = 4, shiny::numericInput("export_dpi", "DPI", value = 100, min = 1, width = "100%")),
                          ),
            ),
          ),
          shiny::fluidRow(
            # buttons
            shiny::column(width = 5,
                          class = "export-left",
                          shiny::downloadButton("export_pdf", "Export as PDF", class = "btn-primary", icon = NULL, width = "49.5%"),
                          shiny::downloadButton("export_svg", "Export as SVG", class = "btn-primary", icon = NULL, width = "49.5%"),
            ),
            shiny::column(width = 7,
                          class = "export-right",
                          shiny::downloadButton("export_png", "Export as PNG", class = "btn-primary", icon = NULL, width = "49.5%"),
                          shiny::downloadButton("export_jpg", "Export as JPG", class = "btn-primary", icon = NULL, width = "49.5%"),
            ),
          ),
        ),
        if (!is.null(logo_path)) { 
          shiny::div(
            id = "logo-container",
            shiny::img(src = file.path("plot2res", basename(logo_path)), height = "100px")
          )
        },
        shiny::br(),
        shiny::actionLink("showdata", "Show data >"),
        shiny::br(),
        shiny::br(),
        DT::dataTableOutput("datatable")
      )
    )
  )
  
  server <- function(input, output, session) {
    shinyjs::hide("datatable")
    if (is.null(hide_export_buttons)) {
      shinyjs::hide("export")
    }
    
    imported_data <- shiny::reactiveVal(NULL)
    
    changed_inputs <- shiny::reactiveValues(names = character())
    shiny::observe({
      # keep track of changed elements
      plot2_args <- setdiff(names(formals(plot2)), c("...", ".data", "data"))
      arg_names  <- intersect(names(input), plot2_args)
      lapply(arg_names, function(id) {
        shiny::observeEvent(input[[id]], {
          if (!(id %in% changed_inputs$names)) {
            changed_inputs$names <- c(changed_inputs$names, id)
          }
        }, ignoreInit = TRUE)
      })
    })
    
    shiny::observe({
      if (input$dataset == "import") {
        if (isTRUE(upload_tab)) {
          # Reset the dropdown so it doesn't stay on "import"
          shiny::updateSelectizeInput(session, "dataset",
            selected = if (!is.null(imported_data())) "imported" else "iris")
          # Navigate to the Upload tab
          shiny::updateTabsetPanel(session, "settings_tabs", selected = "Upload")
        } else {
          shiny::showModal(
            shiny::modalDialog(
              title = "Import a Data File",
              shiny::fileInput("file_upload", "Choose file:",
                               accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx", ".rds", ".sav", ".dta", ".sas7bdat"),
                               width = "100%",
                               multiple = FALSE),
              footer = shiny::tagList(
                shiny::actionButton("import_confirm", "Import", class = "btn-success"),
                shiny::actionButton("import_cancel", "Cancel", class = "btn-danger")
              )
            )
          )
        }
      }
    })
    shiny::observeEvent(input$import_cancel, {
      shiny::removeModal()
    })
    shiny::observeEvent(input$import_confirm, {
      shiny::req(input$file_upload)

      file_path <- input$file_upload$datapath
      file_name <- input$file_upload$name

      # Try to read with `rio::import`
      data <- tryCatch(
        rio::import(file_path),
        error = function(e) {
          shiny::updateSelectizeInput(session, "dataset", selected = "iris") # reset to something that always works
          shiny::showNotification("Failed to import file.", type = "error")
          return(NULL)
        }
      )

      if (!is.null(data)) {
        imported_data(data)
        choices <- data_sets
        choices$`Import data` <- c(stats::setNames("import", "Import another data set..."),
                                   stats::setNames("imported", paste0("Imported data (", paste(dim(data), collapse = " x "), ")")))
        shiny::updateSelectizeInput(session, "dataset", selected = "imported", choices = choices)
      }

      shiny::removeModal()
    })

    # --- Upload tab: auto-detect format when a file is chosen ----------------
    shiny::observeEvent(input$upload_file, {
      ext <- tolower(tools::file_ext(input$upload_file$name))
      # Normalise aliases
      ext <- switch(ext,
        rdata = "rda",
        yml   = "yaml",
        arrow = "feather",
        zsav  = "zsav",
        ext
      )
      if (ext %in% upload_formats) {
        shiny::updateSelectInput(session, "upload_format", selected = ext)
      }
    })

    # --- Upload tab: dynamic sheet selector for Excel / ODS ------------------
    output$upload_sheet_ui <- shiny::renderUI({
      shiny::req(input$upload_file)
      ext <- tolower(tools::file_ext(input$upload_file$name))
      if (ext %in% c("xlsx", "xls", "ods", "xlsb")) {
        sheets <- tryCatch(
          readxl::excel_sheets(input$upload_file$datapath),
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

    # --- Upload tab: import button -------------------------------------------
    shiny::observeEvent(input$upload_import_btn, {
      shiny::req(input$upload_file)

      file_path  <- input$upload_file$datapath
      file_label <- input$upload_file$name
      fmt        <- input$upload_format  # may be "" (auto-detect)

      # Build the argument list for rio::import()
      import_args <- list(file = file_path)
      if (nzchar(fmt)) {
        import_args$format <- fmt
      }

      if (fmt %in% c("csv", "tsv", "txt")) {
        import_args$sep      <- input$upload_delim
        import_args$header   <- isTRUE(input$upload_header)
        import_args$encoding <- input$upload_encoding
        import_args$skip     <- as.integer(input$upload_skip)
        na_str               <- trimws(strsplit(input$upload_na_strings, ",")[[1]])
        import_args$na.strings <- na_str[nzchar(na_str)]
        if (nzchar(input$upload_comment)) {
          import_args$comment.char <- input$upload_comment
        }
        import_args$quote <- input$upload_quote

      } else if (fmt %in% c("xlsx", "xls", "ods", "xlsb")) {
        if (!is.null(input$upload_sheet)) {
          import_args$which <- as.integer(input$upload_sheet)
        }
        import_args$col_names <- isTRUE(input$upload_xl_header)
        import_args$skip      <- as.integer(input$upload_xl_skip)
        if (nzchar(input$upload_xl_range)) {
          import_args$range <- input$upload_xl_range
        }

      } else if (fmt %in% c("sav", "zsav")) {
        import_args$encoding <- input$upload_stat_encoding
        import_args$user_na  <- isTRUE(input$upload_user_na)

      } else if (fmt == "dta") {
        import_args$encoding <- input$upload_stat_encoding
        import_args$labels   <- isTRUE(input$upload_dta_labels)

      } else if (fmt %in% c("sas7bdat", "xpt", "mtp")) {
        import_args$encoding <- input$upload_stat_encoding

      } else if (fmt %in% c("json", "ndjson")) {
        import_args$simplifyVector <- isTRUE(input$upload_json_simplify)
      }

      data <- tryCatch(
        do.call(rio::import, import_args),
        error = function(e) {
          shiny::showNotification(
            paste0("Import failed: ", conditionMessage(e)),
            type = "error", duration = 10
          )
          NULL
        }
      )

      if (!is.null(data)) {
        if (!is.data.frame(data)) {
          data <- tryCatch(as.data.frame(data), error = function(e) NULL)
        }
        if (!is.null(data)) {
          imported_data(data)
          choices <- data_sets
          choices$`Import data` <- c(
            stats::setNames("import", "Import another data set..."),
            stats::setNames("imported",
                            paste0(file_label, " (",
                                   paste(dim(data), collapse = " \u00d7 "), ")"))
          )
          shiny::updateSelectizeInput(session, "dataset",
            selected = "imported", choices = choices)
          shiny::updateTabsetPanel(session, "settings_tabs", selected = "Main")
          shiny::showNotification(
            paste0("Imported ", nrow(data), " rows \u00d7 ", ncol(data), " columns."),
            type = "message", duration = 4
          )
        }
      }
    })

    # --- Upload tab: status label and preview --------------------------------
    output$upload_status <- shiny::renderUI({
      d <- imported_data()
      if (!is.null(d)) {
        shiny::p(
          shiny::strong(paste0("Preview (first 5 of ", nrow(d), " rows):")),
          style = "margin-bottom: 4px; font-size: 0.85rem;"
        )
      }
    })
    output$upload_preview <- DT::renderDataTable({
      shiny::req(imported_data())
      head(imported_data(), 5)
    }, options = list(dom = "t", scrollX = TRUE, pageLength = 5),
       rownames = FALSE)
    
    shiny::observe({
      d <- switch(input$dataset,
                  "imported" = imported_data(),
                  import = NULL, # when clicking Import item that opens the modal
                  eval(parse(text = input$dataset)))
      
      if (!is.null(d) && !is.data.frame(d)) {
        d <- tryCatch(fortify_df(d)$new_df, error = function(x) NULL)
      }
      if (!is.null(d)) {
        if (inherits(d, "sf")) {
          shinyjs::disable("x")
          shinyjs::disable("y")
        } else {
          shinyjs::enable("x")
          shinyjs::enable("y")
        }
        
        changed_inputs$names <- character(0)
        
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
              'func': 'transparant'
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
             type +
             '</span>';
                }"
        
        render_dropdown <- sprintf("
          {
            option: function(data, escape) {
              return '<div>&nbsp;&nbsp;&nbsp;' +
                     (data.type !== 'func' ? %s(data.type) + '&nbsp;&nbsp;' : '') +
                     escape(data.column) +
                     '</div>';
            }
         }", selectize_style)
        
        vars <- names(d)
        types <- vapply(FUN.VALUE = character(1), vars, function(x) pillar::type_sum(d[[x]]))
        vars <- vars[types %unlike% "polygon"]
        types <- types[types %unlike% "polygon"]
        
        # Pass display items via options$options
        items <- mapply(function(v, t) {
          list(column = v, type = t, optgroup = "col_names")
        }, vars, types, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        
        functions_item <- list(list(column = "n()", type = "func", optgroup = "fns"))
        items_y <- c(functions_item, items)
        
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
          delimiter = ';',
          options = items,
          render = I(render_dropdown)
        )
        opt_y <- opt
        opt_y$options <- items_y
        
        shiny::updateSelectizeInput(session, "x", choices = NULL, options = opt, selected = "")
        shiny::updateSelectizeInput(session, "y", choices = NULL, options = opt_y, selected = "")
        shiny::updateSelectizeInput(session, "category", choices = NULL, options = opt, selected = "")
        shiny::updateSelectizeInput(session, "facet", choices = NULL, options = opt, selected = "")
      }
    })
    
    call_string <- shiny::reactive({
      plot2_args <- setdiff(names(formals(plot2)), c("...", ".data", "data"))
      arg_names  <- intersect(changed_inputs$names, plot2_args)
      # sort on this
      arg_names <- arg_names[order(ifelse(arg_names %in% c("type","x","y","category","facet"),
                                          match(arg_names, c("type","x","y","category","facet")),
                                          5 + rank(arg_names)))]
      
      args <- vapply(FUN.VALUE = character(1), arg_names, function(nm) {
        val <- input[[nm]]
        # plotting directions as unquoted strings, which also allow for multiple values
        if (nm %in% c("x", "y", "category", "facet")) {
          if (length(val) > 1) {
            out <- paste0("c(", paste0(val, collapse = ", "), ")")
          } else {
            out <- val
          }
          if (nm == "y" && input$y_calc != "None") {
            out <- paste0(gsub("[\\(\\)]", "", input$y_calc), "(", out, ")")
          }
          return(paste0(nm, " = ", out))
        } else if (nm == "category.type") {
          return(paste0(nm, " = c(", paste0('"', val, '"', collapse = ", "), ")"))
        }
        if (nm == "summarise_function") {
          # must be a function
          val <- eval(str2lang(val))
        } else if (nm == "type" && val == "auto") {
          val <- NULL
        } else if (is.character(val) && val %like% "^([0-9]|[.])+$") {
          val <- as.numeric(val)
        }
        if (val %in% c("TRUE", "FALSE", "NULL")) {
          val <- eval(str2lang(as.character(val)))
        }
        # skip NULL or empty
        if (is.null(val) || (is.character(val) && !nzchar(val))) return("")
        # skip if unchanged
        if (identical(plot2_env$shiny_defaults[[nm]], val)) return("")
        # booleans from switches
        if (is.logical(val) && length(val) == 1) {
          return(sprintf("%s = %s", nm, val))
        }
        # numbers
        if (is.numeric(val) && length(val) == 1) {
          return(sprintf("%s = %s", nm, val))
        }
        # everything else as quoted string
        return(sprintf('%s = "%s"', nm, val))
      })
      
      args <- args[!is.na(args) & nzchar(args)]
      
      # dataset
      df_name <- input$dataset
      if (df_name %in% c("Provided", "imported")) {
        df_name <- "your_df |>  # replace this with your data set"
      } else {
        df_name <- paste0(df_name, " |>")
      }
      
      if (length(args) == 0) {
        paste0(df_name, "\n  plot2()")
      } else if (length(args) == 1) {
        paste0(df_name, "\n  plot2(", args, ")")
      } else {
        paste0(df_name, "\n  plot2(\n    ", paste(args, collapse = ",\n    "), "\n  )")
      }
    })
    
    output$code <- shiny::renderText({
      call_string()
    })
    
    output$plot <- shiny::renderPlot({
      msgs <- character()
      output$error_msg <- shiny::renderText("")
      
      code <- strsplit(call_string(), "\n")[[1]]
      if (code[1] == "your_df |>  # replace this with your data set") {
        code[1] <- "Provided |>"
      }
      code <- paste(code, collapse = "\n")
      if (input$dataset == "imported") {
        Provided <- imported_data()
      }
      p <- withCallingHandlers(
        tryCatch(eval(parse(text = code)),
                 error = function(e) {
                   output$error_msg <- shiny::renderText("This code contains an error.")
                   plot2(NULL)
                 }),
        message = function(m) {
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      )
      
      plot2_env$last_shiny_plot <- p
      
      output$plot2_msgs <- shiny::renderUI({
        if (length(msgs) > 0) {
          shiny::tagList(lapply(msgs, function(m) {
            html <- fansi::sgr_to_html(m, warn = FALSE)
            shiny::div(shiny::HTML(html)) # render as HTML
          }))
        }
      })
      
      p
    }, res = 100)
    
    shiny::observeEvent(input$copy_btn, {
      clipr::write_clip(paste0(call_string(), "\n"), object_type = "character")
      shiny::showNotification("Code copied to clipboard.", duration = 2, closeButton = FALSE, type = "message")
    })
    
    output$export_pdf <- shiny::downloadHandler(
      filename = function() paste0("plot_", gsub("[^0-9_]", "", gsub(" ", "_", format(Sys.time()))), ".pdf"),
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot2_env$last_shiny_plot,
                        device = "pdf",
                        units = "cm",
                        width = input$export_width_cm,
                        height = input$export_height_cm)
                        
      }
    )
    output$export_svg <- shiny::downloadHandler(
      filename = function() paste0("plot_", gsub("[^0-9_]", "", gsub(" ", "_", format(Sys.time()))), ".svg"),
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot2_env$last_shiny_plot,
                        device = "svg",
                        units = "cm",
                        width = input$export_width_cm,
                        height = input$export_height_cm)
        
      }
    )
    output$export_png <- shiny::downloadHandler(
      filename = function() paste0("plot_", gsub("[^0-9_]", "", gsub(" ", "_", format(Sys.time()))), ".png"),
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot2_env$last_shiny_plot,
                        device = "png",
                        units = "px",
                        dpi = input$export_dpi,
                        width = input$export_width_px,
                        height = input$export_height_px)
        
      }
    )
    output$export_jpg <- shiny::downloadHandler(
      filename = function() paste0("plot_", gsub("[^0-9_]", "", gsub(" ", "_", format(Sys.time()))), ".jpg"),
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot2_env$last_shiny_plot,
                        device = "jpg",
                        units = "px",
                        dpi = input$export_dpi,
                        width = input$export_width_px,
                        height = input$export_height_px)
        
      }
    )
    
    
    shiny::observeEvent(input$showexport, {
      shinyjs::toggle("export")
    })
    shiny::observeEvent(input$showdata, {
      shinyjs::toggle("datatable")
    })
    output$datatable <- DT::renderDataTable({
      switch(input$dataset,
             "imported" = imported_data(),
             eval(parse(text = input$dataset)))
    })
  }
  
  if (!interactive()) {
    shiny::shinyApp(ui, server)
  } else if (rstudio_viewer) {
    suppressMessages(
      shiny::runGadget(
        app = ui,
        server = server,
        viewer = shiny::dialogViewer("Generate plot2", width = 1600, height = max_height),
        stopOnCancel = FALSE)
    )
  } else {
    suppressMessages(
      shiny::runApp(list(ui = ui, server = server))
    )
  }
}

# This function creates all the elements and makes sure they have sensible (default) values
make_input <- function(name, default, pretty_labels) {
  if (is.call(default)) {
    deparsed <- deparse(default)
    default <- tryCatch(eval(default), error = function(e) NULL)
    if (is.function(default)) {
      default <- deparsed
    }
  }
  
  if (name %like% "^colour" && identical(default, "ggplot2")) {
    default <- NULL
  }
  
  if ((is.logical(default) && !is.na(default) && name %unlike% "title$") || name %like% "[.](complete|character|zoom|drop|scientific|percent|fixed_y|fixed_x|repeat_lbls_x|repeat_lbls_y|date_remove_years)" || name == "smooth") {
    if (is.null(default)) default <- FALSE
    create_field(bslib::input_switch(name, NULL, value = default), name, default, pretty_labels)
  } else if (is.numeric(default) && is.null(default)) {
    create_field(shiny::numericInput(name, NULL, value = default, width = "100%"), name, default, pretty_labels)
  } else if (name %in% c("colour", "colour_fill")) {
    if (is.null(default)) default <- "ggplot2"
    registered <- names(Filter(function(x) length(x) > 1, plot2_env$reg_cols))
    if (length(registered) == 1) {
      registered <- list(registered)
    }
    create_field(shiny::selectizeInput(name, NULL, selected = default,
                                       choices = list("plot2 (registered colour sets)" = registered,
                                                      "viridis" = viridisLite_colours,
                                                      "Base R" = grDevices::palette.pals()),
                                       width = "100%", options = list(create = TRUE)), name, default, pretty_labels, class = "short")
  } else if (name %like% "angle$") {
    if (is.null(default) || is.infinite(default)) default <- 0
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = 360, step = 45, width = "100%"), name, default, pretty_labels, slider = TRUE)
  } else if (name %like% "(linewidth|size|width|text_factor)$") {
    if (is.null(default) || is.infinite(default)) default <- 1
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = 10, step = 0.5, width = "100%"), name, default, pretty_labels, slider = TRUE)
  } else if (name %like% "(nrow)$") {
    if (is.null(default) || is.infinite(default)) default <- 1
    create_field(shiny::sliderInput(name, NULL, value = default, min = 1, max = 10, step = 1, width = "100%"), name, default, pretty_labels, slider = TRUE)
  } else if (name %like% "alpha$") {
    if (is.null(default) || is.infinite(default)) default <- 0.4
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = 1, step = 0.05, width = "100%"), name, default, pretty_labels, slider = TRUE)
  } else if (name %like% "[.]level$") {
    if (is.null(default) || is.infinite(default)) default <- 0
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0.75, max = 1, step = 0.005, width = "100%"), name, default, pretty_labels, slider = TRUE)
  } else if (name == "category.type") {
    create_field(shiny::selectInput(name, NULL, selected = default, choices = c("colour/fill" = "colour", "shape", "size", "linetype", "linewidth", "alpha"), multiple = TRUE, width = "100%"), name, default, pretty_labels)
  } else if (name %like% "linetype$") {
    if (is.null(default) || is.infinite(default)) default <- 1
    create_field(shiny::selectInput(name, NULL, selected = default, choices = c("1 (solid)" = 1, "2 (dashed)" = 2, "3 (dotted)" = 3, "4 (dotdash)" = 4, "5 (longdash)" = 5, "6 (twodash)" = 6), width = "100%"), name, default, pretty_labels)
  } else if (name %like% "(max_items|n_breaks)$") {
    if (is.null(default) || is.infinite(default)) default <- 0
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = 25, step = 1, width = "100%"), name, default, pretty_labels, slider = TRUE)
  } else if (name %like% "[.]transform$") {
    options_transform <- sort(gsub("_trans$", "", ls(envir = asNamespace("scales"))[grepl("_trans$", ls(envir = asNamespace("scales")))]))
    names(options_transform) <- options_transform
    names(options_transform)[options_transform == "identity"] <- "None (identity)"
    create_field(shiny::selectInput(name, NULL, selected = default, choices = options_transform, width = "100%"), name, default, pretty_labels)
  } else if (name %like% "[.]position$") {
    create_field(shiny::selectInput(name, NULL, selected = default, choices = c("Top" = "top", "Right" = "right", "Bottom" = "bottom", "Left" = "left", "None" = "none"), width = "100%"), name, default, pretty_labels)
  } else if (name %like% "[.]sort$") {
    create_field(shiny::selectInput(name, NULL, selected = default, choices = c("Ascending A-Z" = "asc", "Descending A-Z" = "desc", "Ascending count" = "freq-asc", "Descending count" = "freq-desc", "Order of data" = "inorder"), width = "100%"), name, default, pretty_labels)
  } else if (is.numeric(default) && length(default) == 1) {
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = default * 3, width = "100%"), name, default, pretty_labels, slider = TRUE)
  } else {
    create_field(shiny::textInput(name, NULL, value = if (!is.symbol(default)) as.character(default) else "", width = "100%"), name, default, pretty_labels, pretty_labels)
  }
}

create_field <- function(inputTag, name, default, pretty_labels, slider = FALSE, class = NULL) {
  # store to list
  plot2_env$shiny_defaults[[name]] <- default
  
  if (pretty_labels == TRUE) {
    name <- gsub("_", " ", name)
    name <- gsub("^(x|y|category|facet|legend|y_secondary|datalabels|smooth|title|subtitle)[.]", "", name)
    name <- gsub("(lbl|txt)", "text", name)
    name <- gsub("^n([^aeiou][a-z]+)", "number of \\1s", name)
    name <- gsub("na.rm", "Remove missing", name, fixed = TRUE)
    name <- gsub("na.replace", "Replace missing", name, fixed = TRUE)
    name <- gsub(" breakss", "breaks", name)
    name <- gsub("^se$", "Standard Error", name)
    name <- gsub("^mic$", "MIC", name)
    name <- gsub("^sep$", "Separator character", name)
    name <- gsub("sankey", "Sankey:", name)
    name <- gsub(".", " ", name, fixed = TRUE)
    substr(name, 1, 1) <- toupper(substr(name, 1, 1))
  }
  
  # create element
  shiny::div(
    class = class,
    style = ifelse(slider, 
                   "display: flex; align-items: center; margin-bottom: -10px;",
                   "display: flex; align-items: center; margin-bottom: 4px;"),
    shiny::tags$label(if (pretty_labels) name else shiny::code(name),
                      style = ifelse(slider,
                                     "flex: 0 0 50%; margin: 0; padding-right: 4px; margin-bottom: 10px;",
                                     "flex: 0 0 50%; margin: 0; padding-right: 4px;")),
    shiny::div(style = "flex: 1;", inputTag)
  )
}

label_with_dims <- function(x) {
  dims <- tryCatch(dim(eval(parse(text = x))), error = function(e) NULL)
  if (is.null(dims)) return(x)
  paste0(gsub("(g?g?plot2|dplyr)::", "", x), " (", dims[1], " x ", dims[2], ")")
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
    "bslib",
    "clipr",
    "DT",
    "fansi",
    "pillar",
    "readxl",
    "rio",
    "shiny",
    "shinyjs"
  ))
}

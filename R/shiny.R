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
#' @details
#' ![Shiny app example](create_interactively.jpg)
#' @export
#' @examples
#' \dontrun{
#' 
#' create_interactively()
#' 
#' iris |> create_interactively()
#' }
create_interactively <- function(data = NULL) {
  
  rlang::check_installed("shiny")
  rlang::check_installed("clipr")
  rlang::check_installed("pillar")
  
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
  shiny::addResourcePath("plot2res", system.file(package = "plot2"))
  
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
  
  # Add dimensions to labels
  label_with_dims <- function(x) {
    dims <- tryCatch(dim(eval(parse(text = x))), error = function(e) NULL)
    if (is.null(dims)) return(x)
    paste0(gsub("(g?g?plot2|dplyr)::", "", x), " (", dims[1], " x ", dims[2], ")")
  }
  
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
    make_input(nm, main_args[[nm]])
  })
  x_args <- plot2_formals[grep("^x\\.", names(plot2_formals))]
  x_inputs <- lapply(names(x_args), function(nm) {
    make_input(nm, x_args[[nm]])
  })
  y_args <- plot2_formals[grep("^y\\.", names(plot2_formals))]
  y_inputs <- lapply(names(y_args), function(nm) {
    make_input(nm, y_args[[nm]])
  })
  category_args <- plot2_formals[grep("^(category\\.|category.type)", names(plot2_formals))]
  category_inputs <- lapply(names(category_args), function(nm) {
    make_input(nm, category_args[[nm]])
  })
  facet_args <- plot2_formals[grep("^facet\\.", names(plot2_formals))]
  facet_inputs <- lapply(names(facet_args), function(nm) {
    make_input(nm, facet_args[[nm]])
  })
  datalabels_args <- plot2_formals[grep("^datalabels", names(plot2_formals))]
  datalabels_inputs <- lapply(names(datalabels_args), function(nm) {
    make_input(nm, datalabels_args[[nm]])
  })
  legend_args <- plot2_formals[grep("^legend\\.", names(plot2_formals))]
  legend_args <- legend_args[names(legend_args) != "legend.title"]
  legend_inputs <- lapply(names(legend_args), function(nm) {
    make_input(nm, legend_args[[nm]])
  })
  title_args <- plot2_formals[grep("(title|subtitle|tag|caption)", names(plot2_formals))]
  title_args <- title_args[!names(title_args) %in% c(names(x_args), names(y_args), names(category_args), names(facet_args))]
  title_args <- title_args[!names(title_args) %like% "legend|y_secondary"]
  title_inputs <- lapply(names(title_args), function(nm) {
    make_input(nm, title_args[[nm]])
  })
  smooth_args <- plot2_formals[grep("^smooth", names(plot2_formals))]
  smooth_inputs <- lapply(names(smooth_args), function(nm) {
    make_input(nm, smooth_args[[nm]])
  })
  other_args <- plot2_formals[!names(plot2_formals) %in% c(".data", "x", "y", "category", "facet", "type", "data", "...", "print", names(main_args), names(x_args), names(y_args), names(category_args), names(facet_args), names(datalabels_args), names(legend_args), names(title_args), names(smooth_args))]
  other_args <- other_args[!names(other_args) %like% "y_secondary|legend[.]title"]
  other_inputs <- lapply(names(other_args), function(nm) {
    make_input(nm, other_args[[nm]])
  })
  
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
      "#sidebar { overflow-y: auto; ", ifelse(rstudio_viewer, paste0("height: ", max_height, "px; "), ""), "background-color: #f9f4f2; border-radius: 0; border: none; }"),
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
      ".settings * { margin-bottom: 2px; }",
      ".bslib-input-switch input { width: 3.5em !important; height: 30px !important; }",
      ".bslib-input-switch, .bslib-input-switch input { cursor: pointer; }",
      "code, .code, pre, .pre { font-family: FiraCode, \"Fira Code\", SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace; }"),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 5,
        id = "sidebar",
        shinyjs::useShinyjs(),
        
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
                                              shiny::HTML("<code>n_distinct()</code>"),
                                              shiny::HTML("<code>min()</code>"),
                                              shiny::HTML("<code>max()</code>"),
                                              shiny::HTML("<code>mean()</code>"),
                                              shiny::HTML("<code>median()</code>")
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
        shiny::uiOutput("plot2_msgs"),
        shiny::br(),
        shiny::p("Generated code:"),
        shiny::tags$div(
          class = "copy-container",
          shiny::actionButton("copy_btn", "Copy", class = "copy-button"),
          shiny::verbatimTextOutput("code")),
        shiny::textOutput("error_msg"),
        shiny::br(),
        shiny::div(
          id = "logo-container",
          shiny::img(src = "plot2res/logo.svg", height = "100px")
        ),
        shiny::actionLink("showdata", "Show data >"),
        shiny::br(),
        shiny::br(),
        DT::dataTableOutput("datatable")
      )
    )
  )
  
  server <- function(input, output, session) {
    shinyjs::hide("datatable")
    
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
      d <- eval(parse(text = input$dataset))
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
              ',oc': 'var(--bs-blue)',
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
        
        # Determine types
        types <- vapply(FUN.VALUE = character(1), vars, function(x) pillar::type_sum(d[[x]]))

        vars <- vars[types %unlike% "polygon"]
        types <- types[types %unlike% "polygon"]
        
        # Pass display items via options$options
        # items <- mapply(function(v, t) list(column = v, type = t), vars, types, SIMPLIFY = FALSE, USE.NAMES = FALSE)
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
      if (df_name == "Provided") {
        df_name <- "your_df |>  # replace this with your provided data set"
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
    
    output$code <- shiny::renderText(call_string())
    
    output$plot <- shiny::renderPlot({
      msgs <- character()
      output$error_msg <- shiny::renderText("")
      
      code <- strsplit(call_string(), "\n")[[1]]
      if (code[1] == "your_df |>  # replace this with your provided data set") {
        code[1] <- "Provided |>"
      }
      code <- paste(code, collapse = "\n")
      
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
      
      output$plot2_msgs <- shiny::renderUI({
        if (length(msgs) > 0) {
          shiny::tagList(lapply(msgs, function(m) {
            html <- fansi::sgr_to_shiny::HTML(m, warn = FALSE)
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
    
    shiny::observeEvent(input$showdata, {
      shinyjs::toggle("datatable")
    })
    output$datatable <- DT::renderDataTable({
      eval(parse(text = input$dataset))
    })
  }
  
  if (rstudio_viewer) {
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
make_input <- function(name, default) {
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
    create_field(bslib::input_switch(name, NULL, value = default), name, default)
  } else if (is.numeric(default) && is.null(default)) {
    create_field(shiny::numericInput(name, NULL, value = default, width = "100%"), name, default)
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
                                       width = "100%", options = list(create = TRUE)), name, default, class = "short")
  } else if (name %like% "angle$") {
    if (is.null(default) || is.infinite(default)) default <- 0
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = 360, step = 45, width = "100%"), name, default, slider = TRUE)
  } else if (name %like% "(linewidth|size|width|text_factor)$") {
    if (is.null(default) || is.infinite(default)) default <- 1
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = 10, step = 0.5, width = "100%"), name, default, slider = TRUE)
  } else if (name %like% "(nrow)$") {
    if (is.null(default) || is.infinite(default)) default <- 1
    create_field(shiny::sliderInput(name, NULL, value = default, min = 1, max = 10, step = 1, width = "100%"), name, default, slider = TRUE)
  } else if (name %like% "alpha$") {
    if (is.null(default) || is.infinite(default)) default <- 0.4
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = 1, step = 0.05, width = "100%"), name, default, slider = TRUE)
  } else if (name %like% "[.]level$") {
    if (is.null(default) || is.infinite(default)) default <- 0
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0.75, max = 1, step = 0.005, width = "100%"), name, default, slider = TRUE)
  } else if (name == "category.type") {
    create_field(shiny::selectInput(name, NULL, selected = default, choices = c("colour/fill" = "colour", "shape", "size", "linetype", "linewidth", "alpha"), multiple = TRUE, width = "100%"), name, default)
  } else if (name %like% "linetype$") {
    if (is.null(default) || is.infinite(default)) default <- 1
    create_field(shiny::selectInput(name, NULL, selected = default, choices = c("1 (solid)" = 1, "2 (dashed)" = 2, "3 (dotted)" = 3, "4 (dotdash)" = 4, "5 (longdash)" = 5, "6 (twodash)" = 6), width = "100%"), name, default)
  } else if (name %like% "(max_items|n_breaks)$") {
    if (is.null(default) || is.infinite(default)) default <- 0
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = 25, step = 1, width = "100%"), name, default, slider = TRUE)
  } else if (name %like% "[.]transform$") {
    options_transform <- sort(gsub("_trans$", "", ls(envir = asNamespace("scales"))[grepl("_trans$", ls(envir = asNamespace("scales")))]))
    names(options_transform) <- options_transform
    names(options_transform)[options_transform == "identity"] <- "None (identity)"
    create_field(shiny::selectInput(name, NULL, selected = default, choices = options_transform, width = "100%"), name, default)
  } else if (name %like% "[.]position$") {
    create_field(shiny::selectInput(name, NULL, selected = default, choices = c("Top" = "top", "Right" = "right", "Bottom" = "bottom", "Left" = "left", "None" = "none"), width = "100%"), name, default)
  } else if (name %like% "[.]sort$") {
    create_field(shiny::selectInput(name, NULL, selected = default, choices = c("Ascending A-Z" = "asc", "Descending A-Z" = "desc", "Ascending count" = "freq-asc", "Descending count" = "freq-desc", "Order of data" = "inorder"), width = "100%"), name, default)
  } else if (is.numeric(default) && length(default) == 1) {
    create_field(shiny::sliderInput(name, NULL, value = default, min = 0, max = default * 3, width = "100%"), name, default, slider = TRUE)
  } else {
    create_field(shiny::textInput(name, NULL, value = if (!is.symbol(default)) as.character(default) else "", width = "100%"), name, default)
  }
}

create_field <- function(inputTag, name, default, slider = FALSE, class = NULL) {
  # store to list
  plot2_env$shiny_defaults[[name]] <- default
  # create element
  shiny::div(
    class = class,
    style = ifelse(slider, 
                   "display: flex; align-items: center; margin-bottom: -10px;",
                   "display: flex; align-items: center; margin-bottom: 4px;"),
    shiny::tags$label(shiny::code(name),
                      style = ifelse(slider,
                                     "flex: 0 0 50%; margin: 0; padding-right: 4px; margin-bottom: 10px;",
                                     "flex: 0 0 50%; margin: 0; padding-right: 4px;")),
    shiny::div(style = "flex: 1;", inputTag)
  )
}

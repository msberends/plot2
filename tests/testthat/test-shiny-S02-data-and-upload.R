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

# Not part of the CRAN core, which must finish within 1 minute (#50)
skip_on_cran()

# S02: data sets and the upload tab

csv_file <- function(lines = c("g,v", "a,1", "b,3", "a,2")) {
  f <- tempfile(fileext = ".csv")
  writeLines(lines, f)
  f
}
csv_inputs <- function(session, ...) {
  vals <- list(upload_format = "csv", upload_delim = ",", upload_quote = '"', upload_header = TRUE,
               upload_skip = 0, upload_encoding = "UTF-8", upload_comment = "", upload_na_strings = "NA")
  do.call(session$setInputs, utils::modifyList(vals, list(...)))
}

test_that("S02.01 data passed to the app is offered as 'Provided', referred to by its name and plotted", {
  s02_unique_name <- data.frame(g = c("a", "b", "c"), v = 1:3)
  app <- shiny_app(data = s02_unique_name)
  shiny::testServer(app, {
    shiny_init(session, dataset = "Provided")
    expect_match(call_string(), "s02_unique_name |>", fixed = TRUE)
    invisible(output$plot)
    expect_identical(nrow(plot_result()$plot$data), 3L)
  })
})

test_that("S02.02 a global data frame with a non-syntactic name can be selected", {
  assign("my data", data.frame(a = 1:3), envir = globalenv())
  withr::defer(rm("my data", envir = globalenv()))
  withr::local_options(rlang_interactive = TRUE) # global data frames are only offered interactively
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session, dataset = "my data")
    expect_false(session$isClosed())
    expect_match(call_string(), "`my data` |>", fixed = TRUE)
  })
})

test_that("S02.03 a value of the data set selector that is not one of its choices is not evaluated", {
  marker <- tempfile()
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(dataset = sprintf('{writeLines("x", "%s"); iris}', marker))
  })
  expect_false(file.exists(marker))
})

test_that("S02.04 a CSV file is previewed first and only used after pressing Upload data", {
  f <- csv_file()
  app <- shiny_app(upload_tab = TRUE)
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(upload_file = shiny_file(f))
    csv_inputs(session)
    do_preview()
    expect_identical(nrow(preview_data()), 3L)
    expect_null(imported_data())
    session$setInputs(upload_import_btn = 1)
    expect_identical(nrow(imported_data()), 3L)
    session$setInputs(dataset = "imported")
    expect_match(call_string(), "your_df |>", fixed = TRUE)
  })
})

test_that("S02.05 the comment character, skip and NA strings of a CSV file are applied", {
  f <- csv_file(c("# note", "g,v", "a,1", "# c,2", "b,missing"))
  app <- shiny_app(upload_tab = TRUE)
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(upload_file = shiny_file(f))
    csv_inputs(session, upload_comment = "#", upload_na_strings = "missing")
    do_preview()
    expect_identical(nrow(preview_data()), 2L)
    expect_true(is.na(preview_data()$v[2]))
  })
})

test_that("S02.06 the encoding of a CSV file is chosen from the values the reader accepts", {
  f <- csv_file()
  app <- shiny_app(upload_tab = TRUE)
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(upload_file = shiny_file(f))
    csv_inputs(session, upload_encoding = "Latin-1")
    do_preview()
    expect_identical(nrow(preview_data()), 3L)
  })
})

test_that("S02.07 a .txt file with the default delimiter can be read", {
  f <- csv_file()
  app <- shiny_app(upload_tab = TRUE)
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(upload_file = shiny_file(f))
    csv_inputs(session, upload_format = "txt")
    do_preview()
    expect_identical(nrow(preview_data()), 3L)
  })
})

test_that("S02.08 an uploaded R serialisation file is not deserialised", {
  f <- tempfile(fileext = ".rds")
  saveRDS(data.frame(a = 1:2), f)
  app <- shiny_app(upload_tab = TRUE)
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(upload_file = shiny_file(f), upload_format = "rds")
    do_preview()
    expect_null(preview_data())
  })
})

test_that("S02.09 the app can be started without any way to upload files", {
  f <- csv_file()
  app <- shiny_app(upload_tab = FALSE)
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(upload_file = shiny_file(f))
    csv_inputs(session)
    session$setInputs(upload_import_btn = 1)
    expect_null(imported_data())
  })
})

test_that("S02.10 a missing reader package gives a notification and keeps the session", {
  skip_if(requireNamespace("readODS", quietly = TRUE), "readODS is installed")
  f <- tempfile(fileext = ".ods")
  writeLines("not really ods", f)
  app <- shiny_app(upload_tab = TRUE)
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(upload_file = shiny_file(f), upload_format = "ods")
    expect_no_error(do_preview())
    expect_null(preview_data())
  })
})

test_that("S02.11 data passed under a name that is also a function is still the data", {
  df <- data.frame(g = c("a", "b", "c"), v = 1:3)
  app <- shiny_app(data = df)
  value <- unname(shiny_app_env(app)$data_sets[[1]][1])
  shiny::testServer(app, {
    shiny_init(session, dataset = value)
    invisible(output$plot)
    expect_identical(nrow(plot_result()$plot$data), 3L)
    expect_match(call_string(), "df |>", fixed = TRUE)
  })
})

test_that("S02.12 values the browser sends outside the choices of an upload input are not used", {
  f <- csv_file()
  app <- shiny_app(upload_tab = TRUE)
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(upload_file = shiny_file(f))
    csv_inputs(session, upload_encoding = "latin1", upload_delim = "x", upload_skip = -3)
    do_preview()
    expect_identical(nrow(preview_data()), 3L)
    # an R serialisation file with format 'auto' is refused by its extension
    rds <- tempfile(fileext = ".rds")
    saveRDS(data.frame(a = 1), rds)
    session$setInputs(upload_file = shiny_file(rds), upload_format = "auto")
    do_preview()
    expect_null(preview_data())
  })
})

test_that("S02.13 without the upload tab, there is no tab and no item to upload", {
  app <- shiny_app(upload_tab = FALSE)
  html <- as.character(shiny_app_env(app)$ui)
  expect_false(grepl('data-value="Upload"', html, fixed = TRUE))
  expect_false("import" %in% unlist(shiny_app_env(app)$data_sets))
  html <- as.character(shiny_app_env(shiny_app(upload_tab = TRUE))$ui)
  expect_match(html, 'data-value="Upload"', fixed = TRUE)
  expect_false(grepl(".rds", html, fixed = TRUE))
})

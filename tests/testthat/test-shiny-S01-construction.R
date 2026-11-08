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

# R07 sheet S01: building the app (arguments, logo, inputs)
# review_and_overhaul/reviews/R07_shiny/S01_construction.md

test_that("S01.01 create_interactively() returns an app object outside an interactive session", {
  app <- shiny_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("S01.02 every plot2() argument has an input in the app", {
  skip_open_finding("F119")
  app <- shiny_app()
  html <- as.character(shiny_app_env(app)$ui)
  ids <- gsub('id=|"', "", regmatches(html, gregexpr('id="[^"]+"', html))[[1]])
  # legend.title is left out on purpose: category.title is the same setting (D001.12)
  args <- setdiff(names(formals(plot2)), c(".data", "data", "...", "print", "legend.title"))
  expect_identical(setdiff(args, ids), character(0))
})

test_that("S01.03 every slider has a range that can be used", {
  skip_open_finding("F119")
  app <- shiny_app()
  html <- as.character(shiny_app_env(app)$ui)
  sliders <- regmatches(html, gregexpr('<input class="js-range-slider"[^>]*>', html))[[1]]
  id <- sub('.*id="([^"]+)".*', "\\1", sliders)
  min <- as.numeric(sub('.*data-min="([^"]+)".*', "\\1", sliders))
  max <- as.numeric(sub('.*data-max="([^"]+)".*', "\\1", sliders))
  expect_identical(id[max <= min], character(0))
})

test_that("S01.04 a logo is served on its own, not together with the rest of its folder", {
  skip_open_finding("F113")
  dir <- tempfile("logo")
  dir.create(dir)
  file.copy(system.file("logo.svg", package = "plot2"), file.path(dir, "logo.svg"))
  writeLines("secret", file.path(dir, "secret.txt"))
  app <- shiny_app(logo_path = file.path(dir, "logo.svg"))
  served <- shiny::resourcePaths()[["plot2res"]]
  expect_false(file.exists(file.path(served, "secret.txt")))
})

test_that("S01.05 a logo that does not exist gives an error, NULL removes the logo", {
  expect_error(shiny_app(logo_path = tempfile(fileext = ".svg")), "does not exist")
  app <- shiny_app(logo_path = NULL)
  expect_false(grepl("plot2res", as.character(shiny_app_env(app)$ui)))
})

test_that("S01.06 pretty_labels shows readable labels instead of argument names", {
  app <- shiny_app(pretty_labels = TRUE)
  html <- as.character(shiny_app_env(app)$ui)
  expect_match(html, ">Max items<", fixed = TRUE)
  expect_false(grepl("<code>x.max_items</code>", html, fixed = TRUE))
})

test_that("S01.07 hide_generated_code and hide_export_buttons add their CSS", {
  app <- shiny_app(hide_generated_code = TRUE, hide_export_buttons = TRUE)
  html <- as.character(shiny_app_env(app)$ui)
  expect_match(html, ".generated-code { display: none; }", fixed = TRUE)
  expect_match(html, "#export, .show-export { display: none; }", fixed = TRUE)
})

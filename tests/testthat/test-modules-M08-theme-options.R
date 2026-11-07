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

# R04 sheet M08: theme_minimal2() and package options
# review_and_overhaul/reviews/R04_modules/M08_theme-and-options.md

el <- function(t, name) ggplot2::calc_element(name, ggplot2::complete_theme(t))

test_that("M08.01 complete theme with plot2's sizes", {
  t <- theme_minimal2()
  expect_true(isTRUE(attr(t, "complete")) || isTRUE(t@complete))
  expect_equal(as.numeric(el(t, "plot.title")@size), 13)
  expect_equal(as.numeric(el(t, "plot.subtitle")@size), 11)
})

test_that("M08.02 font colour options are honoured", {
  withr::local_options(plot2.colour_font_primary = "red", plot2.colour_font_secondary = "blue", plot2.colour_font_axis = "green")
  t <- theme_minimal2()
  expect_identical(col_hex(el(t, "plot.title")@colour), "#FF0000")
  expect_identical(col_hex(el(t, "plot.subtitle")@colour), "#0000FF")
  expect_identical(col_hex(el(t, "axis.title.x")@colour), "#00FF00")
})

test_that("M08.03 plot2.colour_background sets the backgrounds", {
  withr::local_options(plot2.colour_background = "yellow")
  t <- theme_minimal2()
  expect_identical(col_hex(el(t, "plot.background")@fill), "#FFFF00")
  expect_identical(col_hex(el(t, "panel.background")@fill), "#FFFF00")
})

test_that("M08.04 + theme(...) merges with plot2's settings", {
  t <- theme_minimal2() + ggplot2::theme(plot.title = ggplot2::element_text(face = "italic"))
  expect_identical(el(t, "plot.title")@face, "italic")
  expect_equal(as.numeric(el(t, "plot.title")@size), 13)
})

test_that("M08.05 theme_minimal2(...) merges as well (F087)", {
  skip_open_finding("F087")
  t <- theme_minimal2(plot.title = ggplot2::element_text(face = "italic"))
  expect_equal(as.numeric(el(t, "plot.title")@size), 13)
})

test_that("M08.06 every plot2.* option used in the code is documented", {
  src <- list.files(testthat::test_path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE)
  skip_if(length(src) == 0, "package sources not available")
  code <- unlist(lapply(src, readLines, warn = FALSE))
  used <- unique(regmatches(code, regexpr("getOption\\(\"plot2[.][a-z_.]+", code)))
  used <- sub("getOption\\(\"", "", used)
  documented <- readLines(testthat::test_path("..", "..", "R", "options.R"), warn = FALSE)
  missing <- used[!vapply(used, function(o) any(grepl(paste0("`", o, "`"), documented, fixed = TRUE)), logical(1))]
  expect_identical(missing, character(0))
})

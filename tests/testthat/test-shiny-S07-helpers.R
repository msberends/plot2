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

# R07 sheet S07: helpers of the app
# review_and_overhaul/reviews/R07_shiny/S07_helpers.md

test_that("S07.01 logical arguments get a switch, sizes a slider, sorting a selector", {
  switch_tag <- as.character(make_input("x.zoom", FALSE, FALSE))
  expect_match(switch_tag, "bslib-input-switch", fixed = TRUE)
  expect_match(as.character(make_input("datalabels.size", 3, FALSE)), "js-range-slider", fixed = TRUE)
  expect_match(as.character(make_input("x.sort", NULL, FALSE)), "freq-desc", fixed = TRUE)
})

test_that("S07.02 pretty labels drop the argument prefix", {
  lab <- as.character(create_field(shiny::div(), "x.max_items", 1, TRUE))
  expect_match(lab, ">Max items<", fixed = TRUE)
  lab <- as.character(create_field(shiny::div(), "na.rm", TRUE, TRUE))
  expect_match(lab, ">Remove missing<", fixed = TRUE)
})

test_that("S07.03 data set labels carry the dimensions", {
  expect_identical(label_with_dims("iris"), "iris (150 x 5)")
  expect_identical(label_with_dims("ggplot2::mpg"), "mpg (234 x 11)")
  expect_identical(label_with_dims("no_such_object_anywhere"), "no_such_object_anywhere")
})

test_that("S07.04 install_shiny_deps() lists every package the app calls", {
  src <- readLines(source_file("R", "shiny.R"))
  used <- unique(unlist(regmatches(src, gregexpr("[A-Za-z0-9.]+(?=::)", src, perl = TRUE))))
  base <- c("stats", "utils", "tools", "grDevices", "rlang", "ggplot2", "plot2", "dplyr")
  dep <- deparse(body(install_shiny_deps))
  listed <- gsub('"', "", unlist(regmatches(dep, gregexpr('"[^"]+"', dep))))
  expect_identical(setdiff(setdiff(used, base), listed), character(0))
})

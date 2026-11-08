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

# R04 sheet M01: plot2.default()
# review_and_overhaul/reviews/R04_modules/M01_default-method.md

test_that("M01.01 numeric vector: columns with index and values", {
  v <- c(3, 1, 2)
  p <- expect_plot2(plot2(v))
  expect_geoms(p, "bar")
  expect_identical(as.numeric(layer_df(p, "bar")$y), c(3, 1, 2))
})

test_that("M01.02 character vector: counts per value", {
  v <- c("b", "a", "b")
  p <- expect_plot2(plot2(v))
  expect_identical(axis_labels(p, "x"), c("a", "b"))
  expect_identical(as.numeric(layer_df(p, "bar")$y), c(1, 2))
})

test_that("M01.03 factor vector keeps the level order", {
  v <- factor(c("lo", "hi", "hi"), levels = c("lo", "hi"))
  expect_identical(axis_labels(plot2(v), "x"), c("lo", "hi"))
})

test_that("M01.04 logical vector: counts for FALSE and TRUE", {
  v <- c(TRUE, FALSE, TRUE)
  p <- expect_plot2(plot2(v))
  expect_identical(axis_labels(p, "x"), c("FALSE", "TRUE"))
})

test_that("M01.05 the plot2.default_type option sets the type for vectors", {
  withr::local_options(plot2.default_type = "point")
  v <- c(3, 1, 2)
  expect_geoms(plot2(v), "point", exact = TRUE)
})

test_that("M01.06 a one-dimensional table gives counts", {
  t <- table(c("a", "b", "b"))
  expect_identical(as.numeric(layer_df(expect_plot2(plot2(t)), "bar")$y), c(1, 2))
})

test_that("M01.07 an lm object is plotted via fortify", {
  m <- stats::lm(mpg ~ wt, data = mtcars)
  expect_geoms(expect_plot2(plot2(m)), "point")
})

test_that("M01.08 a function errors clearly", {
  expect_error(plot2(mean), "does not yet support functions")
})

test_that("M01.09 numeric vector with y given (F030)", {
  v <- c(3, 1, 2)
  expect_plot2(plot2(v, y = c(1, 2, 3)))
})

test_that("M01.10 a single string names a data frame only in the calling environment (F030)", {
  mydf <- data.frame(x = c("a", "b"), y = 1:2)
  expect_identical(nrow(plot_data(plot2("mydf"))), 2L)
  # not found in the calling environment (package data, search path): a value
  expect_identical(nrow(plot_data(plot2("admitted_patients"))), 1L)
  f <- function() plot2("mydf")
  expect_identical(nrow(plot_data(f())), 1L)
})

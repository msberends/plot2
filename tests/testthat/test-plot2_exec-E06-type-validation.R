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

# R03 sheet E06: taxonomy, type validation and cleaning steps
# review_and_overhaul/reviews/R03_plot2_exec/E06_type-validation-and-cleaning.md

test_that("E06.01 discrete type with duplicate x: values are summed", {
  p <- expect_plot2(plot2(fx_cat, x, y))
  expect_identical(as.numeric(layer_df(p, "bar")$y), c(3, 7, 11))
})

test_that("E06.02 barpercent reports the summarising function", {
  expect_plot2_message(plot2(fx_cat, x, y, type = "bp"), "Summarising values", type = "info")
})

test_that("E06.03 zoom = TRUE: the y range follows the data, not 0", {
  df <- data.frame(x = c("a", "b"), y = c(100, 110))
  expect_gt(min(axis_range(plot2(df, x, y, type = "point", zoom = TRUE), "y")), 50)
  expect_lte(min(axis_range(plot2(df, x, y, type = "point"), "y")), 0)
})

test_that("E06.04 point plots have no datalabels unless asked", {
  expect_false("GeomText" %in% geoms(plot2(fx_xy, x, y, type = "point")))
  expect_true("GeomText" %in% geoms(plot2(fx_xy, x, y, datalabels = TRUE)))
})

test_that("E06.04b datalabels = TRUE with an explicit continuous type (F076)", {
  expect_true("GeomText" %in% geoms(plot2(fx_xy, x, y, type = "point", datalabels = TRUE)))
  expect_true("GeomText" %in% geoms(plot2(fx_xy, x, y, type = "line", datalabels = y)))
})

test_that("E06.05 density ignores y, with a message", {
  expect_plot2_message(plot2(fx_xy, x, y, type = "density"), "Ignoring")
})

test_that("E06.06 Sankey without category errors with a clear message", {
  skip_if_not_installed("ggforce")
  sk <- data.frame(from = c("A", "A", "B", "B"), to = c("X", "Y", "X", "Y"), n = c(5, 3, 2, 6))
  expect_error(plot2(sk, x = c(from, to), y = n, type = "sankey"), "category")
})

test_that("E06.07 back-to-back with three facet values errors", {
  df <- data.frame(x = rep(c("a", "b"), 3), y = 1:6, f = rep(c("p", "q", "r"), each = 2))
  expect_error(plot2(df, x, y, facet = f, type = "b2b"))
})

test_that("E06.08 stacked and stacked_fill both TRUE: warning, fill wins", {
  p <- expect_plot2_message(plot2(fx_cat_unique, x, y, category, stacked = TRUE, stacked_fill = TRUE), "stacked", type = "warning")
  expect_true("PositionFill" %in% positions(p))
})

test_that("E06.09 size on a line plot becomes linewidth, with a warning", {
  p <- expect_plot2_message(plot2(fx_xy, x, y, type = "line", size = 2), "linewidth", type = "warning")
  expect_identical(unique(layer_df(p, "line")$linewidth), 2)
})

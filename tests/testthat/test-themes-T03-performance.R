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

# R06 sheet T03: performance (structural checks; timings are in review_and_overhaul/repro/perf_r06.R)
# review_and_overhaul/reviews/R06_themes/T03_performance.md


test_that("T03.01 datalabels are not formatted for a scatter plot, which never shows them", {
  d <- data.frame(v = runif(1000), w = runif(1000))
  n <- 0L
  trace("format_datalabels", tracer = function() n <<- n + 1L, print = FALSE, where = asNamespace("plot2"))
  withr::defer(untrace("format_datalabels", where = asNamespace("plot2")))
  plot2(d, v, w)
  expect_identical(n, 0L)
})

test_that("T03.02 many x values build without datalabels", {
  d <- data.frame(g = sprintf("x%04d", 1:1000), v = 1)
  p <- plot2(d, g, v)
  expect_length(datalabels_of(p), 0)
})

test_that("T03.03 a date axis with many rows is aggregated before plotting", {
  d <- data.frame(dt = as.Date("2020-01-01") + sample(0:30, 1e4, TRUE))
  p <- plot2(d, dt, n())
  expect_lte(nrow(layer_df(p, layer_index(p, "GeomBar"))), 31)
})

test_that("T03.04 without y and without a numeric column, rows are counted", {
  d <- data.frame(h = c("a", "b", "b"))
  p <- plot2(d, h)
  expect_identical(sort(layer_df(p, layer_index(p, "GeomBar"))$y), c(1, 2))
})

test_that("T03.05 a Sankey diagram can be drawn from raw rows", {
  d <- data.frame(h = c("a", "b", "b"), k = c("x", "y", "y"))
  p <- expect_plot2(plot2(d, x = c(h, k), category = h, type = "sankey"))
  expect_true(nrow(layer_df(p, 1)) > 0)
})

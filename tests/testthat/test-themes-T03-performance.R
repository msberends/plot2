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

# T03: performance (structural checks)


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
  skip_if_not_installed("ggforce")
  d <- data.frame(h = c("a", "b", "b"), k = c("x", "y", "y"))
  p <- expect_plot2(plot2(d, x = c(h, k), category = h, type = "sankey"))
  expect_true(nrow(layer_df(p, 1)) > 0)
})

test_that("T03.06 the complete default theme is built once per session (#122)", {
  tiny <- data.frame(g = letters[1:5], v = 1:5)
  invisible(plot2(tiny, g, v))
  n <- 0L
  trace("theme_minimal2_build", tracer = function() n <<- n + 1L, print = FALSE, where = asNamespace("plot2"))
  withr::defer(untrace("theme_minimal2_build", where = asNamespace("plot2")))
  for (i in 1:3) plot2(tiny, g, v)
  expect_identical(n, 0L)
})

test_that("T03.07 the fixed overhead per call stays bounded (#122 benchmark guard)", {
  # timing guard: local runs only, with a generous bound (0.058 s per call on the
  # installed build in 2026-11, 0.11 s before #122); it catches regressions of a
  # different order, such as rebuilding the complete theme on every call
  skip_on_cran()
  skip_on_ci()
  tiny <- data.frame(g = letters[1:5], v = 1:5)
  # warm up: the JIT compiler compiles the functions during the first calls
  for (i in 1:10) plot2(tiny, g, v)
  per_call <- system.time(for (i in 1:10) plot2(tiny, g, v))[["elapsed"]] / 10
  expect_lt(per_call, 0.25)
})

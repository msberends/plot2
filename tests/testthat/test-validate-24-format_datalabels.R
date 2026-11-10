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

# 24: format_datalabels() (#157)

dl <- data.frame(x = c("a", "b", "c"), y = c(1.234, 2.5, 10))

test_that("24.01 default: one decimal, with the decimal mark", {
  expect_identical(datalabels_of(plot2(dl, x, y)), c("1.2", "2.5", "10.0"))
  expect_identical(datalabels_of(plot2(dl, x, y, decimal.mark = ",")), c("1,2", "2,5", "10,0"))
})

test_that("24.02 '%n (%p)' gives the value and its share", {
  expect_identical(datalabels_of(plot2(dl, x, y, datalabels.format = "%n (%p)")), c("1.2 (9.0%)", "2.5 (18.2%)", "10.0 (72.8%)"))
})

test_that("24.03 y.percent: percentage labels, datalabels.round honoured, other formats ignored with a message", {
  d <- data.frame(x = c("a", "b"), y = c(0.12345, 0.5))
  expect_identical(datalabels_of(plot2(d, x, y, y.percent = TRUE, datalabels.round = 2)), c("12.35%", "50.00%"))
  expect_identical(datalabels_of(plot2(d, x, y, y.percent = TRUE, datalabels.round = 0)), c("12%", "50%"))
  expect_plot2_message(plot2(fx_pct, x, y, y.percent = TRUE, datalabels.format = "%n (%p)"),
                       "Ignoring datalabels.format = .* since y.percent = TRUE")
})

test_that("24.04 a function as datalabels.format", {
  expect_identical(datalabels_of(plot2(dl, x, y, datalabels.format = function(x) paste0("~", x))), c("~1.234", "~2.5", "~10"))
})

test_that("24.05 zero values are not labelled", {
  expect_identical(datalabels_of(plot2(data.frame(x = c("a", "b", "c"), y = c(0, 2, 3)), x, y)), c(NA, "2", "3"))
})

test_that("24.06 big.mark in large values", {
  d <- data.frame(x = c("a", "b"), y = c(1234567, 2000))
  expect_identical(datalabels_of(plot2(d, x, y, big.mark = ".", decimal.mark = ",")), c("1.234.567", "2.000"))
})

test_that("24.07 character datalabels are kept as they are", {
  d <- data.frame(x = c("a", "b"), y = 1:2, name = c("first", "second"))
  expect_identical(datalabels_of(plot2(d, x, y, datalabels = name)), c("first", "second"))
})

test_that("24.08 labels are not formatted when none are drawn (#37)", {
  n <- 0L
  trace("format_datalabels", tracer = function() n <<- n + 1L, print = FALSE, where = asNamespace("plot2"))
  withr::defer(untrace("format_datalabels", where = asNamespace("plot2")))
  plot2(data.frame(g = sprintf("g%02d", 1:26), v = 1:26), g, v)
  expect_identical(n, 0L)
})

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

# I002, R02 sheet 06: validate_y_scale()
# review_and_overhaul/reviews/R02_validate/06_validate_y_scale.md

d100 <- data.frame(x = c("a", "b", "c"), y = c(12, 55, 97))

test_that("06.01 data from 0 to 100 get round breaks from zero", {
  p <- expect_plot2(plot2(d100, x, y))
  expect_axis_breaks(p, "y", c(0, 20, 40, 60, 80, 100, 120))
  expect_identical(axis_range(p, "y")[1], 0)
})

test_that("06.02 integer data from -3 to 2 get negative breaks", {
  p <- expect_plot2(plot2(fx_neg, x, y))
  expect_true(any(axis_breaks(p, "y") < 0))
})

test_that("06.03 y.percent = TRUE gives percent labels every 10%, aligned also with negative values", {
  p <- expect_plot2(plot2(fx_pct, x, y, y.percent = TRUE))
  expect_axis_breaks(p, "y", seq(0, 1, 0.1))
  expect_true(all(endsWith(axis_labels(p, "y"), "%")))
  p <- expect_plot2(plot2(data.frame(x = c("a", "b", "c"), y = c(-0.13, 0.27, 0.61)), x, y, y.percent = TRUE))
  expect_true(all(abs(axis_breaks(p, "y") * 10 - round(axis_breaks(p, "y") * 10)) < 1e-8))
})

test_that("06.04 y.percent_break is adjusted to keep about 10 labels, with a message", {
  d <- data.frame(x = c("a", "b"), y = c(0.01, 3.5))
  p <- expect_plot2_message(plot2(d, x, y, y.percent = TRUE), "y.percent_break = ", type = "info")
  expect_lte(length(axis_breaks(p, "y")), 11)
})

test_that("06.05 stacked_fill = TRUE gives limits 0 to 1 without expansion and percent labels", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, stacked_fill = TRUE))
  expect_equal(axis_range(p, "y"), c(0, 1))
  expect_identical(axis_labels(p, "y")[c(1, 11)], c("0%", "100%"))
})

test_that("06.06 y.age = TRUE gives breaks by 10 with an age unit", {
  withr::local_locale(c(LC_COLLATE = "C"))
  p <- expect_plot2(plot2(data.frame(x = c("a", "b"), y = c(23, 78)), x, y, y.age = TRUE))
  expect_identical(unique(diff(axis_breaks(p, "y"))), 10)
  expect_identical(axis_labels(p, "y")[2], "10 yrs")
})

test_that("06.07 y.24h = TRUE gives breaks by 24", {
  p <- expect_plot2(plot2(data.frame(x = c("a", "b"), y = c(5, 100)), x, y, y.24h = TRUE))
  expect_identical(unique(diff(axis_breaks(p, "y"))), 24)
})

test_that("06.08 y.scientific = TRUE gives scientific labels", {
  p <- expect_plot2(plot2(data.frame(x = c("a", "b"), y = c(5, 100)), x, y, y.scientific = TRUE))
  expect_match(axis_labels(p, "y")[2], "10^1", fixed = TRUE)
})

test_that("06.09 y.limits = c(0, 10) with negative data keeps 0 as the lower limit", {
  # ggplot2 warns that the negative bars fall outside the limits the user set
  p <- expect_plot2(plot2(fx_neg, x, y, y.limits = c(0, 10)), allow_warnings = TRUE)
  expect_equal(suppressWarnings(axis_range(p, "y")), c(0, 10))
})

test_that("06.10 y.limits of length 3 is an error; length 1 does not remove all data", {
  expect_error(plot2(d100, x, y, y.limits = c(1, 2, 3)), "length 1 or 2")
  skip_open_finding("F130")
  p <- expect_plot2(plot2(d100, x, y, y.limits = 50))
  expect_identical(nrow(layer_df(p, "bar")), 3L)
})

test_that("06.11 y.transform = 'log10' with zeros does not error and drops the zero limit", {
  d <- data.frame(x = c("a", "b", "c"), y = c(0, 10, 100))
  p <- expect_plot2(plot2(d, x, y, y.transform = "log10"), allow_warnings = TRUE)
  b <- suppressWarnings(built(p)) # ggplot2 warns about log10(0) of the data itself
  expect_true(all(is.finite(axis_range(b, "y"))))
  expect_identical(axis_labels(b, "y"), c("1", "10", "100"))
})

test_that("06.12 y.transform accepts a transformation object", {
  p <- expect_plot2(plot2(d100, x, y, y.transform = scales::transform_log10()))
  expect_identical(scale_of(p, "y")$trans$name, "log-10")
})

test_that("06.13 y.transform = 'reverse' keeps the expansion on the right sides", {
  p <- expect_plot2(plot2(d100, x, y, y.transform = "reverse", y.expand = c(0.1, 0.5)))
  r <- axis_range(p, "y")
  expect_equal(r, c(-97 - 0.5 * 97, 0 + 0.1 * 97))
})

test_that("06.14 facets with similar y ranges share the y scale, with a message; facet.fixed_y = FALSE frees them", {
  p <- expect_plot2_message(plot2(fx_facet_similar, x, y, facet = facet), "facet.fixed_y = TRUE", type = "info")
  expect_identical(axis_range(p, "y", panel = 1), axis_range(p, "y", panel = 2))
  p <- expect_no_plot2_message(plot2(fx_facet_similar, x, y, facet = facet, facet.fixed_y = FALSE), "facet.fixed_y")
  expect_false(identical(axis_range(p, "y", panel = 1), axis_range(p, "y", panel = 2)))
})

test_that("06.15 stacked columns with fixed facets reach the largest stacked sum", {
  d <- data.frame(x = rep(c("a", "b"), 4), y = 1:8, c = rep(c("u", "u", "v", "v"), 2), f = rep(c("p", "q"), each = 4))
  p <- expect_plot2(plot2(d, x, y, c, f, stacked = TRUE, facet.fixed_y = TRUE))
  expect_identical(scale_limits(p, "y")[2], 14)
})

test_that("06.16 all-zero integer y gets limits 0 to 8", {
  p <- expect_plot2(plot2(data.frame(x = c("a", "b"), y = c(0L, 0L)), x, y))
  expect_equal(scale_limits(p, "y"), c(0, 8))
})

test_that("06.17 y.position = 'right' puts the axis on the right", {
  p <- expect_plot2(plot2(d100, x, y, y.position = "right"))
  expect_identical(scale_of(p, "y")$position, "right")
})

test_that("06.18 a secondary axis is drawn; a zero series does not error", {
  d <- data.frame(x = c("a", "b", "c"), y = c(10, 20, 30), z = c(0.1, 0.5, 0.9))
  p <- expect_plot2(plot2(d, x, y, y_secondary = z))
  expect_false(inherits(scale_of(p, "y")$secondary.axis, "waiver"))
  d$z <- 0
  expect_plot2(plot2(d, x, y, y_secondary = z))
})

test_that("06.19 a character y gives a clear error; a factor y works with datalabels", {
  expect_error(plot2(data.frame(x = c("a", "b"), y = c("p", "q")), x, y, type = "col"), "must be numeric")
  expect_identical(axis_class(plot2(data.frame(x = c("a", "b"), y = factor(c("p", "q"))), x, y, type = "point"), "y"), "discrete")
  expect_plot2(plot2(data.frame(x = c("a", "b"), y = factor(c("p", "q"))), x, y, datalabels = TRUE))
})

test_that("06.20 rendering the plot queues no plot2 messages", {
  p <- plot2(data.frame(x = c("a", "b"), y = c(5, 5e7)), x, y) # label function decides on scientific notation
  plot2:::clear_plot2_env()
  invisible(rendered(p))
  expect_identical(setdiff(ls(plot2:::plot2_env), "reg_cols"), character(0))
})

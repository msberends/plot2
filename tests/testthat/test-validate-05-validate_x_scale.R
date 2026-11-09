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

# 05: validate_x_scale() (#157)

test_that("05.01 a character x gives a discrete scale; x.drop = FALSE keeps unused levels", {
  d <- data.frame(x = factor(c("a", "b"), levels = c("a", "b", "c")), y = 1:2)
  expect_identical(axis_class(plot2(d, x, y), "x"), "discrete")
  expect_identical(axis_labels(expect_plot2(plot2(d, x, y, x.drop = FALSE)), "x"), c("a", "b", "c"))
  expect_identical(axis_labels(expect_plot2(plot2(d, x, y, x.drop = TRUE)), "x"), c("a", "b"))
})

test_that("05.02 numeric x gets pretty breaks, including negative ones for small integers", {
  p <- expect_plot2(plot2(fx_xy, x, y))
  expect_axis_breaks(p, "x", c(0, 1, 2, 3, 4, 5, 6))
  p <- expect_plot2(plot2(data.frame(x = -3:4, y = 1:8), x, y, type = "point"))
  expect_true(any(axis_breaks(p, "x") < 0))
})

test_that("05.03 x.limits on a numeric x sets the panel range, with a message about x.expand = 0", {
  p <- expect_plot2_message(plot2(fx_xy, x, y, x.limits = c(0, 10)), "x.expand = 0", type = "info")
  expect_identical(scale_limits(p, "x"), c(0, 10))
  expect_equal(axis_range(p, "x"), c(0, 10))
})

test_that("05.04 x.limits on a discrete x is applied or gives a clear warning", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, x.limits = c("a", "b")))
  expect_identical(axis_labels(p, "x"), c("a", "b"))
})

test_that("05.05 x.limits of the wrong class is reported", {
  # the warnings collected before the error are still emitted
  expect_error(suppressWarnings(plot2(fx_xy, x, y, x.limits = c("a", "b"))), "limits")
  expect_plot2_message(plot2(fx_cat_unique, x, y, x.limits = c(1, 2)), "x.limits.*class", type = "warning")
})

test_that("05.06 x.limits of length 3 is an error", {
  expect_error(plot2(fx_xy, x, y, x.limits = c(1, 2, 3)), "must be of length 2")
})

test_that("05.07 a date x gets automatic breaks and labels depending on its range", {
  ranges <- c(7, 31, 365, 3650)
  breaks <- c("1 day", "1 day", "1 month", "2 years")
  for (i in seq_along(ranges)) {
    d <- data.frame(d = as.Date("2024-01-01") + round(seq(0, ranges[i], length.out = 10)), v = 1:10)
    p <- expect_plot2_message(plot2(d, d, v), paste0("x.date_breaks = \"", breaks[i], "\""), type = "info")
    expect_identical(axis_class(p, "x"), "date")
  }
})

test_that("05.08 a POSIXct x within one day gets hour breaks", {
  p <- expect_plot2_message(plot2(fx_datetimes, time, y), "x.date_breaks = \"2 hours\"", type = "info")
  expect_identical(axis_class(p, "x"), "datetime")
  expect_identical(axis_labels(p, "x")[1:3], c("00", "02", "04"))
})

test_that("05.09 x.date_breaks and x.date_labels are honoured", {
  p <- expect_no_plot2_message(plot2(fx_dates, date, y, x.date_breaks = "1 month", x.date_labels = "yyyy-mm"), "x.date_")
  expect_identical(axis_labels(p, "x"), c("2024-01", "2024-02", "2024-03"))
})

test_that("05.10 x.transform = 'log10' drops the zero lower limit and gives logarithmic breaks", {
  d <- data.frame(x = c(1, 10, 100, 1000), y = 1:4)
  p <- expect_plot2(plot2(d, x, y, x.transform = "log10"))
  expect_true(all(is.finite(axis_range(p, "x"))))
  expect_identical(axis_labels(p, "x"), c("1", "10", "100", "1 000"))
})

test_that("05.11 horizontal = TRUE with a numeric x reverses the scale", {
  p <- expect_plot2(plot2(fx_xy, x, y, horizontal = TRUE, type = "point"))
  expect_identical(scale_of(p, "x")$trans$name, "reverse")
  expect_identical(coord_class(p), "CoordFlip")
})

test_that("05.12 x.position = 'top' puts the axis on top", {
  p <- expect_plot2(plot2(fx_xy, x, y, x.position = "top"))
  expect_identical(scale_of(p, "x")$position, "top")
})

test_that("05.13 facets with similar x ranges share the x range, dissimilar ones do not; both with a message (#64)", {
  d <- data.frame(x = rep(1:3, 2), y = 1:6, f = rep(c("a", "b"), each = 3))
  p <- expect_plot2_message(plot2(d, x, y, facet = f, type = "point"), "facet.fixed_x = TRUE", type = "info")
  expect_identical(axis_range(p, "x", panel = 1), axis_range(p, "x", panel = 2))
  d$x <- c(1:3, 100:102)
  p <- expect_plot2_message(plot2(d, x, y, facet = f, type = "point"), "Using free x scales", type = "info")
  expect_false(identical(axis_range(p, "x", panel = 1), axis_range(p, "x", panel = 2)))
})

test_that("05.14 x.zoom = TRUE makes the axis follow the data", {
  d <- data.frame(x = c(5, 6, 9), y = 1:3)
  p <- expect_plot2(plot2(d, x, y, x.zoom = TRUE))
  expect_false(0 %in% axis_breaks(p, "x"))
  # 5% expansion on both sides (#104)
  expect_equal(axis_range(p, "x"), c(4.8, 9.2))
})

test_that("05.15 x.breaks and an x.labels function are honoured", {
  p <- expect_plot2(plot2(fx_xy, x, y, x.breaks = c(1, 3, 5), x.labels = function(x) paste0(x, "!")))
  expect_axis_breaks(p, "x", c(1, 3, 5))
  expect_axis_labels(p, "x", c("1!", "3!", "5!"))
})

test_that("05.16 the MIC scale of the AMR package can be added to a plot (API decisions in CLAUDE.md)", {
  skip_if_not_installed("AMR")
  d <- data.frame(mic = AMR::as.mic(c("0.25", "1", "4", "1")), n = c(2, 5, 3, 1))
  p <- expect_plot2(suppressMessages(plot2(d, mic, n) + AMR::scale_x_mic()))
  expect_identical(axis_labels(p, "x"), c("<=0.25", "0.5", "1", "2", ">=4")) # MIC steps, with operators at the edges
})

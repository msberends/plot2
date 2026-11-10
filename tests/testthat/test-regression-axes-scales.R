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

# Regression tests for axes, scales and facet scales.
# Each test_that() starts with the GitHub issue number.

test_that("#34 the secondary axis maps the full range, also for zero and negative values", {
  sec_labels <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.sec$get_labels()
  d <- data.frame(x = c("a", "b", "c"), y = c(10, 20, 30), z = c(0, 0, 0))
  expect_plot2(plot2(d, x, y, y_secondary = z))
  d$z <- c(-5, 5, 15)
  p <- expect_plot2(plot2(d, x, y, y_secondary = z))
  # the secondary values span the primary range: -5 at the lowest, 15 at the highest break
  expect_identical(sec_labels(p)[c(1, 4)], c("-5.00", "15.00"))
  expect_equal(range(layer_df(p, "line")$y), c(0, 30))
})

test_that("#60 x.complete works for character and date-time values", {
  p <- expect_plot2(plot2(data.frame(g = c("a", "c", "a"), v = 1:3, k = c("u", "w", "w")), g, v, k, x.complete = TRUE))
  expect_identical(nrow(plot_data(p)), 4L)
  t <- as.POSIXct(c("2024-01-01 00:00", "2024-01-01 12:00", "2024-01-01 18:00"), tz = "UTC")
  p <- expect_plot2(plot2(data.frame(t = t, v = 1:3), t, v, x.complete = TRUE))
  expect_identical(nrow(plot_data(p)), 4L)
  p <- expect_plot2(plot2(data.frame(x = c(1, 2, 5), y = 1:3), x, y, x.complete = TRUE))
  expect_identical(nrow(plot_data(p)), 5L)
})

test_that("#63 x.label_angle and x.label_align apply to the category labels of horizontal plots", {
  d <- data.frame(g = c("a", "b"), v = 1:2)
  # the angle is set by guide_axis() on the left axis after coord_flip() (#154)
  p <- expect_plot2(plot2(d, g, v, horizontal = TRUE, x.label_angle = 45))
  expect_identical(axis_guide_angle(p, "y"), 45)
  expect_null(axis_guide_angle(p, "x"))
  p <- plot2(d, g, v, horizontal = TRUE, x.label_align = 0)
  expect_identical(theme_el(p, "axis.text.y")$hjust, 0)
  p <- plot2(d, g, v, x.label_angle = 45)
  expect_identical(axis_guide_angle(p, "x"), 45)
  # a user-set alignment uses the theme
  p <- plot2(d, g, v, horizontal = TRUE, x.label_angle = 45, x.label_align = 0)
  expect_identical(theme_el(p, "axis.text.y")$angle, 45)
})

test_that("#64 the data-dependent choice of facet scales is reported", {
  expect_plot2_message(plot2(admitted_patients, hospital, n(), facet = ward), "Using free y scales per facet", type = "info")
  d <- data.frame(x = rep(c("a", "b"), 2), f = rep(c("p", "q"), each = 2), y = c(10, 11, 10, 12))
  expect_plot2_message(plot2(d, x, y, facet = f), "facet.fixed_y = TRUE", type = "info")
})

test_that("#66 x.limits on a discrete x selects and orders the items, also horizontally", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, x.limits = c("c", "a")))
  expect_identical(axis_labels(p, "x"), c("c", "a"))
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, x.limits = c("a", "b"), horizontal = TRUE))
  expect_setequal(axis_labels(p, "y"), c("a", "b"))
  expect_plot2_message(plot2(fx_cat_unique, x, y, category, x.limits = c("a", "z")), "not found", type = "warning")
})

test_that("#71 numeric category limits are the data range rounded to pretty values; diverging scales are symmetric", {
  lim <- function(...) scale_of(plot2(...), "colour")$get_limits()
  d <- function(v) data.frame(x = seq_along(v), y = 1, c = v)
  expect_equal(lim(d(c(12, 50, 87)), x, y, c, type = "point"), c(0, 100))
  expect_equal(lim(d(c(-0.37, 0.2, 0.5)), x, y, c, type = "point"), c(-0.4, 0.6))
  expect_equal(lim(d(c(-0.37, 0.2, 0.5)), x, y, c, type = "point",
                   colour = c("blue", "white", "red"), category.midpoint = 0), c(-0.6, 0.6))
})

test_that("#80 the secondary series of a column plot is a line with points in the secondary colour", {
  df <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20))
  p <- expect_plot2(plot2(df, x, y, y_secondary = z))
  expect_geoms(p, c("bar", "line", "point"))
  # one connected line over the discrete x axis, scaled onto the primary axis
  expect_identical(length(unique(layer_df(p, "line")$group)), 1L)
  expect_equal(max(layer_df(p, "line")$y), 3)
  expect_false(identical(unique(layer_df(p, "line")$colour), unique(layer_df(p, "bar")$fill)))
  # with a category: one line per category, without warnings
  d2 <- data.frame(x = rep(c("a", "b", "c"), 2), c = rep(c("p", "q"), each = 3), y = 1:6, z = c(10, 30, 20, 5, 8, 9))
  p <- expect_plot2(plot2(d2, x, y, c, y_secondary = z))
  expect_identical(length(unique(layer_df(p, "line")$group)), 2L)
  # other primary types keep their type
  expect_geoms(plot2(df, x, y, y_secondary = z, type = "point"), c("point", "point"), exact = TRUE)
})

test_that("#142 limits must be of length 2", {
  d <- data.frame(x = c("a", "b", "c"), y = c(10, 50, 100))
  expect_error(plot2(d, x, y, y.limits = 50), "must be of length 2")
  expect_error(plot2(data.frame(x = 1:3, y = 1:3), x, y, x.limits = 2), "must be of length 2")
  expect_error(plot2(data.frame(x = 1:3, y = 1, c = c(1, 5, 9)), x, y, c, category.limits = 5), "must be of length 2")
  expect_plot2(plot2(d, x, y, y.limits = c(0, NA)))
})

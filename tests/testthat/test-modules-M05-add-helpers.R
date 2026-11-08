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

# R04 sheet M05: add_type() and the add_*() helpers
# review_and_overhaul/reviews/R04_modules/M05_add-helpers.md

fx_pts <- function() plot2(fx_xy, x, y, type = "point")
last_layer_df <- function(p) layer_df(p, length(geoms(p)))

test_that("M05.01 add_line(y = 3): a horizontal line", {
  p <- expect_plot2(add_line(fx_pts(), y = 3))
  expect_identical(geoms(p), c("GeomPoint", "GeomHline"))
  expect_identical(layer_df(p, "hline")$yintercept, 3)
})

test_that("M05.02 add_line(x = 2): a vertical line", {
  expect_identical(layer_df(add_line(fx_pts(), x = 2), "vline")$xintercept, 2)
})

test_that("M05.03 add_line(y = mean(y)) per category and per facet", {
  p <- add_line(plot2(fx_cat_unique, x, y, category, type = "point"), y = mean(y))
  expect_setequal(layer_df(p, "hline")$yintercept, c(20, 25))
  p <- add_line(plot2(fx_facet, x, y, facet = facet, type = "point"), y = mean(y))
  expect_setequal(layer_df(p, "hline")$yintercept, c(2, 200))
})

test_that("M05.04 add_line() without values: a line through the data", {
  expect_identical(geoms(add_line(fx_pts())), c("GeomPoint", "GeomLine"))
})

test_that("M05.05 add_line() colour and legend entry", {
  p <- expect_plot2(add_line(fx_pts(), y = 3, colour = "red", legend.value = "Target"))
  expect_identical(unique(col_hex(as.character(layer_df(p, "hline")$colour))), "#FF0000")
  expect_identical(as.character(scale_breaks(p, "linetype")), "Target")
})

test_that("M05.06 add_line() on a plain ggplot2 plot", {
  q <- ggplot2::ggplot(fx_xy, ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_identical(layer_df(add_line(q, y = 3), "hline")$yintercept, 3)
})

test_that("M05.07 add_point(x, y): one new point", {
  d <- last_layer_df(add_point(fx_pts(), x = 3, y = 5))
  expect_identical(c(d$x, d$y), c(3, 5))
})

test_that("M05.08 add_col() adds a column layer", {
  expect_true("GeomCol" %in% geoms(add_col(plot2(fx_counts, group, n), y = n / 2)))
})

test_that("M05.09 add_errorbar() with a category: one bar per row", {
  p <- expect_plot2(add_errorbar(plot2(fx_cat_unique, x, y, category), min = y - 1, max = y + 1))
  expect_identical(nrow(layer_df(p, "errorbar")), 6L)
})

test_that("M05.10 add_errorbar() without a category (F082)", {
  d <- data.frame(g = c("a", "b"), n = c(3, 4))
  p <- expect_plot2(add_errorbar(plot2(d, g, n), min = n - 1, max = n + 1))
  expect_identical(layer_df(p, "errorbar")$ymin, c(2, 3))
})

test_that("M05.11 add_smooth() adds a smooth layer", {
  df <- data.frame(x = 1:30, y = sin(1:30 / 5) + 1:30 / 10)
  expect_identical(geoms(expect_plot2(add_smooth(plot2(df, x, y, type = "point")))), c("GeomPoint", "GeomSmooth"))
})

test_that("M05.12 add_smooth(legend.value) on a discrete x-axis (F085)", {
  d <- data.frame(g = sprintf("g%02d", 1:12), n = c(3, 5, 4, 6, 8, 7, 9, 11, 10, 12, 14, 13))
  p <- expect_plot2(add_smooth(plot2(d, g, n, type = "point"), legend.value = "Trend"))
  expect_true("GeomSmooth" %in% geoms(p))
  # also on a date axis
  d$g <- as.Date("2026-01-01") + 0:11
  expect_plot2(add_smooth(plot2(d, g, n, type = "point"), legend.value = "Trend"))
})

test_that("M05.13 add_type(): geom by name, move, invalid and missing type", {
  expect_identical(geoms(add_type(fx_pts(), "line")), c("GeomPoint", "GeomLine"))
  expect_identical(geoms(add_type(fx_pts(), "line", move = -1)), c("GeomLine", "GeomPoint"))
  expect_error(add_type(fx_pts(), "nonsense"), "invalid")
  expect_error(add_type(fx_pts()), "must be set")
  expect_error(add_type(1, "line"), "must be a ggplot2 object")
})

test_that("M05.14 add_*() on an empty plot returns it unchanged", {
  p <- plot2(fx_xy[0, ], x, y)
  expect_length(geoms(add_line(p, y = 3)), 0)
  expect_length(geoms(add_point(p, x = 1, y = 1)), 0)
})

test_that("M05.15 add_sf() adds an sf layer", {
  skip_if_not_installed("sf")
  p <- expect_plot2(add_sf(plot2(netherlands, datalabels = FALSE), netherlands[1:2, ]))
  expect_identical(geoms(p), c("GeomSf", "GeomSf"))
})

test_that("M05.16 add_sf(): datalabels.colour and other geometry names (F084)", {
  skip_if_not_installed("sf")
  skip_open_finding("F084")
  q <- add_sf(plot2(netherlands), netherlands[1:2, ], datalabels = province, datalabels.colour = "red", datalabels.nudge_y = 0)
  expect_identical(unique(col_hex(as.character(last_layer_df(q)$colour))), "#FF0000")
  nl2 <- sf::st_set_geometry(netherlands, "geom")
  expect_plot2(add_sf(plot2(nl2), netherlands[1:2, ]))
})

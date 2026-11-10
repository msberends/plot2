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

# E13: additional layers for linedot, dumbbell and spider

fx_dumbbell <- data.frame(x = rep(c("a", "b", "c"), 2),
                          y = c(1, 4, 2, 3, 6, 5),
                          when = rep(c("before", "after"), each = 3))

test_that("E13.01 linedot: large outer points in the background colour, smaller inner points", {
  p <- expect_plot2(plot2(fx_xy, x, y, type = "linedot"))
  outer <- layer_df(p, 2)
  inner <- layer_df(p, 3)
  expect_gt(unique(outer$size), unique(inner$size))
  expect_identical(unique(col_hex(as.character(outer$colour))), "#FFFFFF")
})

test_that("E13.02 linedot: a custom background colour is used for the outer points", {
  p <- plot2(fx_xy, x, y, type = "linedot", background = "yellow")
  expect_identical(unique(col_hex(as.character(layer_df(p, 2)$colour))), "#FFFF00")
})

test_that("E13.03 dumbbell: one grey segment per x from minimum to maximum, below the points", {
  p <- expect_plot2(plot2(fx_dumbbell, x, y, when, type = "dumbbell"))
  seg <- layer_df(p, "segment")
  expect_identical(nrow(seg), 3L)
  expect_lt(layer_index(p, "segment"), layer_index(p, "point"))
  # coord_flip(): x and y as in the data
  expect_setequal(paste(pmin(seg$y, seg$yend), pmax(seg$y, seg$yend)), c("1 3", "4 6", "2 5"))
})

test_that("E13.04 dumbbell with facets: segments per facet", {
  df <- rbind(transform(fx_dumbbell, f = "F1"), transform(fx_dumbbell, y = y + 10, f = "F2"))
  p <- expect_plot2(plot2(df, x, y, when, facet = f, type = "dumbbell"))
  expect_identical(nrow(layer_df(p, "segment")), 6L)
})

test_that("E13.05 dumbbell with three categories errors", {
  df <- rbind(fx_dumbbell, data.frame(x = "a", y = 1, when = "later"))
  expect_error(plot2(df, x, y, when, type = "dumbbell"), "two distinct values.*three")
})

test_that("E13.06 spider plots have no axis titles (removed by the theme)", {
  sp <- data.frame(dim = c("a", "b", "c", "d"), score = c(3, 5, 2, 4))
  p <- plot2(sp, dim, score, type = "spider")
  expect_s3_class(theme_el(p, "axis.title.x"), "ggplot2::element_blank")
  expect_s3_class(theme_el(p, "axis.title.y"), "ggplot2::element_blank")
})

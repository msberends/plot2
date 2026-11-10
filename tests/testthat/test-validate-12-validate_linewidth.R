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

# 12: validate_linewidth() (#157)

test_that("12.01 lines 0.5, areas 0.25, maps 0.1 by default", {
  expect_identical(unique(layer_df(plot2(fx_xy, x, y, type = "line"), "line")$linewidth), 0.5)
  expect_identical(unique(layer_df(plot2(fx_dates, date, y, type = "area"), "area")$linewidth), 0.25)
  skip_if_not_installed("sf")
  p <- plot2(netherlands)
  expect_identical(unique(layer_df(p, layer_index(p, "GeomSf"))$linewidth), 0.1)
})

test_that("12.02 dumbbell segments and spider polygons have linewidth 1", {
  p <- plot2(admitted_patients, age_group, n(), ward, type = "dumbbell")
  expect_identical(unique(layer_df(p, layer_index(p, "GeomSegment"))$linewidth), 1)
  expect_identical(plot2:::validate_linewidth(NULL, type = "geom_polygon", type_backup = "spider"), 1)
})

test_that("12.03 a set linewidth is used", {
  expect_identical(unique(layer_df(plot2(fx_xy, x, y, type = "line", linewidth = 2), "line")$linewidth), 2)
  expect_error(plot2(fx_xy, x, y, type = "line", linewidth = "thick"), "`linewidth` must be a non-negative number", fixed = TRUE)
})

test_that("12.04 the boxplot median is thicker than the box outline", {
  p <- plot2(iris, Species, Sepal.Length, type = "boxplot")
  box <- p$layers[[layer_index(p, "GeomBoxplot")]]
  expect_gt(box$geom_params$median_gp$linewidth, unique(layer_df(p, layer_index(p, "GeomBoxplot"))$linewidth))
})

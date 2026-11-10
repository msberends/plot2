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

# validate_size() (#157)

test_that("points have size 2 by default", {
  expect_identical(unique(layer_df(plot2(fx_xy, x, y), "point")$size), 2)
})

test_that("dumbbell dots have size 5, UpSet dots size 4", {
  p <- plot2(admitted_patients, age_group, n(), ward, type = "dumbbell")
  expect_identical(unique(layer_df(p, layer_index(p, "GeomPoint"))$size), 5)
  expect_identical(plot2:::validate_size(NULL, type = "upset", type_backup = "upset"), 4)
})

test_that("a set size is used", {
  expect_identical(unique(layer_df(plot2(fx_xy, x, y, size = 4), "point")$size), 4)
})

test_that("size on a line plot warns and is used as linewidth", {
  expect_warning(p <- plot2(fx_xy, x, y, type = "line", size = 3), "replaced with 'linewidth'")
  expect_identical(unique(layer_df(p, "line")$linewidth), 3)
})

test_that("a negative or character size gives a clear error (#150)", {
  expect_error(plot2(fx_xy, x, y, size = -1), "`size` must be a non-negative number", fixed = TRUE)
  expect_error(plot2(fx_xy, x, y, size = "a"), "`size` must be a non-negative number", fixed = TRUE)
})

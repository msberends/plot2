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

# R04 sheet M06: move_layer() and add_mapping()
# review_and_overhaul/reviews/R04_modules/M06_move-layer-add-mapping.md

fx_two_layers <- function() {
  ggplot2::ggplot(fx_xy, ggplot2::aes(x, y)) + ggplot2::geom_point() + ggplot2::geom_line()
}

test_that("M06.01 default: the last layer moves one position back", {
  expect_identical(geoms(move_layer(fx_two_layers())), c("GeomLine", "GeomPoint"))
})

test_that("M06.02 a large negative move is clamped to the first position", {
  p <- fx_two_layers() + ggplot2::geom_rug()
  expect_identical(geoms(move_layer(p, move = -10)), c("GeomRug", "GeomPoint", "GeomLine"))
})

test_that("M06.03 a non-existing layer errors", {
  expect_error(move_layer(fx_two_layers(), layer = 5), "does not exist")
})

test_that("M06.04 a positive move keeps all layers (F083)", {
  expect_identical(geoms(move_layer(fx_two_layers(), layer = 1, move = 1)), c("GeomLine", "GeomPoint"))
})

test_that("M06.05 add_mapping() adds an aesthetic and keeps the others", {
  p <- add_mapping(fx_two_layers(), colour = y)
  expect_identical(sort(names(p$mapping)), c("colour", "x", "y"))
})

test_that("M06.06 both reject non-ggplot input", {
  expect_error(move_layer(1), "must be a ggplot2 object")
  expect_error(add_mapping(1, colour = y), "must be a ggplot2 object")
})

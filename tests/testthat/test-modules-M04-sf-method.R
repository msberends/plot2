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

# R04 sheet M04: plot2.sf()
# review_and_overhaul/reviews/R04_modules/M04_sf-method.md

test_that("M04.01 plot2(netherlands): sf layer plus labels", {
  skip_if_not_installed("sf")
  p <- expect_plot2(plot2(netherlands))
  expect_identical(geoms(p)[1], "GeomSf")
})

test_that("M04.02 category maps the fill", {
  skip_if_not_installed("sf")
  p <- expect_plot2(plot2(netherlands, category = area_km2))
  expect_false(scale_of(p, "fill")$is_discrete())
})

test_that("M04.03 datalabels = FALSE removes the labels", {
  skip_if_not_installed("sf")
  expect_geoms(plot2(netherlands, datalabels = FALSE), "sf", exact = TRUE)
})

test_that("M04.04 crs transforms the data", {
  skip_if_not_installed("sf")
  p <- plot2(netherlands, crs = 28992)
  expect_identical(sf::st_crs(p$data)$epsg, 28992L)
})

test_that("M04.05 colour without colour_fill: caution about borders", {
  skip_if_not_installed("sf")
  expect_plot2_message(plot2(netherlands, colour = "red"), "will set the borders", type = "caution")
})

test_that("M04.06 x given: caution instead of an error (F080)", {
  skip_if_not_installed("sf")
  skip_open_finding("F080")
  expect_plot2_message(plot2(netherlands, x = province), "will be ignored", type = "caution")
})

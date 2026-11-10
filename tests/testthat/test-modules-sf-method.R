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

# plot2.sf()

test_that("plot2(netherlands): sf layer plus labels", {
  skip_if_not_installed("sf")
  p <- expect_plot2(plot2(netherlands))
  expect_identical(geoms(p)[1], "GeomSf")
})

test_that("category maps the fill", {
  skip_if_not_installed("sf")
  p <- expect_plot2(plot2(netherlands, category = area_km2))
  expect_false(scale_of(p, "fill")$is_discrete())
})

test_that("datalabels = FALSE removes the labels", {
  skip_if_not_installed("sf")
  expect_geoms(plot2(netherlands, datalabels = FALSE), "sf", exact = TRUE)
})

test_that("crs transforms the data", {
  skip_if_not_installed("sf")
  p <- plot2(netherlands, crs = 28992)
  expect_identical(sf::st_crs(p$data)$epsg, 28992L)
})

test_that("colour without colour_fill: caution about borders", {
  skip_if_not_installed("sf")
  expect_plot2_message(plot2(netherlands, colour = "red"), "will set the borders", type = "caution")
})

test_that("x given: caution instead of an error (#92)", {
  skip_if_not_installed("sf")
  expect_plot2_message(plot2(netherlands, x = province), "will be ignored", type = "caution")
})

test_that("colour_fill sets the colours of a discrete category", {
  skip_if_not_installed("sf")
  p <- expect_plot2(plot2(netherlands,
                          category = ifelse(area_km2 > 3000, "large", "small"),
                          colour_fill = c(large = "#4E79A7", small = "#E8E8E8")))
  expect_identical(col_hex(scale_values(p, "fill")), c(large = "#4E79A7", small = "#E8E8E8"))
})

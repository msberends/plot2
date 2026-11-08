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

# R03 sheet E01: generic plot2(), dispatch and empty data
# review_and_overhaul/reviews/R03_plot2_exec/E01_dispatch-and-empty-data.md

test_that("E01.01 data.frame input dispatches; grouped data give the same result", {
  p <- expect_plot2(plot2(fx_cat, x, y))
  g <- plot2(dplyr::group_by(fx_cat, category), x, y)
  expect_identical(layer_df(g, 1)$y, layer_df(p, 1)$y)
})

test_that("E01.02 tibble input gives the same layer data as data.frame input", {
  expect_identical(layer_df(plot2(dplyr::as_tibble(fx_cat), x, y), 1)$y,
                   layer_df(plot2(fx_cat, x, y), 1)$y)
})

test_that("E01.03 empty data: empty plot with the given title and a plot2 warning", {
  p <- expect_plot2_message(plot2(fx_xy[0, ], x, y, title = "Empty"), "No observations", type = "warning")
  expect_length(geoms(p), 0)
  expect_labels(p, title = "Empty")
})

test_that("E01.04 empty data: axis titles only when given", {
  p <- plot2(fx_xy[0, ], x, y, x.title = "X axis", y.title = "Y axis")
  expect_labels(p, x = "X axis", y = "Y axis")
})

test_that("E01.05 empty data with print = TRUE returns the plot invisibly (F010)", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_s3_class(plot2(fx_xy[0, ], x, y, print = TRUE), "gg")
})

test_that("E01.06 data.frame with an sf geometry column is plotted as a map", {
  skip_if_not_installed("sf")
  p <- expect_plot2(plot2(as.data.frame(netherlands)))
  expect_geoms(p, "sf")
})

test_that("E01.07 an atomic character vector gives counted columns", {
  v <- c("a", "b", "b", "c", "c", "c")
  p <- expect_plot2(plot2(v))
  expect_identical(sort(layer_df(p, 1)$y), c(1, 2, 3))
})

test_that("E01.07b a character vector given as a long expression (F072)", {
  expect_plot2(plot2(c("a", "b", "b", "c", "c", "c")))
})

test_that("E01.08 empty data leaves no internal state behind (F002)", {
  expect_clean_state(plot2(fx_xy[0, ], x, y))
})

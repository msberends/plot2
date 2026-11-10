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

# Generic plot2(), dispatch and empty data

test_that("data.frame input dispatches; grouped data give the same result", {
  p <- expect_plot2(plot2(fx_cat, x, y))
  g <- plot2(dplyr::group_by(fx_cat, category), x, y)
  expect_identical(layer_df(g, 1)$y, layer_df(p, 1)$y)
})

test_that("tibble input gives the same layer data as data.frame input", {
  expect_identical(layer_df(plot2(dplyr::as_tibble(fx_cat), x, y), 1)$y,
                   layer_df(plot2(fx_cat, x, y), 1)$y)
})

test_that("empty data: empty plot with the given title and a plot2 warning", {
  p <- expect_plot2_message(plot2(fx_xy[0, ], x, y, title = "Empty"), "No observations", type = "warning")
  expect_length(geoms(p), 0)
  expect_labels(p, title = "Empty")
})

test_that("empty data: axis titles only when given", {
  expect_warning(p <- plot2(fx_xy[0, ], x, y, x.title = "X axis", y.title = "Y axis"), "No observations", class = "plot2_warning")
  expect_labels(p, x = "X axis", y = "Y axis")
})

test_that("empty data with print = TRUE returns the plot invisibly (#22)", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_warning(p <- plot2(fx_xy[0, ], x, y, print = TRUE), "No observations", class = "plot2_warning")
  expect_s3_class(p, "gg")
})

test_that("data.frame with an sf geometry column is plotted as a map", {
  skip_if_not_installed("sf")
  p <- expect_plot2(plot2(as.data.frame(netherlands)))
  expect_geoms(p, "sf")
})

test_that("an atomic character vector gives counted columns", {
  v <- c("a", "b", "b", "c", "c", "c")
  p <- expect_plot2(plot2(v))
  expect_identical(sort(layer_df(p, 1)$y), c(1, 2, 3))
})

test_that("a character vector given as a long expression (#84)", {
  expect_plot2(plot2(c("a", "b", "b", "c", "c", "c")))
})

test_that("empty data leaves no internal state behind (#14)", {
  expect_clean_state(suppressWarnings(plot2(fx_xy[0, ], x, y)))
})

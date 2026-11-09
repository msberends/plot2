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

# E15: Sankey plots

fx_sankey <- data.frame(from = c("A", "A", "B", "B"),
                        to = c("X", "Y", "X", "Y"),
                        n = c(5, 3, 2, 6),
                        grp = c("p", "q", "p", "q"))

test_that("E15.01 layers: flows, nodes and labels", {
  skip_if_not_installed("ggforce")
  p <- expect_plot2(plot2(fx_sankey, x = c(from, to), y = n, category = grp, type = "sankey"))
  expect_geoms(p, c("GeomShape", "GeomParallelSetsAxes", "GeomText"))
})

test_that("E15.02 the x-axis shows the variable names in the order given", {
  skip_if_not_installed("ggforce")
  expect_identical(axis_labels(plot2(fx_sankey, x = c(from, to), y = n, category = grp, type = "sankey"), "x"), c("from", "to"))
  expect_identical(axis_labels(plot2(fx_sankey, x = c(to, from), y = n, category = grp, type = "sankey"), "x"), c("to", "from"))
})

test_that("E15.03 a missing category errors with a clear message", {
  skip_if_not_installed("ggforce")
  expect_error(plot2(fx_sankey, x = c(from, to), y = n, type = "sankey"), "category")
})

test_that("E15.04 axes are removed by default, with a message", {
  skip_if_not_installed("ggforce")
  expect_plot2_message(plot2(fx_sankey, x = c(from, to), y = n, category = grp, type = "sankey"), "sankey_remove_axes")
})

test_that("E15.05 facets are supported", {
  skip_if_not_installed("ggforce")
  p <- expect_plot2(plot2(fx_sankey, x = c(from, to), y = n, category = grp, facet = grp, type = "sankey"))
  expect_facets(p, n_panels = 2L)
})

test_that("E15.06 horizontal = TRUE flips the coordinates", {
  skip_if_not_installed("ggforce")
  expect_identical(coord_class(plot2(fx_sankey, x = c(from, to), y = n, category = grp, type = "sankey", horizontal = TRUE)), "CoordFlip")
})

test_that("E15.07 sankey_alpha reaches the flow layer", {
  skip_if_not_installed("ggforce")
  p <- plot2(fx_sankey, x = c(from, to), y = n, category = grp, type = "sankey", sankey_alpha = 0.3)
  expect_identical(unique(layer_df(p, "GeomShape")$alpha), 0.3)
})

test_that("E15.08 a single x variable gives the friendly message (#83)", {
  expect_error(plot2(fx_sankey, x = from, y = n, category = grp, type = "sankey"), "at least two")
})

test_that("E15.09 no helper columns in p$data (#81)", {
  skip_if_not_installed("ggforce")
  p <- plot2(fx_sankey, x = c(from, to), y = n, category = grp, type = "sankey")
  expect_false(any(startsWith(colnames(p$data), "_sankey")))
})

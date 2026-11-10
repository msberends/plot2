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

# facets, datalabels and coordinates

test_that("one panel per facet value; facet keeps its original name", {
  p <- expect_plot2(plot2(fx_facet, x, y, facet = facet))
  expect_facets(p, n_panels = 2L)
  expect_identical(names(plot_of(p)$facet$params$facets), "facet")
})

test_that("columns get label and text datalabel layers with the values", {
  p <- expect_plot2(plot2(fx_counts, group, n))
  expect_geoms(p, c("label", "text"))
  expect_setequal(datalabels_of(p), c("12", "7", "30", "1"))
})

test_that("horizontal plots use coord_flip(); clipping off with a title", {
  p <- plot2(fx_counts, group, n, horizontal = TRUE, title = "T")
  expect_identical(coord_class(p), "CoordFlip")
  expect_identical(plot_of(p)$coordinates$clip, "off")
})

test_that("vertical plot with datalabels and a title: clipping off", {
  p <- plot2(fx_counts, group, n, title = "T")
  expect_identical(coord_class(p), "CoordCartesian")
  expect_identical(plot_of(p)$coordinates$clip, "off")
})

test_that("vertical plot without title or facets: default coordinates", {
  p <- plot2(fx_counts, group, n)
  expect_identical(coord_class(p), "CoordCartesian")
  expect_identical(plot_of(p)$coordinates$clip, "on")
})

test_that("type = 'blank' has no datalabels", {
  expect_false("GeomText" %in% geoms(plot2(fx_counts, group, n, type = "blank")))
})

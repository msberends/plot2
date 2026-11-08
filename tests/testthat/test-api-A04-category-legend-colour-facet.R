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

# R05 sheet A04: plot2() arguments for category, legend, colours and facets
# review_and_overhaul/reviews/R05_api/A04_category-legend-colour-facet.md

bar_fill <- function(p) layer_df(p, layer_index(p, "GeomBar"))$fill

test_that("A04.01 legend.position defaults to 'top' for discrete and 'right' for numeric categories and sf", {
  expect_identical(legend_position(plot2(admitted_patients, hospital, n(), gender)), "top")
  expect_identical(legend_position(plot2(iris, Sepal.Length, Sepal.Width, Petal.Length)), "right")
  skip_if_not_installed("sf")
  expect_identical(legend_position(plot2(netherlands)), "right")
})

test_that("A04.02 legend.position = NA hides the legend, abbreviations are accepted", {
  expect_identical(legend_position(plot2(admitted_patients, hospital, n(), gender, legend.position = NA)), "none")
  expect_identical(legend_position(plot2(admitted_patients, hospital, n(), gender, legend.position = "b")), "bottom")
})

test_that("A04.03 legend.position = NULL means the automatic default (documentation says 'none')", {
  expect_identical(legend_position(plot2(admitted_patients, hospital, n(), gender, legend.position = NULL)), "top")
})

test_that("A04.04 colour_opacity applies with an explicit colour", {
  expect_identical(unname(as.character(bar_fill(plot2(fx_counts, group, n, colour = "red", colour_opacity = 0.5))[1])), "#FF000080")
})

test_that("A04.05 colour_opacity applies with the default colours", {
  skip_open_finding("F096")
  expect_true(all(substr(bar_fill(plot2(fx_counts, group, n, colour_opacity = 0.5)), 8, 9) == "80"))
  expect_true(all(substr(bar_fill(plot2(fx_cat, x, y, category, colour_opacity = 0.5)), 8, 9) == "80"))
})

test_that("A04.06 category.focus accepts a value and a position", {
  a <- unique(bar_fill(plot2(fx_cat, x, y, category, category.focus = "v", colour = c("red", "blue"))))
  b <- unique(bar_fill(plot2(fx_cat, x, y, category, category.focus = 2, colour = c("red", "blue"))))
  expect_identical(length(a), 2L)
  expect_identical(a, b)
})

test_that("A04.07 category.sort 'alpha-desc' (used in the examples) sorts descending", {
  p <- plot2(fx_cat, x, y, category, category.sort = "alpha-desc")
  expect_identical(as.character(scale_breaks(p, "fill")), c("v", "u"))
})

test_that("A04.08 a named colour vector matches category values", {
  p <- plot2(fx_cat, x, y, category, colour = c(v = "red", u = "blue"))
  d <- layer_df(p, layer_index(p, "GeomBar"))
  expect_identical(unname(as.character(unique(d$fill[d$group == 1]))), "#0000FF")
})

test_that("A04.09 facet.nrow sets the number of facet rows", {
  expect_facets(plot2(admitted_patients, hospital, n(), facet = age_group, facet.nrow = 1), nrow = 1L)
})

test_that("A04.10 reverse reverses the category values, legend.reverse only the legend", {
  a <- plot2(fx_cat, x, y, category, stacked = TRUE)
  b <- plot2(fx_cat, x, y, category, stacked = TRUE, reverse = TRUE)
  first <- function(p) { d <- layer_df(p, layer_index(p, "GeomBar")); d$fill[d$x == 1 & d$ymin == 0] }
  expect_false(identical(first(a), first(b)))
})

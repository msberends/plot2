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

# plot2.formula()

fx_mtcars <- transform(mtcars, cyl = as.character(cyl), am = as.character(am))

test_that("y ~ x with data: points, titles from the formula", {
  p <- expect_plot2(plot2(mpg ~ wt, data = mtcars))
  expect_geoms(p, "point")
  expect_labels(p, x = "wt", y = "mpg")
})

test_that("the third term is the category", {
  p <- expect_plot2(plot2(mpg ~ wt + cyl, data = fx_mtcars))
  expect_identical(as.character(scale_breaks(p, "colour")), c("4", "6", "8"))
})

test_that("the fourth term is the facet", {
  p <- expect_plot2(plot2(mpg ~ wt + cyl + am, data = fx_mtcars))
  expect_facets(p, n_panels = 2L)
})

test_that("missing data errors clearly", {
  expect_error(plot2(mpg ~ wt), "requires the `data` argument")
})

test_that("a formula after a pipe (#93)", {
  expect_geoms(expect_plot2(mtcars |> plot2(mpg ~ wt)), "point")
})

test_that("computed terms (#31)", {
  p <- expect_plot2(plot2(log(mpg) ~ wt, data = mtcars))
  expect_equal(sort(layer_df(p, "point")$y), sort(log(mtcars$mpg)))
})

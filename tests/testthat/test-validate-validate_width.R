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

# validate_width() (#157)

bar_width <- function(p, geom) {
  l <- layer_df(p, layer_index(p, geom))
  unique(round(l$xmax - l$xmin, 3))
}

test_that("columns have width 0.5", {
  expect_identical(bar_width(plot2(fx_counts, group, n), "GeomBar"), 0.5)
})

test_that("boxplots have width 0.66", {
  expect_identical(bar_width(plot2(iris, Species, Sepal.Length, type = "boxplot"), "GeomBoxplot"), 0.66)
})

test_that("a set width is used, and the whiskers follow", {
  p <- plot2(iris, Species, Sepal.Length, type = "boxplot", width = 0.9)
  expect_identical(bar_width(p, "GeomBoxplot"), 0.9)
  expect_identical(bar_width(p, "GeomErrorbar"), round(0.9 * 0.75, 3))
  expect_error(plot2(fx_counts, group, n, width = -1), "`width` must be a non-negative number", fixed = TRUE)
})

test_that("with categories, the datalabels are dodged like the bars", {
  p <- plot2(fx_cat_unique, x, y, category)
  expect_equal(sort(layer_df(p, "text")$x), sort(layer_df(p, "bar")$x))
})

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

# I002, R02 sheet 11: validate_width()
# review_and_overhaul/reviews/R02_validate/11_validate_width.md

bar_width <- function(p, geom) {
  l <- layer_df(p, layer_index(p, geom))
  unique(round(l$xmax - l$xmin, 3))
}

test_that("11.01 columns have width 0.5", {
  expect_identical(bar_width(plot2(fx_counts, group, n), "GeomBar"), 0.5)
})

test_that("11.02 boxplots have width 0.66", {
  expect_identical(bar_width(plot2(iris, Species, Sepal.Length, type = "boxplot"), "GeomBoxplot"), 0.66)
})

test_that("11.03 a set width is used, and the whiskers follow", {
  p <- plot2(iris, Species, Sepal.Length, type = "boxplot", width = 0.9)
  expect_identical(bar_width(p, "GeomBoxplot"), 0.9)
  expect_identical(bar_width(p, "GeomErrorbar"), round(0.9 * 0.75, 3))
  expect_error(plot2(fx_counts, group, n, width = -1), "`width` must be a non-negative number", fixed = TRUE)
})

test_that("11.04 with categories, the datalabels are dodged like the bars", {
  p <- plot2(fx_cat_unique, x, y, category)
  expect_equal(sort(layer_df(p, "text")$x), sort(layer_df(p, "bar")$x))
})

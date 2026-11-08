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

# R03 sheet E09: mapping and geom generation
# review_and_overhaul/reviews/R03_plot2_exec/E09_mapping-and-geoms.md

fx_spider <- data.frame(dim = c("a", "b", "c", "d", "e"), score = c(3, 5, 2, 4, 1))

test_that("E09.01 mapping without category: x and y", {
  p <- plot2(fx_counts, group, n)
  expect_true(all(c("x", "y") %in% names(plot_of(p)$mapping)))
  expect_false(any(c("colour", "fill") %in% names(plot_of(p)$mapping)))
})

test_that("E09.02 mapping with category: colour, fill and group", {
  p <- plot2(fx_cat_unique, x, y, category)
  expect_true(all(c("colour", "fill", "group") %in% names(plot_of(p)$mapping)))
})

test_that("E09.03 category.type = 'shape' maps shape, not colour", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, type = "point", category.type = "shape"))
  m <- names(plot_of(p)$mapping)
  expect_true("shape" %in% m)
  expect_false("colour" %in% m)
})

test_that("E09.04 boxplot: whiskers drawn first, in the set colour", {
  p <- expect_plot2(plot2(iris, Species, Sepal.Length, colour = "red"))
  expect_identical(geoms(p)[1:2], c("GeomErrorbar", "GeomBoxplot"))
  expect_identical(unique(col_hex(as.character(layer_df(p, "errorbar")$colour))), "#FF0000")
})

test_that("E09.04b boxplots raise no deprecation warning (F078)", {
  expect_plot2(plot2(iris, Species, Sepal.Length))
})

test_that("E09.05 secondary y adds layers for the secondary series (a line with points for columns, F068)", {
  df <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20))
  n_primary <- length(geoms(plot2(df, x, y)))
  expect_identical(length(geoms(plot2(df, x, y, y_secondary = z))), n_primary + 2L)
  expect_identical(length(geoms(plot2(df, x, y, y_secondary = z, y_secondary.type = "col"))), n_primary + 1L)
})

test_that("E09.06 secondary series of a column plot is not drawn over the bars (F068)", {
  df <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20))
  expect_geoms(plot2(df, x, y, y_secondary = z), "line")
})

test_that("E09.07 spider: extra coord_spider() arguments are passed on", {
  p <- expect_plot2(plot2(fx_spider, dim, score, type = "spider", start = pi / 2))
  expect_equal(plot_of(p)$coordinates$start, pi / 2)
})

test_that("E09.08 spider: facets and fewer than three x values error", {
  df <- rbind(transform(fx_spider, f = "p"), transform(fx_spider, f = "q"))
  expect_error(plot2(df, dim, score, facet = f, type = "spider"))
  expect_error(plot2(fx_spider[1:2, ], dim, score, type = "spider"))
})

test_that("E09.09 smooth: drawn below the points, in smooth.colour", {
  df <- data.frame(x = 1:30, y = sin(1:30 / 5) + 1:30 / 10)
  p <- expect_plot2(plot2(df, x, y, smooth = TRUE, smooth.colour = "red"))
  expect_identical(geoms(p)[1], "GeomSmooth")
  expect_identical(unique(col_hex(as.character(layer_df(p, "smooth")$colour))), "#FF0000")
})

test_that("E09.10 histogram: a density curve is added by default", {
  p <- expect_plot2(plot2(data.frame(v = c(1, 2, 2, 3, 3, 3, 4, 4, 5)), type = "histogram"))
  expect_geoms(p, c("bar", "density"))
})

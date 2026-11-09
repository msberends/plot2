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

# A05: plot2() arguments for data handling, datalabels, geoms and appearance

d30 <- data.frame(x = paste0("v", sprintf("%02d", 1:30)), y = 1:30)

test_that("A05.01 datalabels left blank: shown for up to 25 x values, not for more", {
  expect_identical(length(datalabels_of(plot2(d30[1:20, ], x, y))), 20L)
  expect_identical(length(datalabels_of(plot2(d30, x, y))), 0L)
})

test_that("A05.02 datalabels = TRUE forces labels beyond 25", {
  expect_identical(length(datalabels_of(plot2(d30, x, y, datalabels = TRUE))), 30L)
})

test_that("A05.03 datalabels.format = NULL does not transform the labels", {
  d <- data.frame(x = c("a", "b"), y = c(1.234, 2.5))
  expect_identical(as.numeric(datalabels_of(plot2(d, x, y, datalabels.format = NULL))), c(1.234, 2.5))
})

test_that("A05.04 x.sort with a manual vector adds missing values at the end", {
  p <- plot2(admitted_patients, hospital, n(), x.sort = c("B", "D", "A"))
  expect_identical(as.character(axis_labels(p, "x")), c("B", "D", "A", "C"))
})

test_that("A05.05 x.max_items with x.sort = 'freq-desc' keeps the most frequent values", {
  d <- data.frame(x = c(rep("a", 1), rep("b", 5), rep("c", 3), rep("d", 4)))
  p <- plot2(d, x, n(), x.sort = "freq-desc", x.max_items = 3)
  expect_identical(as.character(axis_labels(p, "x"))[1:2], c("b", "d"))
  expect_length(axis_labels(p, "x"), 3)
})

test_that("A05.06 documented size, width and linewidth defaults", {
  p_point <- plot2(iris, Sepal.Length, Sepal.Width)
  expect_identical(unique(layer_df(p_point)$size), 2)
  p_box <- plot2(admitted_patients, hospital, age)
  b <- layer_df(p_box, layer_index(p_box, "GeomBoxplot"))
  expect_equal(unique(round(b$xmax - b$xmin, 2)), 0.66)
  p_col <- plot2(fx_counts, group, n)
  c1 <- layer_df(p_col, layer_index(p_col, "GeomBar"))
  expect_equal(unique(round(c1$xmax - c1$xmin, 2)), 0.5)
  expect_identical(unique(layer_df(plot2(mtcars, mpg, hp, type = "l"))$linewidth), 0.5)
  expect_identical(unique(layer_df(plot2(mtcars, mpg, hp, type = "a"))$linewidth), 0.25)
})

test_that("A05.07 a histogram has a density smooth by default", {
  expect_geoms(plot2(admitted_patients, age, type = "hist"), c("bar", "density"))
  expect_geoms(plot2(admitted_patients, age, type = "hist", smooth = FALSE), "bar", exact = TRUE)
})

test_that("A05.08 theme = NULL gives the ggplot2 default theme", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, theme = NULL)
  expect_identical(theme_el(p, "panel.background")$fill, ggplot2::theme_grey()$panel.background$fill)
})

test_that("A05.09 jitter_seed makes jitter reproducible", {
  a <- layer_df(plot2(iris, Species, Sepal.Length, type = "j", jitter_seed = 1))$x
  b <- layer_df(plot2(iris, Species, Sepal.Length, type = "j", jitter_seed = 1))$x
  expect_identical(a, b)
})

test_that("A05.10 na.replace sets the label of NA values", {
  d <- data.frame(x = c("a", NA), y = 1:2)
  expect_true("missing" %in% axis_labels(plot2(d, x, y, na.replace = "missing"), "x"))
})

test_that("A05.11 summarise_function = FALSE keeps the values, as documented", {
  expect_geoms(plot2(admitted_patients, hospital, age, type = "col", summarise_function = FALSE), "bar")
})

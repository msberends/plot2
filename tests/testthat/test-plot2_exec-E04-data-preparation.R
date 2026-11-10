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

# E04: data preparation (direction columns, y variants)

test_that("E04.01 y = c(a, b): one category per column, no y title", {
  p <- expect_plot2(plot2(fx_wide, group, c(first, second)))
  expect_identical(as.character(scale_breaks(p, "fill")), c("first", "second"))
  expect_identical(label_text(p, "y"), NA_character_)
})

test_that("E04.02 y = c(a, b) together with a category errors", {
  expect_error(plot2(fx_wide, group, c(first, second), category = third))
})

test_that("E04.03 y = where(is.numeric) selects all numeric columns", {
  p <- expect_plot2(plot2(fx_wide, group, where(is.numeric)))
  expect_identical(as.character(scale_breaks(p, "fill")), c("first", "second", "third"))
})

test_that("E04.04 y = n() counts rows per x", {
  p <- expect_plot2(plot2(fx_cat, x, n()))
  expect_identical(as.numeric(layer_df(p, "bar")$y), c(2, 2, 2))
})

test_that("E04.05 y = n_distinct(col) per x, and per x and category", {
  df <- data.frame(x = c("a", "a", "a", "b"), id = c(1, 1, 2, 3), g = c("u", "u", "v", "u"))
  p <- expect_plot2(plot2(df, x, n_distinct(id)))
  expect_identical(as.numeric(layer_df(p, "bar")$y), c(2, 1))
  p <- expect_plot2(plot2(df, x, n_distinct(id), g))
  expect_identical(nrow(plot_data(p)), 3L)
})

test_that("E04.06 y = range(y): generated categories, with a caution", {
  p <- expect_plot2_message(plot2(fx_cat, x, range(y)), "auto-generated", type = "caution")
  expect_identical(as.character(scale_breaks(p, "fill")), c("range (1)", "range (2)"))
})

test_that("E04.07 y = y * 2 is computed per row; y title is the expression", {
  p <- expect_plot2(plot2(fx_xy, x, y * 2, type = "point"))
  expect_identical(layer_df(p, "point")$y, fx_xy$y * 2)
  expect_identical(label_text(p, "y"), "y * 2")
})

test_that("E04.08 x = c(a, b) is pasted with sep", {
  p <- expect_plot2(plot2(fx_cat_unique, c(x, category), y, sep = "-"))
  expect_true("a-u" %in% axis_labels(p, "x"))
  p <- plot2(fx_cat_unique, c(x, category), y)
  expect_true("a / u" %in% axis_labels(p, "x"))
})

test_that("E04.09 facet = where(is.character) combines columns", {
  df <- data.frame(x = c(1.5, 2, 3, 4), y = c(1, 3, 2, 4), f1 = c("a", "a", "b", "b"), f2 = c("p", "q", "p", "q"))
  p <- expect_plot2(plot2(df, x, y, facet = where(is.character)))
  expect_facets(p, n_panels = 4L)
})

test_that("E04.10 numeric y values are not taken as column positions (#77)", {
  df <- data.frame(x = c("a", "b"), y = c(2, 3), z = c("p", "q"))
  p <- plot2(df, x, c(2, 3))
  expect_identical(as.numeric(layer_df(p, "bar")$y), c(2, 3))
})

test_that("E04.11 column names with regex characters in y = c(...) (#29)", {
  df <- data.frame(g = c("a", "b"), `a.b` = c(1, 2), `a+b` = c(3, 4), check.names = FALSE)
  p <- expect_plot2(plot2(df, g, c(`a.b`, `a+b`)))
  # both columns are found; category.sort = TRUE sorts them ("+" before ".")
  expect_identical(as.character(scale_breaks(p, "fill")), c("a+b", "a.b"))
})

test_that("E04.12 numeric x and y with identical values give points, not a histogram (#86)", {
  expect_geoms(plot2(data.frame(x = 1:4, y = 1:4), x, y), "point", exact = TRUE)
})

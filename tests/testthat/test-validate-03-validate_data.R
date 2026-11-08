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

# I002, R02 sheet 03: validate_data()
# review_and_overhaul/reviews/R02_validate/03_validate_data.md

test_that("03.01 without y, a column named n is used as y, with a message", {
  p <- expect_plot2_message(plot2(fx_counts), "Using y = n", type = "info")
  expect_labels(p, x = "group", y = "n")
  expect_identical(layer_df(p, "bar")$y, c(12, 7, 30, 1)[order(c("a", "b", "c", "d"))])
})

test_that("03.02 without x and y, two numeric columns give x = first and y = second", {
  p <- expect_plot2_message(plot2(fx_xy), "Using type = \"point\"", type = "info")
  expect_geoms(p, "point", exact = TRUE)
  expect_labels(p, x = "x", y = "y")
})

test_that("03.03 one numeric column and nothing else gives a histogram", {
  p <- expect_plot2_message(plot2(data.frame(v = c(1, 2, 2, 3, 5))), "type = \"histogram\"", type = "info")
  expect_geoms(p, "bar")
  expect_identical(layer_stats(p)[1], "StatBin")
})

test_that("03.04 x as the only numeric column with another type suggests a histogram", {
  expect_error(plot2(data.frame(v = c(1, 2, 3)), x = v, type = "col"), "histogram")
})

test_that("03.05 a difftime y is coerced to double, with a message", {
  d <- data.frame(g = c("a", "b"), d = as.difftime(c(1, 2), units = "hours"))
  p <- expect_plot2_message(plot2(d), "Coercing values of y from class difftime", type = "info")
  expect_identical(layer_df(p, "bar")$y, c(1, 2))
})

test_that("03.06 a POSIXct column is never chosen as automatic y", {
  d <- data.frame(t = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC"), grp = c("a", "b"), v = c(5, 6))
  p <- expect_plot2(plot2(d, grp))
  expect_labels(p, y = "v")
})

test_that("03.07 a missing x becomes the first column that is not y, with a message", {
  p <- expect_plot2_message(plot2(fx_cat_unique, y = y), "Using x = x", type = "info")
  expect_labels(p, x = "x", y = "y")
})

test_that("03.08 missing x and category: the first character columns become x and category", {
  d <- data.frame(a = c("p", "q", "p", "q"), b = c("u", "u", "v", "v"), v = 1:4)
  p <- expect_plot2_message(plot2(d), "Using category = b", type = "info")
  expect_labels(p, x = "a", y = "v")
  expect_identical(rlang::as_label(plot_of(p)$mapping$fill), "b")
})

test_that("03.09 category, facet and datalabels = FALSE remove these directions", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category = FALSE, facet = FALSE, datalabels = FALSE))
  expect_geoms(p, "bar", exact = TRUE)
  expect_identical(names(plot_data(p)), c("x", "y"))
  expect_facets(p, type = "FacetNull")
})

test_that("03.10 constant numeric columns of 0 or 1 used as category or datalabels are kept as data", {
  skip_open_finding("F007")
  p <- expect_plot2(plot2(data.frame(x = letters[1:3], y = 1:3, c = 0), x, y, c))
  expect_true("fill" %in% names(plot_of(p)$mapping))
  p <- expect_plot2(plot2(data.frame(x = letters[1:3], y = 4:6, d = 1), x, y, datalabels = d))
  expect_identical(datalabels_of(p), c("1", "1", "1"))
})

test_that("03.11 a category or datalabels containing NA does not crash", {
  skip_open_finding("F007")
  expect_plot2(plot2(data.frame(x = letters[1:3], y = 1:3, c = NA), x, y, c))
  expect_plot2(plot2(data.frame(x = letters[1:3], y = 1:3, d = c(1, NA, 1)), x, y, datalabels = d))
})

test_that("03.12 surrogate columns for y = n() and x = c(a, b) are in the data with matching labels", {
  p <- expect_plot2(plot2(fx_cat, x, n()))
  expect_true("n()" %in% names(plot_data(p)))
  expect_labels(p, y = "n()")
  p <- expect_plot2(plot2(fx_wide, x = c(first, second), y = third))
  expect_true("c(first, second)" %in% names(plot_data(p)))
  expect_labels(p, x = "c(first, second)")
})

test_that("03.13 datalabels = TRUE shows y; on tiles it shows the category", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, datalabels = TRUE))
  expect_setequal(datalabels_of(p), as.character(fx_cat_unique$y))
  d <- data.frame(x = c("a", "a", "b", "b"), y = c("p", "q", "p", "q"), v = c(1, 2, 3, 4))
  p <- expect_plot2(plot2(d, x, y, category = v, type = "tile", datalabels = TRUE))
  expect_setequal(datalabels_of(p), c("1", "2", "3", "4"))
})

test_that("03.14 the years heuristic only applies to discrete geoms", {
  skip_open_finding("F013")
  d <- data.frame(year = c(2001, 2002, 2005), v = c(1, 3, 2))
  p <- expect_plot2(plot2(d, year, v, type = "point"))
  expect_identical(axis_class(p, "x"), "continuous")
})

test_that("03.15 infinite y values are removed with a message", {
  d <- data.frame(x = c("a", "b", "c"), y = c(1, Inf, -Inf))
  p <- expect_plot2_message(plot2(d, x, y), "Removed 2 rows with an infinite value", type = "info")
  expect_identical(nrow(layer_df(p, "bar")), 1L)
})

test_that("03.16 an integer64 y is converted", {
  skip_if_not_installed("bit64")
  d <- data.frame(x = c("a", "b"))
  d$y <- bit64::as.integer64(c(3, 5))
  p <- expect_plot2(plot2(d, x, y))
  expect_identical(layer_df(p, "bar")$y, c(3, 5))
})

test_that("03.17 na.rm = TRUE removes rows with NA in x, category or facet and counts them correctly", {
  p <- expect_plot2_message(plot2(fx_na, x, y, category, facet, na.rm = TRUE),
                            "^Removed 3 rows since na.rm = TRUE$", type = "info")
  expect_false(anyNA(plot_data(p)$x))
})

test_that("03.18 na.replace replaces NA in character columns; numeric NA is untouched", {
  p <- expect_plot2_message(plot2(fx_na, x, y, category, na.replace = "Unknown"), "na.replace = \"Unknown\"", type = "info")
  expect_true("Unknown" %in% plot_data(p)$category)
  expect_true("Unknown" %in% plot_data(p)$x)
})

test_that("03.19 NA in y is removed with a warning that can be caught", {
  skip_open_finding("F014")
  expect_warning(plot2(fx_na, x, y), "y = NA")
})

test_that("03.20 a date x with the year as category unifies the years, with a message", {
  d <- data.frame(d = as.Date(c("2023-01-05", "2023-02-05", "2024-01-05", "2024-02-05")), v = 1:4,
                  yr = c("2023", "2023", "2024", "2024"))
  p <- expect_plot2_message(plot2(d, d, v, yr), "x.date_remove_years = TRUE", type = "info")
  expect_identical(length(unique(plot_data(p)$d)), 2L)
})

test_that("03.21 duplicate rows are summarised; identical rows with an explicit summarise_function give a caution", {
  p <- expect_no_plot2_message(plot2(fx_cat, x, y), "Applying")
  expect_identical(layer_df(p, "bar")$y, c(3, 7, 11))
  d <- data.frame(x = c("a", "a", "b"), y = c(2, 2, 3))
  p <- expect_plot2_message(plot2(d, x, y, summarise_function = mean), "summarise_function = mean", type = "caution")
  expect_identical(layer_df(p, "bar")$y, c(2, 3))
})

test_that("03.21b summarise_function = NULL disables summarising", {
  skip_open_finding("F073")
  p <- expect_plot2(plot2(fx_cat, x, y, summarise_function = NULL))
  expect_identical(nrow(plot_data(p)), 6L)
})

test_that("03.22 Sankey data are reshaped; the flow levels follow the order of x", {
  d <- data.frame(a = c("p", "q", "p"), b = c("u", "v", "v"), n = c(1, 2, 3))
  p <- expect_plot2(plot2(d, x = c(b, a), y = n, category = a, type = "sankey"))
  expect_identical(levels(factor(plot_data(p)$`_sankey_x`)), c("b", "a"))
})

test_that("03.23 the output is sorted by x", {
  d <- data.frame(x = c("c", "a", "b"), y = c(1, 2, 3))
  p <- expect_plot2(plot2(d, x, y, x.sort = "asc"))
  expect_identical(as.character(plot_data(p)$x), c("a", "b", "c"))
})

test_that("03.24 tibble, data.frame, data.table and grouped_df inputs give identical plots", {
  ref <- layer_df(plot2(fx_cat, x, y, category), "bar")
  expect_identical(layer_df(plot2(tibble::as_tibble(fx_cat), x, y, category), "bar"), ref)
  expect_identical(layer_df(plot2(dplyr::group_by(fx_cat, x), x, y, category), "bar"), ref)
  skip_if_not_installed("data.table")
  expect_identical(layer_df(plot2(data.table::as.data.table(fx_cat), x, y, category), "bar"), ref)
})

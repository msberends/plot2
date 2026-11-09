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

# I002, R02 sheet 23: summarise_data()
# review_and_overhaul/reviews/R02_validate/23_summarise_data.md

test_that("23.01 duplicate x values are summed by default", {
  p <- expect_plot2(plot2(fx_cat, x, y))
  expect_identical(layer_df(p, "bar")$y, c(3, 7, 11))
})

test_that("23.02 summarise_function = mean, median and max are applied", {
  expect_identical(layer_df(expect_plot2(plot2(fx_cat, x, y, summarise_function = mean)), "bar")$y, c(1.5, 3.5, 5.5))
  expect_identical(layer_df(expect_plot2(plot2(fx_cat, x, y, summarise_function = median)), "bar")$y, c(1.5, 3.5, 5.5))
  expect_identical(layer_df(expect_plot2(plot2(fx_cat, x, y, summarise_function = max)), "bar")$y, c(2, 4, 6))
})

test_that("23.03 an x, category or facet column named n, y or count is summarised correctly", {
  p <- expect_plot2(plot2(data.frame(count = c("a", "a", "b"), v = 1:3), count, v))
  expect_identical(layer_df(p, "bar")$y, c(3, 3))
  p <- expect_plot2(plot2(data.frame(n = c("a", "a", "b"), v = 1:3), n, v))
  expect_identical(layer_df(p, "bar")$y, c(3, 3))
})

test_that("23.04 a summarise_function that returns several values gives a clear error", {
  expect_error(plot2(fx_cat, x, y, summarise_function = range), "single value")
})

test_that("23.05 datalabels show the summarised values", {
  expect_identical(datalabels_of(expect_plot2(plot2(fx_cat, x, y, summarise_function = mean))), c("1.5", "3.5", "5.5"))
})

test_that("23.06 facets and categories are kept after summarising", {
  d <- data.frame(x = rep(c("a", "b"), 8), y = 1:16, c = rep(c("u", "v"), each = 2, times = 4),
                  f = rep(c("p", "q"), each = 8))
  p <- expect_plot2(plot2(d, x, y, c, f))
  expect_facets(p, n_panels = 2L)
  expect_identical(nrow(layer_df(p, "bar")), 8L)
  expect_equal(sum(layer_df(p, "bar")$y), sum(d$y))
})

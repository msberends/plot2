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

# R03 sheet E10: axis labels and scale assembly
# review_and_overhaul/reviews/R03_plot2_exec/E10_labels-and-scales.md

test_that("E10.01 axis titles are the column names; y = n() gives 'n()'", {
  expect_labels(plot2(fx_counts, group, n), x = "group", y = "n")
  expect_labels(plot2(fx_cat, x, n()), y = "n()")
})

test_that("E10.02 histogram y title is 'Count'", {
  expect_labels(plot2(fx_xy, x, type = "histogram"), y = "Count")
})

test_that("E10.03 density y title is 'Density' (F070)", {
  skip_open_finding("F070")
  expect_labels(plot2(fx_xy, x, type = "density"), y = "Density")
})

test_that("E10.04 numeric category gives a continuous colour scale", {
  p <- expect_plot2(plot2(fx_numcat, x, y, value, type = "point"))
  expect_true(scale_of(p, "colour")$is_discrete() == FALSE)
})

test_that("E10.05 category values with markdown are shown as expressions", {
  df <- data.frame(x = c("a", "b"), y = 1:2, k = c("*E. coli*", "*S. aureus*"))
  p <- expect_plot2(plot2(df, x, y, k))
  labs <- scale_labels(p, "fill")
  expect_false(identical(as.character(labs), c("*E. coli*", "*S. aureus*")))
})

test_that("E10.06 without x, the x-axis has no labels (F079)", {
  skip_open_finding("F079")
  p <- expect_plot2(plot2(data.frame(v = c(1, 2, 3, 4, 10)), y = v))
  l <- axis_labels(p, "x")
  expect_true(length(l) == 0 || all(is.na(l) | l == ""))
})

test_that("E10.07 the secondary axis is named after the secondary variable", {
  df <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20))
  p <- plot2(df, x, y, y_secondary = z)
  expect_identical(as.character(plot_of(p)$scales$get_scales("y")$secondary.axis$name), "z")
})

test_that("E10.08 named colours: legend limited to the categories present", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, colour = c(u = "red", v = "blue", w = "green")))
  expect_identical(as.character(scale_breaks(p, "fill")), c("u", "v"))
  expect_identical(col_hex(scale_values(p, "fill")[c("u", "v")]), c(u = "#FF0000", v = "#0000FF"))
})

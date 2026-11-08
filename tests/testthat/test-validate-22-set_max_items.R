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

# I002, R02 sheet 22: set_max_items()
# review_and_overhaul/reviews/R02_validate/22_set_max_items.md

d5 <- data.frame(x = c("a", "b", "c", "d", "e"), y = c(50, 40, 30, 20, 10))

test_that("22.01 five values with x.max_items = 3: two values plus the rest item (F062)", {
  expect_identical(axis_labels(plot2(d5, x, y, x.max_items = 3), "x"), c("a", "b", "(rest, x3)"))
})

test_that("22.02 the rest item holds the summed y of the merged values", {
  p <- plot2(d5, x, y, x.max_items = 3)
  expect_identical(p$data$y[as.character(p$data$x) == "(rest, x3)"], 60)
})

test_that("22.03 x.max_txt with the number of merged values and their share of y (F062)", {
  expect_identical(axis_labels(plot2(d5, x, y, x.max_items = 3, x.max_txt = "Other (%n, %p)"), "x"),
                   c("a", "b", "Other (3, 40%)"))
})

test_that("22.04 horizontal: the rest item at the bottom", {
  p <- plot2(d5, x, y, x.max_items = 3, horizontal = TRUE)
  expect_identical(rev(axis_labels(p, "y")), c("a", "b", "(rest, x3)"))
})

test_that("22.05 category.max_items and facet.max_items", {
  dc <- data.frame(x = rep("a", 5), y = c(50, 40, 30, 20, 10), cat = c("p", "q", "r", "s", "t"))
  expect_identical(scale_breaks(plot2(dc, x, y, cat, category.max_items = 3), "fill"), c("p", "q", "(rest, x3)"))
  df <- data.frame(x = rep("a", 5), y = c(50, 40, 30, 20, 10), f = c("p", "q", "r", "s", "t"))
  expect_identical(levels(built(plot2(df, x, y, facet = f, facet.max_items = 3))$layout$layout$f), c("p", "q", "(rest, x3)"))
})

test_that("22.06 numeric x with x.max_items warns", {
  expect_warning(plot2(fx_xy, x, y, x.max_items = 3), "only works when values are a character or (sorted) factor", fixed = TRUE)
})

test_that("22.07 barpercent shows at most 10 items by default", {
  d20 <- data.frame(x = sprintf("v%02d", 1:20), y = 20:1)
  p <- plot2(d20, x, y, type = "barpercent")
  expect_length(unique(p$data$x), 10)
  expect_identical(rev(axis_labels(p, "y"))[10], "(rest, x11)")
})

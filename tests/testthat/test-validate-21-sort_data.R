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

# 21: sort_data() (#157)

srt <- data.frame(x = c("A10", "b", "A2", "c", "b"), y = c(1, 2, 3, 4, 10))

test_that("21.01 character x is sorted naturally by default", {
  expect_identical(axis_labels(plot2(srt, x, y), "x"), c("A2", "A10", "b", "c"))
})

test_that("21.02 a factor keeps its level order by default", {
  d <- data.frame(x = factor(c("b", "a", "c"), levels = c("c", "b", "a")), y = 1:3)
  expect_identical(axis_labels(plot2(d, x, y), "x"), c("c", "b", "a"))
})

test_that("21.03 a vector of values comes first, the rest follows in data order", {
  expect_identical(axis_labels(plot2(srt, x, y, x.sort = c("c", "b")), "x"), c("c", "b", "A10", "A2"))
})

test_that("21.04 freq-desc with duplicates sorts on the summed y, with a message", {
  expect_plot2_message(plot2(srt, x, y, x.sort = "freq-desc"), "Applying x.sort = \"freq-desc\" using summarise_function = sum")
  expect_identical(axis_labels(plot2(srt, x, y, x.sort = "freq-desc"), "x"), c("b", "c", "A2", "A10"))
})

test_that("21.05 x.sort = FALSE keeps the order of appearance", {
  expect_identical(axis_labels(plot2(srt, x, y, x.sort = FALSE), "x"), c("A10", "b", "A2", "c"))
})

test_that("21.06 category.sort and facet.sort work alike", {
  d <- data.frame(x = rep(c("a", "b"), 2), y = 1:4, cat = rep(c("p", "q"), each = 2))
  expect_identical(scale_breaks(plot2(d, x, y, cat, category.sort = "desc"), "fill"), c("q", "p"))
  f <- data.frame(x = rep("a", 4), y = 1:4, f = c("z", "y", "z", "y"))
  expect_identical(levels(built(plot2(f, x, y, facet = f, facet.sort = "desc"))$layout$layout$f), c("z", "y"))
  expect_identical(levels(built(plot2(f, x, y, facet = f))$layout$layout$f), c("y", "z"))
})

test_that("21.07 x.drop = FALSE keeps unused factor levels on the axis", {
  d <- data.frame(x = factor(c("a", "b"), levels = c("a", "b", "zz")), y = 1:2)
  expect_identical(axis_labels(plot2(d, x, y, x.drop = FALSE), "x"), c("a", "b", "zz"))
})

test_that("21.08 natural sorting does not need stringr (#20)", {
  expect_no_match(paste(deparse(plot2:::natural_sort), collapse = " "), "stringr")
  expect_identical(plot2:::natural_sort(c("A10", "a2", "A2", "b")), c("a2", "A2", "A10", "b"))
})

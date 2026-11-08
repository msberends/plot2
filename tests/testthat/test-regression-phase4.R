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

# Regression tests for Phase 4 findings (input handling and parsing).
# Each test_that() starts with the finding ID; see review_and_overhaul/findings/.

test_that("F017 like() is case-insensitive without changing the meaning of the pattern", {
  expect_true(plot2:::like("ABC", "\\D"))
  expect_false(plot2:::like("123", "\\D"))
  expect_true(plot2:::like("abc", "^A"))
  expect_identical(plot2:::like("x", c("X", "y")), c(TRUE, FALSE))
})

test_that("F017 column names with regex characters can be plotted", {
  d <- data.frame(g = c("a", "b"), `rate (%)` = 1:2, `n[1` = 3:4, check.names = FALSE)
  p <- expect_plot2(plot2(d, g, c(`rate (%)`, `n[1`)))
  expect_setequal(as.character(scale_breaks(p, "fill")), c("rate (%)", "n[1"))
})

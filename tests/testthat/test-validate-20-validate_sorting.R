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

# I002, R02 sheet 20: validate_sorting()
# review_and_overhaul/reviews/R02_validate/20_validate_sorting.md

srt <- data.frame(x = c("b", "A10", "c", "A2"), y = c(5, 1, 9, 3))
srt_fct <- transform(srt, x = factor(x, levels = c("c", "A2", "b", "A10")))

# the order as read: left to right, or top to bottom when horizontal
read_order <- function(d, sort, horizontal = FALSE) {
  if (horizontal) {
    rev(axis_labels(plot2(d, x, y, x.sort = sort, horizontal = TRUE), "y"))
  } else {
    axis_labels(plot2(d, x, y, x.sort = sort), "x")
  }
}

expected <- list(
  "TRUE" = c("A2", "A10", "b", "c"), "asc" = c("A2", "A10", "b", "c"), "ascending" = c("A2", "A10", "b", "c"),
  "alpha" = c("A2", "A10", "b", "c"),
  "desc" = c("c", "b", "A10", "A2"), "descending" = c("c", "b", "A10", "A2"), "alpha-desc" = c("c", "b", "A10", "A2"),
  "FALSE" = c("b", "A10", "c", "A2"), "order" = c("b", "A10", "c", "A2"), "inorder" = c("b", "A10", "c", "A2"),
  "freq" = c("c", "b", "A2", "A10"), "freq-desc" = c("c", "b", "A2", "A10"), "infreq" = c("c", "b", "A2", "A10"),
  "freq-asc" = c("A10", "A2", "b", "c"))
as_sort <- function(nm) if (nm %in% c("TRUE", "FALSE")) as.logical(nm) else nm

test_that("20.01 every accepted spelling gives the documented order", {
  for (nm in names(expected)) {
    expect_identical(read_order(srt, as_sort(nm)), expected[[nm]], label = paste0("x.sort = ", nm))
  }
})

test_that("20.02 horizontal plots read from top to bottom in the same order (F141)", {
  for (nm in names(expected)) {
    expect_identical(read_order(srt, as_sort(nm), horizontal = TRUE), expected[[nm]], label = paste0("x.sort = ", nm, ", horizontal"))
  }
  # factors keep their level order, also when horizontal
  for (nm in c("TRUE", "asc", "alpha")) {
    expect_identical(read_order(srt_fct, as_sort(nm), horizontal = TRUE), c("c", "A2", "b", "A10"), label = paste0("factor, x.sort = ", nm))
  }
  expect_identical(read_order(srt_fct, "desc", horizontal = TRUE), c("A10", "b", "A2", "c"))
})

test_that("20.03 an invalid value errors and names it", {
  expect_error(plot2(srt, x, y, x.sort = "nonsense"), "invalid sorting option: 'nonsense'")
})

test_that("20.04 the legend is reversed for horizontal plots, except with frequency sorting", {
  d <- data.frame(x = rep(c("a", "b"), 2), y = 1:4, cat = rep(c("p", "q"), each = 2))
  expect_true(plot2(d, x, y, cat, horizontal = TRUE)$guides$guides$fill$params$reverse)
  expect_false(isTRUE(plot2(d, x, y, cat, horizontal = TRUE, category.sort = "freq-desc")$guides$guides$fill$params$reverse))
})

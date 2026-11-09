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

# A08: vignette 'An Introduction to plot2()'

test_that("A08.01 every chunk of the introduction vignette runs without error or warning", {
  skip_on_cran()
  skip_if_not_installed("broom")
  withr::local_pdf(NULL)
  # the vignette shows how to set options, so these must not leak into other tests
  withr::local_options(plot2.colour = NULL, plot2.decimal.mark = NULL, plot2.silent = NULL)
  res <- eval_chunks(source_file("vignettes", "plot2.Rmd"), exclude = "opts_chunk")
  expect_identical(res$code[!is.na(res$error)], character(0))
  expect_identical(res$code[!is.na(res$warnings)], character(0))
})

test_that("A08.02 x.max_items without sorting keeps the first values, not the most frequent", {
  p <- plot2(mtcars, carb, y = n(), x.max_items = 5, type = "col")
  expect_identical(as.character(axis_labels(p, "x"))[1:4], c("1", "2", "3", "4"))
})

test_that("A08.03 a custom x.sort order is followed and missing values are appended", {
  p <- plot2(mtcars, carb, y = n(), x.sort = c(4, 1, 6, 8))
  expect_identical(as.character(axis_labels(p, "x")), c("4", "1", "6", "8", "2", "3"))
})

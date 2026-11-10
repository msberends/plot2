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

# 19: complete_direction() (#157)

gaps <- data.frame(x = c(1L, 2L, 5L), y = c(3, 4, 6))

test_that("19.01 integer x with gaps: the missing values are added with y = 0", {
  p <- plot2(gaps, x, y, x.complete = TRUE)
  expect_identical(as.numeric(p$data$x), c(1, 2, 3, 4, 5))
  expect_identical(as.numeric(p$data$y), c(3, 4, 0, 0, 6))
})

test_that("19.02 x.complete = NA: the added rows are NA and removed with a warning", {
  expect_plot2_message(plot2(gaps, x, y, x.complete = NA), "Unable to plot 2 values where y = NA", type = "warning")
  expect_identical(nrow(suppressWarnings(plot2(gaps, x, y, x.complete = NA))$data), 3L)
})

test_that("19.03 dates are completed per day", {
  d <- data.frame(x = as.Date("2024-01-01") + c(0, 1, 4), y = c(1, 2, 3))
  expect_identical(plot2(d, x, y, x.complete = TRUE)$data$x, as.Date("2024-01-01") + 0:4)
})

test_that("19.04 date-times are completed per sensible period, not per second (#60)", {
  d <- data.frame(x = as.POSIXct("2024-01-01 00:00", tz = "UTC") + 3600 * c(0, 1, 5), y = c(1, 2, 3))
  expect_identical(nrow(plot2(d, x, y, x.complete = TRUE)$data), 6L)
})

test_that("19.05 character x is completed over the observed values within categories (#60)", {
  d <- data.frame(x = c("a", "b", "a", "c"), y = 1:4, cat = c("u", "u", "v", "v"))
  p <- plot2(d, x, y, cat, x.complete = TRUE)
  expect_identical(nrow(p$data), 6L)
  expect_identical(as.integer(p$data$y[p$data$x == "c" & p$data$cat == "u"]), 0L)
})

test_that("19.06 category.complete = TRUE with a numeric category", {
  d <- data.frame(x = c("a", "a", "b"), y = 1:3, cat = c(1L, 3L, 1L))
  p <- plot2(d, x, y, cat, category.complete = TRUE)
  expect_identical(sort(unique(as.numeric(p$data$cat))), c(1, 2, 3))
  expect_identical(nrow(p$data), 6L)
})

test_that("19.07 added rows get no datalabel, so a zero bar stays unlabelled", {
  p <- plot2(data.frame(x = c("a", "b"), y = 1:2, cat = c("u", "v")), x, y, cat, x.complete = TRUE)
  expect_identical(datalabels_of(p), c("1", NA, NA, "2"))
})

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

# R03 sheet E05: UpSet plots
# review_and_overhaul/reviews/R03_plot2_exec/E05_upset.md

# intersections: a&b x2, a x1, b&c x1, a&c x1, c x1; set sizes a 4, b 3, c 3
fx_upset <- data.frame(a = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
                       b = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
                       c = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
                       g = c("m", "f", "m", "f", "m", "f"),
                       w = c(1, 2, 3, 4, 5, 6))

test_that("E05.01 result is a patchwork of four plots", {
  local_known_deprecation("F075")
  p <- expect_plot2(plot2(fx_upset, x = c(a, b, c), type = "upset"))
  expect_s3_class(p, "patchwork")
  expect_length(p, 4)
})

test_that("E05.02 intersection sizes are the combination counts, sorted descending", {
  local_known_deprecation("F075")
  p <- plot2(fx_upset, x = c(a, b, c), type = "upset")
  expect_identical(as.numeric(layer_df(p[[2]], "bar")$y), c(2, 1, 1, 1, 1))
})

test_that("E05.03 set sizes equal the column sums", {
  local_known_deprecation("F075")
  p <- plot2(fx_upset, x = c(a, b, c), type = "upset")
  # drawn as negative bars, growing to the left
  expect_setequal(abs(as.numeric(layer_df(p[[3]], "bar")$y)), c(4, 3, 3))
})

test_that("E05.04 y title: default 'Intersection size', replaced by y.title", {
  local_known_deprecation("F075")
  upset_ylab <- function(p) toString(p[[1]]$layers[[1]]$aes_params$label)
  expect_identical(upset_ylab(plot2(fx_upset, x = c(a, b, c), type = "upset")), "Intersection size")
  expect_identical(upset_ylab(plot2(fx_upset, x = c(a, b, c), type = "upset", y.title = "Patients")), "Patients")
})

test_that("E05.05 a category stacks the intersection bars", {
  local_known_deprecation("F075")
  p <- expect_plot2(plot2(fx_upset, x = c(a, b, c), category = g, type = "upset"))
  expect_identical(as.character(scale_breaks(p[[2]], "fill")), c("f", "m"))
})

test_that("E05.06 with y, intersection heights are sums of y", {
  local_known_deprecation("F075")
  p <- plot2(fx_upset, x = c(a, b, c), y = w, type = "upset")
  # a&b: 1 + 2; a: 3; b&c: 4; a&c: 5; c: 6
  expect_setequal(as.numeric(layer_df(p[[2]], "bar")$y), c(3, 3, 4, 5, 6))
})

test_that("E05.07 x.sort = 'freq-asc' sorts ascending", {
  local_known_deprecation("F075")
  p <- plot2(fx_upset, x = c(a, b, c), type = "upset", x.sort = "freq-asc")
  expect_identical(as.numeric(layer_df(p[[2]], "bar")$y), c(1, 1, 1, 1, 2))
})

test_that("E05.08 a single x column errors", {
  local_known_deprecation("F075")
  expect_error(plot2(fx_upset, x = a, type = "upset"))
})

test_that("E05.09 x.sort as a vector (F071)", {
  skip_open_finding("F071")
  expect_plot2(plot2(fx_upset, x = c(a, b, c), type = "upset", x.sort = c("1", "2")))
})

test_that("E05.10 print = TRUE returns the plot invisibly (F010)", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_s3_class(plot2(fx_upset, x = c(a, b, c), type = "upset", print = TRUE), "patchwork")
})

test_that("E05.11 no internal state left behind", {
  local_known_deprecation("F075")
  expect_clean_state(plot2(fx_upset, x = c(a, b, c), type = "upset"), check = c("state", "locale"))
})

test_that("E05.11b the random-number stream is untouched (F002)", {
  expect_clean_state(plot2(fx_upset, x = c(a, b, c), type = "upset"), check = "rng")
})

test_that("E05.12 no R warnings (F075)", {
  skip_open_finding("F075")
  expect_plot2(plot2(fx_upset, x = c(a, b, c), type = "upset"))
})

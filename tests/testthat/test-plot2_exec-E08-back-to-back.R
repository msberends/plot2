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

# E08: back-to-back plots

fx_pop <- data.frame(age = rep(c("0-9", "10-19", "20-29"), 2),
                     sex = rep(c("M", "F"), each = 3),
                     n = c(10, 20, 30, 100, 200, 300))
fx_pop_cat <- data.frame(age = rep(c("0-9", "10-19"), 2),
                         sex = rep(c("M", "F"), each = 2),
                         grp = c("a", "b", "b", "b"),
                         n = c(1, 2, 3, 4))

test_that("E08.01 two halves; the left half has a reversed value axis", {
  skip_if_not_installed("patchwork")
  p <- expect_plot2(plot2(fx_pop, age, n, facet = sex, type = "b2b"))
  expect_s3_class(p, "patchwork")
  expect_length(p, 2)
  expect_lte(max(axis_range(p[[1]], "x")), 0)
  expect_gte(min(axis_range(p[[2]], "x")), 0)
})

test_that("E08.02 title and subtitle are placed on the combined plot", {
  skip_if_not_installed("patchwork")
  p <- plot2(fx_pop, age, n, facet = sex, type = "b2b", title = "Population", subtitle = "By age")
  expect_identical(p$patches$annotation$title, "Population")
  expect_identical(p$patches$annotation$subtitle, "By age")
})

test_that("E08.03 both halves share the same value range (#76)", {
  skip_if_not_installed("patchwork")
  p <- plot2(fx_pop, age, n, facet = sex, type = "b2b")
  expect_equal(abs(rev(axis_range(p[[1]], "x"))), axis_range(p[[2]], "x"))
})

test_that("E08.04 small counts with a category (#77)", {
  skip_if_not_installed("patchwork")
  expect_plot2(plot2(fx_pop_cat, age, n, grp, facet = sex, type = "b2b"))
})

test_that("E08.05 category colours are equal on both sides (#78)", {
  skip_if_not_installed("patchwork")
  d <- transform(fx_pop_cat, n = n * 100)
  p <- plot2(d, age, n, grp, facet = sex, type = "b2b")
  expect_identical(scale_values(p[[1]], "fill")[["b"]], scale_values(p[[2]], "fill")[["b"]])
})

test_that("E08.06 theme given as a function (#17)", {
  skip_if_not_installed("patchwork")
  expect_plot2(plot2(fx_pop, age, n, facet = sex, type = "b2b", theme = ggplot2::theme_bw))
})

test_that("E08.07 facet given as an expression (#33)", {
  skip_if_not_installed("patchwork")
  expect_plot2(plot2(fx_pop, age, n, facet = tolower(sex), type = "b2b"))
})

test_that("E08.08 a facet with three values errors", {
  df <- rbind(fx_pop, transform(fx_pop[1:3, ], sex = "X"))
  expect_error(plot2(df, age, n, facet = sex, type = "b2b"))
})

test_that("E08.09 no internal state left behind", {
  expect_clean_state(plot2(fx_pop, age, n, facet = sex, type = "b2b"))
})

test_that("E08.08 one legend for both halves, also when a category is missing on one side (#78)", {
  skip_if_not_installed("patchwork")
  pop3 <- data.frame(age = rep(c("0-9", "10-19"), 2), sex = rep(c("M", "F"), each = 2),
                     grp = c("a", "b", "b", "b"), n = c(100, 200, 300, 400))
  p <- expect_plot2(plot2(pop3, age, n, grp, facet = sex, type = "b2b"))
  g <- patchwork::patchworkGrob(p)
  expect_identical(sum(grepl("^guide-box", g$layout$name) & g$layout$name != "guide-box-inside" &
                         vapply(g$grobs, function(x) !inherits(x, "zeroGrob"), logical(1))), 1L)
  # a value range set by the user is kept
  pop <- data.frame(age = rep(c("0-9", "10-19"), 2), sex = rep(c("M", "F"), each = 2), n = 1:4)
  p <- plot2(pop, age, n, facet = sex, type = "b2b", y.limits = c(0, 100))
  expect_identical(axis_range(p[[2]], "x"), c(0, 100))
})

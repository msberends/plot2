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

# A03: plot2() arguments for axes and scales

x_range <- function(p) axis_range(p, "x")
y_range <- function(p) axis_range(p, "y")

# #104 (maintainer's decision): zoomed axes cover the data with ggplot2's usual 5% expansion
expand5 <- function(x) range(x) + c(-1, 1) * 0.05 * diff(range(x))

test_that("A03.01 x.zoom covers the data range with a 5% expansion, as documented", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, x.zoom = TRUE)
  expect_equal(x_range(p), expand5(iris$Sepal.Length))
})

test_that("A03.02 y.zoom covers the data range with a 5% expansion, as documented", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, y.zoom = TRUE)
  expect_equal(y_range(p), expand5(iris$Sepal.Width))
})

test_that("A03.03 zoom = TRUE equals x.zoom = TRUE plus y.zoom = TRUE", {
  a <- plot2(iris, Sepal.Length, Sepal.Width, zoom = TRUE)
  b <- plot2(iris, Sepal.Length, Sepal.Width, x.zoom = TRUE, y.zoom = TRUE)
  expect_equal(x_range(a), x_range(b))
  expect_equal(y_range(a), y_range(b))
  # with explicit limits, zoom = TRUE must not expand beyond them (it now does, y.zoom does not)
  expect_identical(y_range(plot2(iris, Sepal.Length, Sepal.Width, y.limits = c(1, NA), zoom = TRUE))[1], 1)
})

test_that("A03.04 zooming removes the zero baseline", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, zoom = TRUE)
  expect_gt(x_range(p)[1], 0)
  expect_gt(y_range(p)[1], 0)
})

test_that("A03.05 the documented default x.expand, when passed explicitly, gives the default plot", {
  expect_equal(x_range(plot2(iris, Sepal.Length, Sepal.Width, x.expand = NULL)),
               x_range(plot2(iris, Sepal.Length, Sepal.Width)))
})

test_that("A03.06 the y axis starts at zero where possible", {
  expect_identical(y_range(plot2(iris, Sepal.Length, Sepal.Width))[1], 0)
})

test_that("A03.07 x.date_breaks accepts human-readable intervals", {
  p <- plot2(admitted_patients, date, n(), x.date_breaks = "2 years", x.date_labels = "yyyy")
  labs <- as.integer(axis_labels(p, "x"))
  expect_true(all(diff(labs[!is.na(labs)]) == 2))
})

test_that("A03.08 y.percent_break sets the distance between percentage breaks", {
  d <- data.frame(x = c("a", "b"), y = c(0.2, 0.95))
  expect_equal(diff(axis_breaks(plot2(d, x, y, y.percent = TRUE, y.percent_break = 0.25), "y"))[1], 0.25)
})

test_that("A03.08b an explicit y.percent_break is honoured, also when it gives few breaks", {
  d <- data.frame(x = c("a", "b"), y = c(0.2, 0.55))
  expect_equal(diff(axis_breaks(plot2(d, x, y, y.percent = TRUE, y.percent_break = 0.25), "y"))[1], 0.25)
})

test_that("A03.09 y.limits = c(0, NA) starts the axis at zero", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, y.limits = c(0, NA))
  expect_identical(y_range(p)[1], 0)
  p <- plot2(iris, Sepal.Length, Sepal.Width, y.limits = c(1, NA))
  expect_identical(y_range(p)[1], 1)
})

test_that("A03.10 x.remove removes axis labels and title", {
  p <- plot2(fx_counts, group, n, x.remove = TRUE)
  expect_s3_class(theme_el(p, "axis.text.x"), "element_blank")
})

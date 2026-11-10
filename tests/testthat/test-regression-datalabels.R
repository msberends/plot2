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

# Regression tests for datalabels.
# Each test_that() starts with the GitHub issue number.

test_that("#37 datalabels are formatted once, on the final rows, with all placeholders", {
  d <- data.frame(x = rep(c("a", "b"), each = 50), y = 1:100)
  counter <- new.env()
  counter$n <- integer(0)
  trace("format_datalabels", tracer = bquote(assign("n", c(get("n", envir = .(counter)), length(datalabels)), envir = .(counter))), print = FALSE, where = asNamespace("plot2"))
  withr::defer(untrace("format_datalabels", where = asNamespace("plot2")))
  p <- plot2(d, x, y, type = "col", datalabels.format = "%n (%p) of %n")
  expect_identical(counter$n, 2L)
  expect_identical(datalabels_of(p), c("1 275 (25.2%) of 1 275", "3 775 (74.8%) of 3 775"))
})

test_that("#40 %p datalabels sum to 100% per x value by default, or per datalabels.percent_by", {
  d <- data.frame(x = rep(c("a", "b"), each = 2), c = rep(c("p", "q"), 2), f = "z", y = c(1, 3, 2, 2))
  pct <- function(p) as.double(sub("%", "", datalabels_of(p))) / 100
  p <- expect_plot2(plot2(d, x, y, c, datalabels.format = "%p", stacked = TRUE))
  expect_equal(sort(pct(p)), c(0.25, 0.5, 0.5, 0.75))
  p <- expect_plot2(plot2(d, x, y, c, datalabels.format = "%p", datalabels.percent_by = "total"))
  expect_equal(sort(pct(p)), c(0.125, 0.25, 0.25, 0.375))
  p <- expect_plot2(plot2(d, x, y, c, datalabels.format = "%p", datalabels.percent_by = "category"))
  expect_equal(sort(pct(p)), c(1 / 3, 0.4, 0.6, 2 / 3), tolerance = 0.001)
  # without a category, "x" is the same as within the facet
  p <- expect_plot2(plot2(admitted_patients, hospital, n(), facet = gender, datalabels.format = "%p"))
  ld <- layer_df(p, "text")
  expect_equal(as.vector(tapply(as.double(sub("%", "", ld$label)), ld$PANEL, sum)), c(100, 100), tolerance = 0.002)
  expect_error(plot2(d, x, y, c, datalabels.format = "%p", datalabels.percent_by = "row"), "must be")
})

test_that("#67 the datalabel background follows the layer data", {
  p <- plot2(data.frame(g = c("a", "b", "c"), v = 1:3), g, v)
  p$data <- data.frame(g = c("a", "b"), v = 4:5, datalabels = c("4", "5"))
  b <- ggplot2::ggplot_build(p)
  expect_identical(b$data[[1]]$label, c("4-", "5-"))
})

test_that("#74 max_items counts the rest item, %p is the share of y, %n the number of merged values", {
  d <- data.frame(g = letters[1:5], v = c(50, 20, 10, 10, 10))
  p <- plot2(d, g, v, x.max_items = 3, x.max_txt = "rest (%p, %n)")
  expect_identical(levels(p$data$g), c("a", "b", "rest (30%, 3)"))
  expect_identical(p$data$v, c(50, 20, 30))
  # without a numeric y, the share of rows
  p <- plot2(data.frame(g = c("a", "a", "b", "c", "d")), g, x.max_items = 2, x.max_txt = "rest (%p)")
  expect_identical(levels(p$data$g), c("a", "rest (60%)"))
})

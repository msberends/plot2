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

test_that("F018 apostrophes and backslashes in markdown text are kept", {
  expect_identical(deparse(md_to_expression("Children's *E. coli* isolates")[[1]]),
                   deparse(quote(paste("Children's ", italic("E. coli"), " isolates"))))
  expect_identical(deparse(md_to_expression("it's $omega$")[[1]]),
                   deparse(quote(paste("it's ", omega))))
  df <- data.frame(g = c("a", "b", "c"), v = 1:3)
  p <- expect_plot2(plot2(df, g, v, title = "Children's *E. coli* isolates"))
  expect_true(is.expression(ggplot2::get_labs(p)$title))
})

test_that("F018 a single or currency $ is text, paired $...$ is plotmath", {
  df <- data.frame(g = c("a", "b", "c"), v = 1:3)
  expect_type(ggplot2::get_labs(plot2(df, g, v, title = "Costs in $ per patient"))$title, "character")
  expect_type(ggplot2::get_labs(plot2(df, g, v, title = "From $5 to $10"))$title, "character")
  expect_identical(deparse(md_to_expression("$a$ and $b$")[[1]]), deparse(quote(paste(a, " and ", b))))
})

test_that("F018 markdown is not detected across titles", {
  expect_false(plot2:::validate_markdown(NULL, x.title = "a*b", y.title = "c*d", legend.title = NULL,
                                         title = NULL, subtitle = NULL, tag = NULL, caption = NULL))
  expect_true(plot2:::validate_markdown(NULL, x.title = "*ab*", y.title = NULL, legend.title = NULL,
                                        title = NULL, subtitle = NULL, tag = NULL, caption = NULL))
})

test_that("F019 formula terms are expressions, evaluated in the formula's environment", {
  k <- 2
  p <- expect_plot2(plot2(I(mpg * k) ~ I(hp + 1), data = mtcars))
  expect_equal(sort(layer_df(p, "point")$y), sort(mtcars$mpg * 2))
  expect_equal(sort(layer_df(p, "point")$x), sort(mtcars$hp + 1))
  expect_labels(p, x = "I(hp + 1)", y = "I(mpg * k)")
})

test_that("F019 `|` in a formula gives the facet, and too many terms error clearly", {
  m <- transform(mtcars, cyl = as.character(cyl), am = as.character(am))
  p <- expect_plot2(plot2(mpg ~ wt + cyl | am, data = m))
  expect_facets(p, n_panels = 2L)
  expect_error(plot2(mpg ~ wt + cyl + am + gear, data = m), "at most")
})

test_that("F081 a formula after a pipe keeps the other arguments", {
  p <- expect_plot2(mtcars |> plot2(mpg ~ wt, title = "Fuel"))
  expect_labels(p, x = "wt", y = "mpg", title = "Fuel")
})

test_that("F020 `data` outside formula notation errors clearly", {
  expect_error(plot2(iris, Species, Sepal.Length, data = mtcars), "only used in formula notation")
})

test_that("F031 types match exactly or by an unambiguous start, otherwise error with suggestions", {
  expect_identical(plot2:::validate_type("hist"), "geom_histogram")
  expect_identical(plot2:::validate_type("dens"), "geom_density")
  expect_identical(plot2:::validate_type("seg"), "geom_segment")
  expect_error(plot2:::validate_type(" "), "invalid")
  expect_error(plot2:::validate_type("()"), "invalid")
  expect_error(plot2:::validate_type("boxplt"), "did you mean \"boxplot\"")
  # a part of a name in the middle is not a match ("line" is not "hline")
  expect_error(plot2:::validate_type("ine"), "invalid")
})

test_that("F013 years and months become discrete only for discrete or automatic types and whole numbers", {
  years <- data.frame(year = 2015:2024, n = 1:10)
  expect_identical(axis_class(plot2(years, year, n), "x"), "discrete")
  expect_identical(axis_class(plot2(years, year, n, type = "col"), "x"), "discrete")
  expect_identical(axis_class(plot2(years, year, n, type = "line"), "x"), "continuous")
  expect_identical(axis_class(plot2(data.frame(m = 1:12, v = 12:1), m, v, type = "point"), "x"), "continuous")
  expect_identical(axis_class(plot2(years, year, n, x.character = FALSE), "x"), "continuous")
  expect_false(plot2:::looks_like_years(c(2001.5, 2010)))
  expect_true(plot2:::looks_like_months(c(1:12, NA)))
})

test_that("F028 %p datalabels sum to 100% per x value by default, or per datalabels.percent_by", {
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

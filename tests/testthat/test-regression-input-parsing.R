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

# Regression tests for input handling and parsing.
# Each test_that() starts with the GitHub issue number.

test_that("#29 like() is case-insensitive without changing the meaning of the pattern", {
  expect_true(plot2:::like("ABC", "\\D"))
  expect_false(plot2:::like("123", "\\D"))
  expect_true(plot2:::like("abc", "^A"))
  expect_identical(plot2:::like("x", c("X", "y")), c(TRUE, FALSE))
})

test_that("#29 column names with regex characters can be plotted", {
  d <- data.frame(g = c("a", "b"), `rate (%)` = 1:2, `n[1` = 3:4, check.names = FALSE)
  p <- expect_plot2(plot2(d, g, c(`rate (%)`, `n[1`)))
  expect_setequal(as.character(scale_breaks(p, "fill")), c("rate (%)", "n[1"))
})

test_that("#30 apostrophes and backslashes in markdown text are kept", {
  expect_identical(deparse(md_to_expression("Children's *E. coli* isolates")[[1]]),
                   deparse(quote(paste("Children's ", italic("E. coli"), " isolates"))))
  expect_identical(deparse(md_to_expression("it's $omega$")[[1]]),
                   deparse(quote(paste("it's ", omega))))
  df <- data.frame(g = c("a", "b", "c"), v = 1:3)
  p <- expect_plot2(plot2(df, g, v, title = "Children's *E. coli* isolates"))
  expect_true(is.expression(ggplot2::get_labs(p)$title))
})

test_that("#30 a single or currency $ is text, paired $...$ is plotmath", {
  df <- data.frame(g = c("a", "b", "c"), v = 1:3)
  expect_type(ggplot2::get_labs(plot2(df, g, v, title = "Costs in $ per patient"))$title, "character")
  expect_type(ggplot2::get_labs(plot2(df, g, v, title = "From $5 to $10"))$title, "character")
  expect_identical(deparse(md_to_expression("$a$ and $b$")[[1]]), deparse(quote(paste(a, " and ", b))))
})

test_that("#30 markdown is not detected across titles", {
  expect_false(plot2:::validate_markdown(NULL, x.title = "a*b", y.title = "c*d", legend.title = NULL,
                                         title = NULL, subtitle = NULL, tag = NULL, caption = NULL))
  expect_true(plot2:::validate_markdown(NULL, x.title = "*ab*", y.title = NULL, legend.title = NULL,
                                        title = NULL, subtitle = NULL, tag = NULL, caption = NULL))
})

test_that("#31 formula terms are expressions, evaluated in the formula's environment", {
  k <- 2
  p <- expect_plot2(plot2(I(mpg * k) ~ I(hp + 1), data = mtcars))
  expect_equal(sort(layer_df(p, "point")$y), sort(mtcars$mpg * 2))
  expect_equal(sort(layer_df(p, "point")$x), sort(mtcars$hp + 1))
  expect_labels(p, x = "I(hp + 1)", y = "I(mpg * k)")
})

test_that("#31 `|` in a formula gives the facet, and too many terms error clearly", {
  m <- transform(mtcars, cyl = as.character(cyl), am = as.character(am))
  p <- expect_plot2(plot2(mpg ~ wt + cyl | am, data = m))
  expect_facets(p, n_panels = 2L)
  expect_error(plot2(mpg ~ wt + cyl + am + gear, data = m), "at most")
})

test_that("#32 `data` outside formula notation errors clearly", {
  expect_error(plot2(iris, Species, Sepal.Length, data = mtcars), "only used in formula notation")
})

test_that("#55 transformation objects are accepted for x, y and category", {
  d <- data.frame(x = c(1, 10, 100, 1000), y = c(3, 1, 4, 2), v = c(1, 10, 100, 1000))
  expect_identical(axis_labels(expect_plot2(plot2(d, x, y, x.transform = scales::transform_log10())), "x"),
                   c("1", "10", "100", "1 000"))
  expect_plot2(plot2(fx_cat, x, y, y.transform = scales::transform_reverse()))
  expect_plot2(plot2(d, x, y, v, category.transform = scales::transform_log10()))
})

test_that("#84 character vectors given as a long expression or named y are plotted", {
  p <- expect_plot2(plot2(c("a", "b", "b", "c", "c", "c", "a", "a", "a", "b")))
  expect_identical(sort(layer_df(p, 1)$y), c(3, 3, 4))
  y <- c("p", "q", "q")
  expect_plot2(plot2(y))
})

test_that("#91 a missing x is guessed by column name, and left empty when no column is left", {
  expect_geoms(plot2(data.frame(value = c(1, 5, 3, 8, 2)), y = value), c("errorbar", "boxplot"), exact = TRUE)
  p <- expect_plot2(plot2(data.frame(g = c("a", "b", "a"), value = c(1, 5, 3)), y = value))
  expect_labels(p, x = "g")
})

test_that("#93 a formula after a pipe keeps the other arguments", {
  p <- expect_plot2(mtcars |> plot2(mpg ~ wt, title = "Fuel"))
  expect_labels(p, x = "wt", y = "mpg", title = "Fuel")
})

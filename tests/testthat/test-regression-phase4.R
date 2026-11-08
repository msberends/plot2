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

test_that("F022 the secondary axis maps the full range, also for zero and negative values", {
  sec_labels <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.sec$get_labels()
  d <- data.frame(x = c("a", "b", "c"), y = c(10, 20, 30), z = c(0, 0, 0))
  expect_plot2(plot2(d, x, y, y_secondary = z))
  d$z <- c(-5, 5, 15)
  p <- expect_plot2(plot2(d, x, y, y_secondary = z))
  # the secondary values span the primary range: -5 at the lowest, 15 at the highest break
  expect_identical(sec_labels(p)[c(1, 4)], c("-5.00", "15.00"))
  expect_equal(range(layer_df(p, "line")$y), c(0, 30))
})

test_that("F068 the secondary series of a column plot is a line with points in the secondary colour", {
  df <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20))
  p <- expect_plot2(plot2(df, x, y, y_secondary = z))
  expect_geoms(p, c("bar", "line", "point"))
  # one connected line over the discrete x axis, scaled onto the primary axis
  expect_identical(length(unique(layer_df(p, "line")$group)), 1L)
  expect_equal(max(layer_df(p, "line")$y), 3)
  expect_false(identical(unique(layer_df(p, "line")$colour), unique(layer_df(p, "bar")$fill)))
  # with a category: one line per category, without warnings
  d2 <- data.frame(x = rep(c("a", "b", "c"), 2), c = rep(c("p", "q"), each = 3), y = 1:6, z = c(10, 30, 20, 5, 8, 9))
  p <- expect_plot2(plot2(d2, x, y, c, y_secondary = z))
  expect_identical(length(unique(layer_df(p, "line")$group)), 2L)
  # other primary types keep their type
  expect_geoms(plot2(df, x, y, y_secondary = z, type = "point"), c("point", "point"), exact = TRUE)
})

test_that("F050 legend.position accepts FALSE, \"inside\" and coordinates", {
  d <- data.frame(g = c("a", "b"), v = 1:2, c = c("x", "y"))
  expect_identical(legend_position(expect_plot2(plot2(d, g, v, c, legend.position = FALSE))), "none")
  p <- expect_plot2(plot2(d, g, v, c, legend.position = c(0.9, 0.8)))
  expect_identical(legend_position(p), "inside")
  expect_equal(theme_of(p)$legend.position.inside, c(0.9, 0.8))
  expect_identical(legend_position(expect_plot2(plot2(d, g, v, c, legend.position = "inside"))), "inside")
  p <- expect_plot2(plot2(iris, Sepal.Length, Sepal.Width, Petal.Length, legend.position = c(0.9, 0.9)))
  expect_identical(legend_position(p), "inside")
  expect_error(plot2(d, g, v, c, legend.position = c(2, 1)), "between 0 and 1")
})

test_that("F052 the data-dependent choice of facet scales is reported", {
  expect_plot2_message(plot2(admitted_patients, hospital, n(), facet = ward), "Using free y scales per facet", type = "info")
  d <- data.frame(x = rep(c("a", "b"), 2), f = rep(c("p", "q"), each = 2), y = c(10, 11, 10, 12))
  expect_plot2_message(plot2(d, x, y, facet = f), "facet.fixed_y = TRUE", type = "info")
})

test_that("F063 scientific notation only from 1e9 or below 1e-4, otherwise big marks or more decimals", {
  lab <- function(v) axis_labels(plot2(data.frame(g = c("a", "b"), v = v), g, v), "y")
  expect_true(all(grepl("^[0-9 ]+$", lab(c(500000, 2500000)))))
  expect_true(any(grepl("10^9", lab(c(5e8, 2.5e9)), fixed = TRUE)))
  expect_true(any(grepl("10^-5", lab(c(0.00001, 0.00005)), fixed = TRUE)))
  expect_true("0.001" %in% lab(c(0.001, 0.005)))
})

test_that("F032 a partial theme is added on top of the default theme and wins", {
  d <- data.frame(g = c("a", "b", "c"), v = 1:3)
  p <- expect_plot2(plot2(d, g, v, theme = ggplot2::theme(panel.background = ggplot2::element_rect(fill = "pink"))))
  expect_identical(theme_el(p, "panel.background")$fill, "pink")
  # the rest of the default theme is still there
  expect_identical(theme_el(p, "axis.text.x")$size, theme_el(plot2(d, g, v), "axis.text.x")$size)
  expect_type(theme_el(plot2(d, g, v, facet = g), "strip.text")$size, "double")
})

test_that("F057 text_factor multiplies every text size exactly", {
  els <- c("axis.text.x", "axis.title.y", "plot.title", "legend.text", "strip.text")
  sizes <- function(p) vapply(els, function(el) theme_el(p, el)$size, double(1))
  for (th in c("theme_minimal2", "theme_bw")) {
    expect_equal(sizes(plot2(iris, Species, theme = th, text_factor = 2)) / sizes(plot2(iris, Species, theme = th)),
                 stats::setNames(rep(2, length(els)), els))
  }
})

test_that("F055 the datalabel background follows the layer data", {
  p <- plot2(data.frame(g = c("a", "b", "c"), v = 1:3), g, v)
  p$data <- data.frame(g = c("a", "b"), v = 4:5, datalabels = c("4", "5"))
  b <- ggplot2::ggplot_build(p)
  expect_identical(b$data[[1]]$label, c("4-", "5-"))
})

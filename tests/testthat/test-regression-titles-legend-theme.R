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

# Regression tests for titles, legend and theme.
# Each test_that() starts with the GitHub issue number.

test_that("#44 a partial theme is added on top of the default theme and wins", {
  d <- data.frame(g = c("a", "b", "c"), v = 1:3)
  p <- expect_plot2(plot2(d, g, v, theme = ggplot2::theme(panel.background = ggplot2::element_rect(fill = "pink"))))
  expect_identical(theme_el(p, "panel.background")$fill, "pink")
  # the rest of the default theme is still there
  expect_identical(theme_el(p, "axis.text.x")$size, theme_el(plot2(d, g, v), "axis.text.x")$size)
  expect_type(theme_el(plot2(d, g, v, facet = g), "strip.text")$size, "double")
})

test_that("#62 legend.position accepts FALSE, \"inside\" and coordinates", {
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

test_that("#69 text_factor multiplies every text size exactly", {
  els <- c("axis.text.x", "axis.title.y", "plot.title", "legend.text", "strip.text")
  sizes <- function(p) vapply(els, function(el) theme_el(p, el)$size, double(1))
  for (th in c("theme_minimal2", "theme_bw")) {
    expect_equal(sizes(plot2(iris, Species, theme = th, text_factor = 2)) / sizes(plot2(iris, Species, theme = th)),
                 stats::setNames(rep(2, length(els)), els))
  }
})

test_that("#99 theme_minimal2(...) changes only the given properties", {
  t_default <- theme_minimal2()
  t <- theme_minimal2(plot.title = ggplot2::element_text(face = "italic"))
  expect_identical(ggplot2::calc_element("plot.title", t)$face, "italic")
  expect_identical(ggplot2::calc_element("plot.title", t)$colour, ggplot2::calc_element("plot.title", t_default)$colour)
  expect_identical(ggplot2::calc_element("plot.title", t)$size, ggplot2::calc_element("plot.title", t_default)$size)
})

test_that("#103 theme_minimal2() text sizes are plain numbers", {
  size <- ggplot2::calc_element("plot.title", theme_minimal2())$size
  expect_false(inherits(size, "unit"))
  expect_equal(as.numeric(size), 13)
})

test_that("#117 titles show the column name in functions and loops", {
  h <- function(v) plot2(iris, !!rlang::sym(v), n())
  expect_labels(h("Species"), x = "Species")
  ps <- lapply(c("Sepal.Length", "Sepal.Width"), function(v) plot2(iris, Species, .data[[v]]))
  expect_identical(vapply(ps, function(p) p$labels$y, ""), c("Sepal.Length", "Sepal.Width"))
  k <- function(df, col) plot2(df, Species, {{ col }}, y.title = TRUE)
  expect_labels(k(iris, Petal.Width), y = "Petal.Width")
  # in-line calculations keep their code as title
  expect_labels(plot2(iris, Species, mean(.data$Sepal.Length)), y = "mean(Sepal.Length)")
})

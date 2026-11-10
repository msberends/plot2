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

# Regression tests for plot types and added layers.
# Each test_that() starts with the GitHub issue number.

test_that("#25 years and months become discrete only for discrete or automatic types and whole numbers", {
  years <- data.frame(year = 2015:2024, n = 1:10)
  expect_identical(axis_class(plot2(years, year, n), "x"), "discrete")
  expect_identical(axis_class(plot2(years, year, n, type = "col"), "x"), "discrete")
  expect_identical(axis_class(plot2(years, year, n, type = "line"), "x"), "continuous")
  expect_identical(axis_class(plot2(data.frame(m = 1:12, v = 12:1), m, v, type = "point"), "x"), "continuous")
  expect_identical(axis_class(plot2(years, year, n, x.character = FALSE), "x"), "continuous")
  expect_false(plot2:::looks_like_years(c(2001.5, 2010)))
  expect_true(plot2:::looks_like_months(c(1:12, NA)))
})

test_that("#43 types match exactly or by an unambiguous start, otherwise error with suggestions", {
  expect_identical(plot2:::validate_type("hist"), "geom_histogram")
  expect_identical(plot2:::validate_type("dens"), "geom_density")
  expect_identical(plot2:::validate_type("seg"), "geom_segment")
  expect_error(plot2:::validate_type(" "), "invalid")
  expect_error(plot2:::validate_type("()"), "invalid")
  expect_error(plot2:::validate_type("boxplt"), "did you mean \"boxplot\"")
  # a part of a name in the middle is not a match ("line" is not "hline")
  expect_error(plot2:::validate_type("ine"), "invalid")
})

test_that("#52 add_type() adds a layer when data is given, and warns when there is no data at all", {
  d <- data.frame(x = 1:3, y = 1:3)
  p <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y), data = d)
  expect_length(add_type(p, type = "line", mapping = ggplot2::aes(x, y), data = d)$layers, 2L)
  expect_warning(p2 <- add_type(p, type = "line", mapping = ggplot2::aes(x, y)), "no data", class = "plot2_warning")
  expect_length(p2$layers, 1L)
  skip_if_not_installed("ggbeeswarm")
  p3 <- add_type(plot2(iris, Species, Sepal.Length, type = "boxplot"), type = "beeswarm")
  expect_s3_class(p3$layers[[length(p3$layers)]]$position, "PositionBeeswarm")
})

test_that("#83 type requirements are checked before the data are prepared", {
  skip_if_not_installed("ggforce")
  d <- data.frame(from = c("A", "A", "B"), to = c("X", "Y", "X"), n = 1:3, grp = c("A", "A", "B"))
  expect_error(plot2(d, x = from, y = n, category = grp, type = "sankey"), "at least two variables")
  # without y, a Sankey plot counts rows (#118)
  expect_plot2(plot2(d, x = c(from, to), category = grp, type = "sankey"))
  expect_error(plot2(d, x = c(from, to), y = n, type = "sankey"), "require `category`")
})

test_that("#86 a histogram is chosen by the arguments, not by identical values", {
  expect_geoms(plot2(data.frame(x = 1:4, y = 1:4), x, y), "point", exact = TRUE)
  expect_true("GeomBar" %in% geoms(plot2(data.frame(x = c(1, 2, 2, 3, 3, 3)), x, x)))
})

test_that("#94 add_errorbar() works without a category and keeps the facets", {
  d <- data.frame(g = c("a", "b", "a", "b"), n = c(3, 4, 5, 6), f = c("p", "p", "q", "q"))
  p <- expect_plot2(add_errorbar(plot2(d, g, n, facet = f), min = n - 1, max = n + 1))
  eb <- layer_df(p, "errorbar")
  expect_identical(nrow(eb), 4L)
  expect_identical(sort(unique(as.integer(eb$PANEL))), 1:2)
})

test_that("#95 move_layer() reorders by index and clamps the new position", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::geom_rug()
  expect_identical(geoms(move_layer(p, layer = 1, move = 2)), c("GeomLine", "GeomRug", "GeomPoint"))
  expect_identical(geoms(move_layer(p, layer = 1, move = 10)), c("GeomLine", "GeomRug", "GeomPoint"))
  expect_identical(geoms(move_layer(p, layer = 2, move = 0)), c("GeomPoint", "GeomLine", "GeomRug"))
})

test_that("#143 violin plots with single-observation groups render", {
  d <- data.frame(x = c("a", "a", "a", "b"), y = c(1, 2, 3, 4))
  p <- expect_plot2(plot2(d, x, y, type = "violin"), allow_warnings = TRUE)
  expect_true(nrow(suppressWarnings(layer_df(p, "violin"))) > 0)
})

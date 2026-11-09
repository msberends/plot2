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

# 01: validate_type() (#157)

test_that("01.01 without x, a numeric y gives a boxplot with a message", {
  df <- data.frame(y = c(1, 3, 2, 5, 4))
  expect_plot2_message(plot2(df, y = y), "since there is no X-axis", type = "info")
  expect_geoms(plot2(df, y = y), "boxplot")
})

test_that("01.02 numeric x and y give points", {
  expect_geoms(plot2(fx_xy, x, y), "point", exact = TRUE)
})

test_that("01.03 only one numeric variable as x gives a histogram", {
  expect_geoms(plot2(fx_xy, x), "bar")
  expect_geoms(plot2(fx_xy, x), "density")
})

test_that("01.04 character x: boxplot when every group has three values, columns otherwise", {
  three <- data.frame(x = rep(c("a", "b"), each = 3), y = 1:6)
  expect_geoms(plot2(three, x, y), "boxplot")
  expect_geoms(plot2(fx_cat_unique, x, y), "bar")
  expect_false("GeomBoxplot" %in% geoms(plot2(fx_cat_unique, x, y)))
})

test_that("01.05 the plot2.default_type option is respected", {
  withr::local_options(plot2.default_type = "geom_point")
  expect_geoms(plot2(fx_cat_unique, x, y), "point", exact = TRUE)
  # normalised like `type` (#107)
  withr::local_options(plot2.default_type = "l")
  expect_geoms(plot2(fx_cat_unique, x, y), "line", exact = TRUE)
})

test_that("01.06 every single-letter alias maps to the documented geom", {
  aliases <- c(a = "GeomArea", b = "GeomBoxplot", c = "GeomBar", j = "GeomPoint",
               l = "GeomLine", p = "GeomPoint", r = "GeomRibbon", v = "GeomViolin")
  for (a in names(aliases)) {
    expect_true(aliases[[a]] %in% geoms(plot2(fx_cat, x, y, type = a)), label = paste0("type = \"", a, "\""))
  }
  expect_geoms(plot2(fx_xy, x, type = "h"), "bar")
  skip_if_not_installed("ggbeeswarm")
  expect_geoms(plot2(iris, Species, Sepal.Length, type = "bs"), "point")
})

test_that("01.07 type names are normalised", {
  expect_geoms(plot2(fx_cat, x, y, type = "points"), "point", exact = TRUE)
  expect_geoms(plot2(fx_cat, x, y, type = "Column"), "bar")
  expect_geoms(plot2(fx_cat, x, y, type = "ggplot2::geom_col()"), "bar")
})

test_that("01.08 an unknown type errors and names the type (#43)", {
  expect_error(plot2(fx_xy, x, y, type = "xyz"), "plot type \"xyz\" is invalid")
})

test_that("01.09 empty or symbol-only types error (#43)", {
  expect_error(plot2(fx_xy, x, y, type = " "), "is invalid")
  expect_error(plot2(fx_xy, x, y, type = "()"), "is invalid")
})

test_that("01.10 a type of length 2 warns and uses the first (#149)", {
  expect_warning(p <- plot2(fx_cat_unique, x, y, type = c("col", "line")), "can only be of length 1", class = "plot2_warning")
  expect_geoms(p, "bar")
  expect_false("GeomLine" %in% geoms(p))
})

test_that("01.11 beeswarm uses ggbeeswarm", {
  skip_if_not_installed("ggbeeswarm")
  p <- expect_plot2(plot2(iris, Species, Sepal.Length, type = "beeswarm"))
  expect_true(any(vapply(p$layers, function(l) inherits(l$position, "PositionBeeswarm") || grepl("beeswarm", class(l$geom)[1], ignore.case = TRUE) || grepl("Beeswarm", class(l$position)[1]), logical(1))))
})

test_that("01.12 several x variables with a two-level category suggest a Sankey plot", {
  expect_plot2_message(plot2(admitted_patients, x = c(hospital, ward), y = n(), category = gender),
                       "a Sankey plot can be used", type = "info")
})

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

# plot2() arguments for directions, type and titles

test_that("type left blank: two numeric axes give points", {
  expect_geoms(plot2(iris, Sepal.Length, Sepal.Width), "point", exact = TRUE)
})

test_that("type left blank: at least three values per x give a boxplot", {
  d <- data.frame(x = rep(c("a", "b"), each = 5), y = 1:10)
  expect_geoms(plot2(d, x, y), "boxplot")
})

test_that("type left blank otherwise: columns, or the plot2.default_type option", {
  d <- data.frame(x = c("a", "b"), y = 1:2)
  expect_geoms(plot2(d, x, y), "bar")
  withr::local_options(plot2.default_type = "geom_line")
  expect_geoms(plot2(d, x, y), "line", exact = TRUE)
})

test_that("plot2.default_type accepts the same values as type", {
  d <- data.frame(x = c("a", "b"), y = 1:2)
  withr::local_options(plot2.default_type = "line")
  expect_geoms(plot2(d, x, y), "line", exact = TRUE)
  withr::local_options(plot2.default_type = "l")
  expect_geoms(plot2(d, x, y), "line", exact = TRUE)
})

test_that("type = 'bar' gives horizontal columns", {
  p <- plot2(fx_counts, group, n, type = "bar")
  expect_geoms(p, "bar")
  expect_identical(coord_class(p), "CoordFlip")
})

test_that("type = 'barpercent' is horizontal, sorted by frequency, with '%n (%p)' labels", {
  d <- data.frame(g = c("a", "b", "b", "c", "c", "c"))
  p <- plot2(d, g, n(), type = "barpercent")
  expect_identical(coord_class(p), "CoordFlip")
  # horizontal: the first axis label is at the bottom, so the most frequent value is on top
  expect_identical(as.character(axis_labels(p, "y")), c("a", "b", "c"))
  expect_true(all(grepl("^[0-9]+ \\([0-9.]+%\\)$", datalabels_of(p))))
})

test_that("type = 'barpercent' without y counts rows", {
  d <- data.frame(g = c("a", "b", "b"), id = c(101, 102, 103))
  p <- plot2(d, g, type = "barpercent")
  expect_identical(sort(layer_df(p, layer_index(p, "GeomBar"))$y), c(1, 2))
  expect_true(length(datalabels_of(p)) == 2)
})

test_that("documented type abbreviations give the documented geoms", {
  d <- data.frame(x = rep(c("a", "b"), each = 5), y = c(1:10))
  expect_geoms(plot2(d, x, y, type = "a"), "area")
  expect_geoms(plot2(d, x, y, type = "b"), "boxplot")
  expect_geoms(plot2(d, x, y, type = "c"), "bar")
  expect_geoms(plot2(fx_xy, x, type = "h"), "bar")
  expect_geoms(plot2(d, x, y, type = "j"), "point")
  expect_geoms(plot2(d, x, y, type = "l"), "line")
  expect_geoms(plot2(d, x, y, type = "p"), "point")
  expect_geoms(plot2(d, x, y, type = "v"), "violin")
  expect_geoms(plot2(d, x, y, type = "ld"), c("line", "point"))
  d3 <- data.frame(x = c("a", "b", "c"), y = c(3, 5, 4))
  expect_geoms(plot2(d3, x, y, type = "sp"), "polygon")
})

test_that("type = 'bs' gives a beeswarm when ggbeeswarm is installed", {
  skip_if_not_installed("ggbeeswarm")
  expect_identical(class(plot2(iris, Species, Sepal.Length, type = "bs")$layers[[1]]$geom)[1], "GeomPoint")
})

test_that("a missing ggbeeswarm gives an install hint, not 'geom_beeswarm() does not exist'", {
  skip_if(requireNamespace("ggbeeswarm", quietly = TRUE), "ggbeeswarm installed")
  expect_error(plot2(iris, Species, Sepal.Length, type = "beeswarm"), "ggbeeswarm")
})

test_that("dumbbell requires exactly two category values, as documented", {
  expect_error(plot2(admitted_patients, hospital, median(age), age_group, type = "dumbbell"), "two distinct values")
  expect_geoms(plot2(admitted_patients, hospital, median(age), gender, type = "dumbbell"), c("point", "segment"))
})

test_that("category.title is shown for a numeric legend and hidden for a discrete one", {
  expect_labels(plot2(iris, Sepal.Length, Sepal.Width, Petal.Length), colour = "Petal.Length")
  expect_labels(plot2(admitted_patients, hospital, n(), gender), fill = NULL)
})

test_that("titles accept a calculation over the data", {
  p <- plot2(admitted_patients, age, type = "hist", title = paste("n =", n_distinct(patient_id)))
  expect_labels(p, title = paste("n =", dplyr::n_distinct(admitted_patients$patient_id)))
})

test_that("x.title = FALSE removes the axis title", {
  expect_labels(plot2(fx_counts, group, n, x.title = FALSE), x = NULL)
})

test_that("'line-point', used in the vignette, is a documented alias of linedot", {
  expect_geoms(plot2(fx_xy, x, y, type = "line-point"), c("line", "point"))
})

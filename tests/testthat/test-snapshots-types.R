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

# Visual snapshots, one per plot type (F038). Skipped on CRAN and without vdiffr.
# After an intended visual change, review with testthat::snapshot_review().

test_that("SNAP.01 column, stacked, horizontal and percentage bars", {
  expect_plot2_snapshot("col", plot2(fx_counts, group, n))
  expect_plot2_snapshot("col-category", plot2(fx_cat_unique, x, y, category))
  expect_plot2_snapshot("col-stacked", plot2(fx_cat_unique, x, y, category, stacked = TRUE))
  expect_plot2_snapshot("col-stacked-fill", plot2(fx_cat_unique, x, y, category, stacked_fill = TRUE))
  expect_plot2_snapshot("bar-horizontal", plot2(fx_counts, group, n, horizontal = TRUE, x.sort = "freq-desc"))
})

test_that("SNAP.02 point, line, area and smooth", {
  expect_plot2_snapshot("point", plot2(fx_xy, x, y))
  expect_plot2_snapshot("point-numeric-category", plot2(fx_numcat, x, y, value))
  expect_plot2_snapshot("line-dates", plot2(fx_dates, date, y, type = "line"))
  expect_plot2_snapshot("area", plot2(fx_dates, date, y, type = "area"))
  expect_plot2_snapshot("smooth", plot2(mtcars, mpg, hp, smooth = TRUE, smooth.method = "lm"))
})

test_that("SNAP.03 distributions", {
  expect_plot2_snapshot("boxplot", plot2(iris, Species, Sepal.Length, type = "boxplot"))
  expect_plot2_snapshot("violin", plot2(iris, Species, Sepal.Length, type = "violin"))
  expect_plot2_snapshot("histogram", plot2(iris, Sepal.Length, type = "histogram"))
  expect_plot2_snapshot("jitter", plot2(iris, Species, Sepal.Length, type = "jitter", jitter_seed = 1))
})

test_that("SNAP.04 facets and datalabels", {
  expect_plot2_snapshot("facet", plot2(fx_facet, x, y, facet = facet))
  expect_plot2_snapshot("percent-labels", plot2(fx_pct, x, y, y.percent = TRUE))
  expect_plot2_snapshot("negative", plot2(fx_neg, x, y))
})

test_that("SNAP.05 special types", {
  expect_plot2_snapshot("dumbbell", plot2(admitted_patients, age_group, n(), ward, type = "dumbbell"))
  expect_plot2_snapshot("back-to-back", plot2(admitted_patients, age_group, n(), facet = gender, type = "back-to-back"))
  expect_plot2_snapshot("heatmap-matrix", plot2(cor(mtcars[, 1:5])))
  skip_if_not_installed("ggforce")
  expect_plot2_snapshot("sankey", plot2(admitted_patients, x = c(hospital, ward), category = gender, type = "sankey"))
})

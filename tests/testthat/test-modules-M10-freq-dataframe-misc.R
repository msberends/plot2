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

# R04 sheet M10: plot2.freq(), the data.frame wrapper, re-exports and data
# review_and_overhaul/reviews/R04_modules/M10_freq-dataframe-small-files.md

test_that("M10.01 freq object: frequency order, titles Item and Count", {
  skip_if_not_installed("cleaner")
  fq <- cleaner::freq(c("a", "b", "b", "c", "c", "c"))
  p <- expect_plot2(plot2(fq))
  expect_identical(axis_labels(p, "x"), c("c", "b", "a"))
  expect_labels(p, x = "Item", y = "Count")
})

test_that("M10.02 grouped data frames are ungrouped", {
  g <- dplyr::group_by(fx_cat, category)
  expect_identical(layer_df(plot2(g, x, y), 1)$y, layer_df(plot2(fx_cat, x, y), 1)$y)
})

test_that("M10.03 re-exports are exported and identical to their source", {
  re <- list(dplyr = c("%>%", "n", "n_distinct", "first", "last", "all_of", "any_of"),
             tidyselect = c("everything", "starts_with", "ends_with", "matches", "where"))
  for (pkg in names(re)) {
    for (f in re[[pkg]]) {
      expect_true(f %in% getNamespaceExports("plot2"), label = f)
      expect_identical(getExportedValue("plot2", f), getExportedValue(pkg, f), label = f)
    }
  }
})

test_that("M10.04 bundled data have the documented structure", {
  expect_s3_class(admitted_patients, "data.frame")
  expect_identical(dim(admitted_patients), c(250L, 7L))
  skip_if_not_installed("sf")
  expect_s3_class(netherlands, "sf")
  expect_identical(sf::st_crs(netherlands)$epsg, 4326L)
})

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

# validate_legend.position() (#157)

test_that("default: top for a discrete category, right for a numeric category", {
  expect_identical(legend_position(plot2(fx_cat_unique, x, y, category)), "top")
  expect_identical(legend_position(plot2(fx_numcat, x, y, value)), "right")
})

test_that("abbreviations, case and spaces are normalised", {
  given <- c("t", "R ", "bottom", "Left", "none")
  expected <- c("top", "right", "bottom", "left", "none")
  for (i in seq_along(given)) {
    expect_identical(legend_position(plot2(fx_cat_unique, x, y, category, legend.position = given[i])), expected[i],
                     label = paste0("legend.position = \"", given[i], "\""))
  }
})

test_that("NA hides the legend, NULL is automatic (#106)", {
  expect_identical(legend_position(plot2(fx_cat_unique, x, y, category, legend.position = NA)), "none")
  expect_identical(legend_position(plot2(fx_cat_unique, x, y, category, legend.position = NULL)), "top")
})

test_that("an invalid value errors and lists the allowed values", {
  expect_error(plot2(fx_cat_unique, x, y, category, legend.position = "middle"), "'top', 'right', 'bottom', 'left', 'inside', 'none'")
  expect_error(plot2(fx_cat_unique, x, y, category, legend.position = c(2, 0.5)), "two values between 0 and 1")
})

test_that("FALSE hides the legend (#62)", {
  expect_identical(legend_position(plot2(fx_cat_unique, x, y, category, legend.position = FALSE)), "none")
})

test_that("two numbers place the legend inside the plot (#62)", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, legend.position = c(0.9, 0.8)))
  expect_identical(legend_position(p), "inside")
  expect_equal(theme_el(p, "legend.position.inside"), c(0.9, 0.8))
})

test_that("a back-to-back plot honours the setting", {
  skip_if_not_installed("patchwork")
  p <- plot2(admitted_patients, age_group, n(), ward, facet = gender, type = "back-to-back", legend.position = "bottom")
  expect_identical(p$theme$legend.position, "bottom")
})

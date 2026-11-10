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

# Argument handling, missing-argument flags, title evaluation

test_that("an extra argument reaches the geom, with a message", {
  p <- expect_plot2_message(plot2(fx_xy, x, y, type = "point", alpha = 0.3), "additional argument.*alpha")
  expect_identical(layer_params(p, "point")$alpha, 0.3)
})

test_that("a misspelt argument is reported by plot2 and by ggplot2", {
  d <- plot2_diagnostics(plot2(fx_xy, x, y, type = "point", colr = "red"))
  expect_true(any(grepl("colr", d$messages$text)))
  expect_true(any(grepl("Ignoring unknown parameters", d$warnings)))
})

test_that("summarise_function must be a function", {
  expect_error(plot2(fx_cat, x, y, summarise_function = "mean"), "summarise_function")
})

test_that("summarise_function = FALSE keeps duplicates (#85)", {
  expect_identical(nrow(plot_data(plot2(fx_cat, x, y))), 3L)
  expect_identical(nrow(plot_data(plot2(fx_cat, x, y, summarise_function = FALSE))), 6L)
})

test_that("a title can be computed from the data", {
  expect_labels(plot2(fx_cat, x, y, title = paste("n =", n())), title = "n = 6")
})

test_that("category.title is used as legend title", {
  p <- plot2(fx_cat_unique, x, y, category, category.title = "Group")
  expect_labels(p, fill = "Group")
})

test_that("a title referring to a missing column does not stop the plot", {
  # plot2 warns that the title cannot be evaluated (an R warning since #26)
  p <- expect_plot2_message(plot2(fx_cat, x, y, title = nonexistent), "nonexistent", type = "warning")
  expect_true(label_text(p, "title") %in% c("", NA_character_))
})

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

# R04 sheet M03: plot2.matrix()
# review_and_overhaul/reviews/R04_modules/M03_matrix-method.md

test_that("M03.01 correlation matrix: tiles and datalabels, one tile per cell", {
  m <- cor(mtcars[, 1:3])
  p <- expect_plot2_message(plot2(m), "type = \"tile\"")
  expect_geoms(p, c("tile", "text"))
  expect_identical(nrow(layer_df(p, "tile")), 9L)
})

test_that("M03.02 correlation matrix: the diagonal is 1", {
  m <- cor(mtcars[, 1:3])
  d <- plot_data(plot2(m))
  diag_vals <- d[as.character(d$x) == as.character(d$y), , drop = FALSE]
  expect_equal(as.numeric(diag_vals[[setdiff(colnames(d), c("x", "y", "_var_datalabels"))[1]]]), c(1, 1, 1))
})

test_that("M03.03 plain matrix: row names on the x-axis", {
  m <- matrix(1:6, nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("A", "B")))
  expect_identical(axis_labels(expect_plot2(plot2(m)), "x"), c("r1", "r2", "r3"))
})

test_that("M03.04 plain matrix: all columns and axis titles (F089)", {
  skip_open_finding("F089")
  m <- matrix(1:6, nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("A", "B")))
  p <- plot2(m)
  expect_false(is.na(label_text(p, "x")))
  expect_identical(as.character(scale_breaks(p, "fill")), c("A", "B"))
})

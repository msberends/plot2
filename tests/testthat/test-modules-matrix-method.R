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

# plot2.matrix()

test_that("correlation matrix: tiles and datalabels, one tile per cell", {
  m <- cor(mtcars[, 1:3])
  p <- expect_plot2_message(plot2(m), "type = \"tile\"")
  expect_geoms(p, c("tile", "text"))
  expect_identical(nrow(layer_df(p, "tile")), 9L)
})

test_that("correlation matrix: the diagonal is 1", {
  m <- cor(mtcars[, 1:3])
  d <- plot_data(plot2(m))
  diag_vals <- d[as.character(d$x) == as.character(d$y), , drop = FALSE]
  expect_equal(as.numeric(diag_vals[[setdiff(colnames(d), c("x", "y", "datalabels"))[1]]]), c(1, 1, 1))
})

test_that("plain matrix: row names on the x-axis", {
  m <- matrix(1:6, nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("A", "B")))
  expect_identical(axis_labels(expect_plot2(plot2(m)), "x"), c("r1", "r2", "r3"))
})

test_that("plain matrix: all columns, and titles from named dimensions (#101)", {
  m <- matrix(1:6, nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("A", "B")))
  p <- expect_plot2(plot2(m))
  expect_identical(as.character(scale_breaks(p, "fill")), c("A", "B"))
  expect_identical(nrow(layer_df(p, "bar")), 6L)
  m <- matrix(1:6, nrow = 3, dimnames = list(region = c("r1", "r2", "r3"), year = c("2020", "2021")))
  p <- expect_plot2(plot2(m))
  expect_identical(label_text(p, "x"), "region")
  expect_identical(label_text(p, "fill"), "year")
})

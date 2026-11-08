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

# Regression tests for the findings fixed in Phase 5 of the review and overhaul
# (review_and_overhaul/plans/roadmap.md)

test_that("F035 the methods and plot2_exec() take their arguments from the generic", {
  generic <- formals(plot2)
  generic$data <- NULL
  for (m in c("plot2.default", "plot2.data.frame", "plot2.formula", "plot2.matrix", "plot2.sf", "plot2.freq", "plot2_exec")) {
    f <- formals(getFromNamespace(m, "plot2"))
    expect_identical(setdiff(names(generic), names(f)), character(0))
  }
  # method-specific defaults
  expect_identical(formals(plot2:::plot2.freq)$x.sort, "freq-desc")
  expect_identical(formals(plot2:::plot2.matrix)$x.title, FALSE)
  expect_true(all(c("crs", "datalabels.centroid") %in% names(formals(plot2:::plot2.sf))))
  # every method forwards each argument once
  cl <- as.list(plot2:::plot2_exec_call())
  expect_false(anyDuplicated(names(cl)[nzchar(names(cl))]) > 0)
})

test_that("F105 titles show the column name in functions and loops", {
  h <- function(v) plot2(iris, !!rlang::sym(v), n())
  expect_labels(h("Species"), x = "Species")
  ps <- lapply(c("Sepal.Length", "Sepal.Width"), function(v) plot2(iris, Species, .data[[v]]))
  expect_identical(vapply(ps, function(p) p$labels$y, ""), c("Sepal.Length", "Sepal.Width"))
  k <- function(df, col) plot2(df, Species, {{ col }}, y.title = TRUE)
  expect_labels(k(iris, Petal.Width), y = "Petal.Width")
  # in-line calculations keep their code as title
  expect_labels(plot2(iris, Species, mean(.data$Sepal.Length)), y = "mean(Sepal.Length)")
})

test_that("F034 the generic shows the defaults the methods use; matrix plots say that x, y and category are ignored", {
  expect_identical(formals(plot2)$y_secondary.colour, formals(plot2:::plot2.data.frame)$y_secondary.colour)
  expect_message(plot2(matrix(1:6, 2), x = 1), class = "plot2_caution")
})

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

# R06 sheet T04: plot2 inside functions, knitr, Quarto and Shiny
# review_and_overhaul/reviews/R06_themes/T04_embedding.md

f_embrace <- function(df, col) plot2(df, {{ col }}, n())
f_data <- function(df, v) plot2(df, .data[[v]], n())

test_that("T04.01 titles show the column name when plot2() is called from a function", {
  skip_open_finding("F105")
  expect_labels(f_embrace(iris, Species), x = "Species")
  expect_labels(f_data(iris, "Species"), x = "Species")
})

test_that("T04.02 the data are right when plot2() is called from a function", {
  expect_identical(as.character(axis_labels(f_embrace(iris, Species), "x")), levels(iris$Species))
  expect_identical(as.character(axis_labels(f_data(iris, "Species"), "x")), levels(iris$Species))
})

test_that("T04.03 local variables can be used in in-line calculations", {
  f <- function(df) { k <- 10; plot2(df, Species, mean(Sepal.Length) * k) }
  p <- f(iris)
  expect_equal(sort(layer_df(p, layer_index(p, "GeomBar"))$y), sort(tapply(iris$Sepal.Length, iris$Species, mean) * 10), ignore_attr = TRUE)
})

test_that("T04.04 a plot rendered after another plot2() call is unaffected (Shiny-like delayed rendering)", {
  a <- plot2(iris, Species, mean(Sepal.Length), title = "A")
  b <- plot2(mtcars, factor(cyl), mean(hp), title = "B", y.percent = TRUE)
  expect_identical(axis_labels(a, "y"), axis_labels(plot2(iris, Species, mean(Sepal.Length), title = "A"), "y"))
})

test_that("T04.05 knitting does not put plot2 notes into the document", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  rmd <- tempfile(fileext = ".Rmd")
  writeLines(c("```{r}", "p <- plot2::plot2(data.frame(x = c('a', 'a', 'b'), y = 1:3), x, y)", "```"), rmd)
  out <- knitr::knit(rmd, output = tempfile(fileext = ".md"), quiet = TRUE, envir = new.env())
  expect_false(any(grepl("plot2|Using|Assuming", setdiff(readLines(out), c("```r", "p <- plot2::plot2(data.frame(x = c('a', 'a', 'b'), y = 1:3), x, y)", "```")))))
})

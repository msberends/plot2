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

# Every automatic choice documented in the section 'Automatic choices' of ?plot2 (#48):
# one test per rule, checking both the choice and its message

msg <- function(expr) {
  out <- character(0)
  withCallingHandlers(expr, message = function(m) {
    out <<- c(out, trimws(cli::ansi_strip(conditionMessage(m))))
    invokeRestart("muffleMessage")
  })
  out
}
has_msg <- function(msgs, pattern) any(grepl(pattern, msgs))

test_that("A11.01 y: a column named n, otherwise the first numeric column; n() without numeric columns", {
  expect_true(has_msg(msg(plot2(data.frame(g = c("a", "b"), v = 1:2, n = 3:4), g)), "Using y = n$"))
  expect_true(has_msg(msg(plot2(data.frame(g = c("a", "b"), v = 1:2), g)), "Using y = v"))
  expect_true(has_msg(msg(plot2(data.frame(g = c("a", "b", "b")), g)), "Using y = n\\(\\) to count rows"))
})

test_that("A11.02 x: the first numeric column, or the first column not used elsewhere", {
  expect_true(has_msg(msg(plot2(data.frame(a = 1:3, b = 4:6))), "Using x = a"))
  expect_true(has_msg(msg(plot2(data.frame(g = c("a", "b"), v = 1:2), y = v)), "Using x = g"))
})

test_that("A11.03 category: the first character column if x and category are not given", {
  d <- data.frame(n = 1:4, g = c("a", "a", "b", "b"), h = c("p", "q", "p", "q"))
  expect_true(has_msg(msg(plot2(d)), "Using category = h"))
})

test_that("A11.04 type: boxplot, points, histogram or columns", {
  expect_true(has_msg(msg(plot2(iris, Species, Sepal.Length)), "Using type = \"boxplot\""))
  expect_true(has_msg(msg(plot2(iris, Sepal.Length, Sepal.Width)), "Using type = \"point\""))
  expect_true(has_msg(msg(plot2(data.frame(v = c(1, 2, 2, 3)))), "type = \"histogram\""))
  expect_true(has_msg(msg(plot2(cor(mtcars[, 1:3]))), "type = \"tile\""))
})

test_that("A11.05 x.character for years and months; x.date_remove_years", {
  expect_true(has_msg(msg(plot2(data.frame(yr = 2020:2022, v = 1:3), yr, v)), "x.character = TRUE.*years"))
  expect_true(has_msg(msg(plot2(data.frame(m = 1:12, v = 1:12), m, v)), "x.character = TRUE.*months"))
  d <- data.frame(dt = as.Date(c("2020-01-01", "2021-01-01")), v = 1:2)
  d$yr <- format(d$dt, "%Y")
  expect_true(has_msg(msg(plot2(d, dt, v, yr)), "x.date_remove_years = TRUE"))
})

test_that("A11.06 facet scales: fixed below 25% difference, free otherwise", {
  d <- data.frame(f = rep(c("a", "b"), each = 3), x = 1:6, y = c(1, 2, 3, 100, 200, 300))
  expect_true(has_msg(msg(plot2(d, x, y, facet = f, type = "point")), "Using free y scales"))
  d$y <- c(1, 2, 3, 1, 2, 3.1)
  expect_true(has_msg(msg(plot2(d, x, y, facet = f, type = "point")), "facet.fixed_y = TRUE"))
})

test_that("A11.07 expansion 0 with set limits; y.percent for densities; scientific notation", {
  expect_true(has_msg(msg(plot2(iris, Species, Sepal.Length, type = "col", y.limits = c(0, 400))), "y.expand = 0"))
  # the y labels are formatted when the plot is drawn, so the notice comes then
  expect_true(has_msg(msg(ggplot2::ggplot_build(plot2(data.frame(g = c("a", "b"), v = c(1e9, 2e9)), g, v))), "y.scientific = TRUE"))
  expect_false(has_msg(msg(ggplot2::ggplot_build(plot2(data.frame(g = c("a", "b"), v = c(1e6, 2e6)), g, v))), "y.scientific"))
})

test_that("A11.08 binwidth and smooth for histograms", {
  m <- msg(plot2(iris, Sepal.Length, type = "histogram"))
  expect_true(has_msg(m, "binwidth"))
  expect_true(has_msg(m, "smooth = TRUE"))
})

test_that("A11.09 datalabels: at most 25 automatically", {
  d <- data.frame(x = paste0("v", 1:30), y = 1:30)
  expect_true(has_msg(msg(plot2(d, x, y)), "Omitting printing of 30 datalabels"))
})

test_that("A11.10 legend position", {
  expect_identical(theme_el(plot2(iris, Sepal.Length, Sepal.Width, Petal.Length), "legend.position"), "right")
  expect_identical(theme_el(plot2(iris, Sepal.Length, Sepal.Width, Species), "legend.position"), "top")
})

test_that("A11.11 the help page lists the automatic choices", {
  rd <- paste(readLines(source_file("man", "plot2.Rd")), collapse = "\n")
  expect_match(rd, "section\\{Automatic choices\\}")
})

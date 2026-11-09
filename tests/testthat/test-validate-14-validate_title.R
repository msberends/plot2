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

# 14: validate_title() (#157)

long <- paste(rep("word", 30), collapse = " ")

test_that("14.01 literal title, subtitle, caption and tag are set", {
  p <- plot2(fx_xy, x, y, title = "T", subtitle = "S", caption = "C", tag = "A")
  expect_labels(p, title = "T", subtitle = "S", caption = "C", tag = "A")
})

test_that("14.02 a title is evaluated on the data", {
  expect_labels(plot2(fx_xy, x, y, title = paste("n =", n())), title = "n = 5")
})

test_that("14.03 FALSE, NULL and an empty string remove a title", {
  expect_identical(label_text(plot2(fx_xy, x, y, title = FALSE), "title"), NA_character_)
  expect_identical(label_text(plot2(fx_xy, x, y, x.title = NULL), "x"), NA_character_)
  expect_identical(label_text(plot2(fx_xy, x, y, x.title = ""), "x"), NA_character_)
  expect_identical(label_text(plot2(fx_xy, x, y, x.title = FALSE), "x"), NA_character_)
})

test_that("14.04 TRUE gives the column name, and the expression for y = n()", {
  expect_identical(label_text(plot2(fx_xy, x, y, x.title = TRUE), "x"), "x")
  expect_identical(label_text(plot2(fx_cat, x, n(), y.title = TRUE), "y"), "n()")
})

test_that("14.05 '>=' becomes a symbol with markdown, not without", {
  expect_identical(label_text(plot2(fx_xy, x, y, title = "a >= b"), "title"), "a ≥ b")
  expect_identical(label_text(plot2(fx_xy, x, y, title = "a >= b", markdown = FALSE), "title"), "a >= b")
})

test_that("14.06 long titles are wrapped at title.linelength, accented ones too (#68)", {
  plain <- label_text(plot2(fx_xy, x, y, title = long, title.linelength = 40), "title")
  accented <- label_text(plot2(fx_xy, x, y, title = gsub("o", "ó", long), title.linelength = 40), "title")
  expect_true(all(nchar(strsplit(plain, "\n")[[1]]) <= 40))
  expect_identical(lengths(strsplit(c(plain, accented), "\n")), c(4L, 4L))
})

test_that("14.07 a markdown title longer than the limit warns and is not wrapped", {
  expect_warning(p <- plot2(fx_xy, x, y, title = paste0("*", long, "*"), title.linelength = 40),
                 "Multiple lines in title cannot be set", class = "plot2_warning")
  expect_no_match(label_text(p, "title"), "\n")
})

test_that("14.08 an apostrophe in a markdown title is kept (#30)", {
  expect_identical(label_text(plot2(fx_xy, x, y, title = "Patient's *E. coli*"), "title"),
                   "paste(\"Patient's \", italic(\"E. coli\"))")
})

test_that("14.09 <br> gives a line break", {
  expect_identical(label_text(plot2(fx_xy, x, y, title = "line one<br>line two"), "title"), "line one\nline two")
})

test_that("14.10 a title that refers to a missing column warns, and the plot is still made", {
  expect_warning(p <- plot2(fx_xy, x, y, title = paste("max", max(nonexistent))), "nonexistent", class = "plot2_warning")
  expect_plot2(p)
})

test_that("14.11 an expression title is passed through", {
  expect_identical(label_text(plot2(fx_xy, x, y, title = expression(alpha^2)), "title"), "alpha^2")
})

test_that("14.12 a column takes precedence over an object of the same name in the calling environment", {
  # `date` is also a base R function, and `y` is a global object here
  df <- data.frame(x = 1:3, y = c(4, 5, 6), date = as.Date(c("2001-01-01", "2002-01-01", "2003-01-01")))
  y <- 1000
  p <- plot2(df, x, y,
             title = paste("From", min(format(date, "%Y"))),
             subtitle = paste("Maximum", max(y)))
  expect_identical(label_text(p, "title"), "From 2001")
  expect_identical(label_text(p, "subtitle"), "Maximum 6")
})

test_that("14.13 a title from an object in the calling environment is still used", {
  my_title <- "A title from a variable"
  my_expression <- expression(beta^2)
  expect_identical(label_text(plot2(fx_xy, x, y, title = my_title), "title"), "A title from a variable")
  expect_identical(label_text(plot2(fx_xy, x, y, title = my_expression), "title"), "beta^2")
})

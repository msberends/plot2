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

# 13: validate_markdown() (#157)

test_that("13.01 markdown = FALSE keeps the text literal", {
  p <- plot2(fx_xy, x, y, title = "*text*", markdown = FALSE)
  expect_identical(p$labels$title, "*text*")
})

test_that("13.02 by default, markdown in a title becomes an expression", {
  p <- plot2(fx_xy, x, y, title = "*E. coli*")
  expect_true(is.expression(p$labels$title))
  expect_identical(label_text(p, "title"), "paste(italic(\"E. coli\"))")
})

test_that("13.03 markdown = NULL with a superscript in the y name: guessed TRUE, with a message", {
  df <- data.frame(x = 1:3, `y^2` = 1:3, check.names = FALSE)
  expect_plot2_message(plot2(df, x, `y^2`, markdown = NULL), "Assuming markdown = TRUE")
  expect_identical(label_text(plot2(df, x, `y^2`, markdown = NULL), "y"), "paste(\"y\"^\"2\")")
})

test_that("13.04 markdown = NULL with one underscore in each of two titles is not guessed TRUE (#30)", {
  expect_no_plot2_message(plot2(fx_xy, x, y, title = "a_b", subtitle = "c_d", markdown = NULL), "Assuming markdown")
  expect_identical(plot2(fx_xy, x, y, title = "a_b", subtitle = "c_d", markdown = NULL)$labels$title, "a_b")
})

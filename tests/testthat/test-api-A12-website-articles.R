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

# A12: articles of the package website (vignettes/articles, not part of the package)

run_article <- function(name, exclude = "opts_chunk|Sys.setenv|install.packages") {
  withr::local_pdf(NULL)
  withr::local_options(plot2.colour = NULL, plot2.font = NULL, plot2.text_factor = NULL)
  withr::defer(unregister_colour("^company"))
  eval_chunks(source_file("vignettes", "articles", paste0(name, ".Rmd")), exclude = exclude)
}

test_that("A12.01 every chunk of 'Colours and themes' runs without error or warning", {
  skip_on_cran()
  res <- run_article("colours-and-themes")
  expect_identical(res$code[!is.na(res$error)], character(0))
  expect_identical(res$code[!is.na(res$warnings)], character(0))
})

test_that("A12.02 every chunk of 'Maps' runs without error or warning", {
  skip_on_cran()
  skip_if_not_installed("sf")
  res <- run_article("maps")
  expect_identical(res$code[!is.na(res$error)], character(0))
  expect_identical(res$code[!is.na(res$warnings)], character(0))
})

test_that("A12.03 every chunk of 'plot2 for ggplot2 users' runs without error or warning", {
  skip_on_cran()
  res <- run_article("plot2-for-ggplot2-users")
  expect_identical(res$code[!is.na(res$error)], character(0))
  expect_identical(res$code[!is.na(res$warnings)], character(0))
})

test_that("A12.04 every chunk of 'Fonts' runs without error, apart from the font download", {
  skip_on_cran()
  # the font chunks need showtext and network access, and fig.showtext in R Markdown
  res <- run_article("fonts", exclude = "opts_chunk|Sys.setenv|install.packages|font = |plot2.font")
  expect_identical(res$code[!is.na(res$error)], character(0))
})

test_that("A12.05 every article is listed in _pkgdown.yml", {
  articles <- tools::file_path_sans_ext(list.files(source_file("vignettes", "articles"), pattern = "[.]Rmd$"))
  yml <- readLines(source_file("_pkgdown.yml"))
  for (article in articles) {
    expect_true(any(grepl(paste0("articles/", article, "$"), yml)), label = article)
  }
})

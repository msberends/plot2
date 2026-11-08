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

# R05 sheet A10: README, NEWS, DESCRIPTION and pkgdown
# review_and_overhaul/reviews/R05_api/A10_readme-and-package-files.md

test_that("A10.01 the evaluated README chunks run without error or warning", {
  skip_on_cran()
  skip_if_not_installed("ggforce")
  withr::local_pdf(NULL)
  res <- eval_chunks(source_file("README.Rmd"), include_unevaluated = FALSE, exclude = "opts_chunk|install.packages")
  expect_identical(res$code[!is.na(res$error)], character(0))
  expect_identical(res$code[!is.na(res$warnings)], character(0))
})

test_that("A10.02 the README chunks marked eval = FALSE run without error or warning too", {
  skip_open_finding("F100")
  withr::local_pdf(NULL)
  res <- eval_chunks(source_file("README.Rmd"), exclude = "opts_chunk|install.packages|create_interactively")
  expect_identical(res$code[!is.na(res$error)], character(0))
  expect_identical(res$code[!is.na(res$warnings)], character(0))
})

test_that("A10.03 the pkgdown reference index lists every topic", {
  skip_on_cran()
  skip_if_not_installed("pkgdown")
  pkg <- dirname(source_file("_pkgdown.yml"))
  expect_no_error(suppressMessages(pkgdown::check_pkgdown(pkg)))
})

test_that("A10.04 NEWS.md mentions the user-visible features added since 1.0.0", {
  skip_open_finding("F104")
  news <- paste(readLines(source_file("NEWS.md")), collapse = " ")
  expect_true(all(vapply(c("spider", "coord_spider", "beeswarm"), grepl, logical(1), x = news, ignore.case = TRUE)))
})

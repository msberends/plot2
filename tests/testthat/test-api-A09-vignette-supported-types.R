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

# R05 sheet A09: vignette 'Supported Plot Types'
# review_and_overhaul/reviews/R05_api/A09_vignette-supported-types.md

test_that("A09.01 every chunk of the supported types vignette runs without error or warning", {
  skip_on_cran()
  for (pkg in c("ggplot2movies", "ggbeeswarm", "sf", "ggforce")) skip_if_not_installed(pkg)
  withr::local_pdf(NULL)
  withr::local_options(plot2.colour = NULL)
  res <- eval_chunks(source_file("vignettes", "supported_types.Rmd"))
  expect_identical(res$code[!is.na(res$error)], character(0))
  expect_identical(res$code[!is.na(res$warnings)], character(0))
})

test_that("A09.02 the vignette builds without its suggested data packages", {
  skip_open_finding("F102")
  src <- readLines(source_file("vignettes", "supported_types.Rmd"))
  expect_true(any(grepl("requireNamespace\\(\"ggplot2movies\"", src)))
})

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

# Vignette 'Supported Plot Types'

test_that("every chunk of the plot types vignette runs without error or warning", {
  skip_on_cran()
  for (pkg in c("ggbeeswarm", "sf", "ggforce", "patchwork")) skip_if_not_installed(pkg)
  withr::local_pdf(NULL)
  withr::local_options(plot2.colour = NULL)
  res <- eval_chunks(source_file("vignettes", "supported_types.Rmd"), exclude = "opts_chunk")
  expect_identical(res$code[!is.na(res$error)], character(0))
  expect_identical(res$code[!is.na(res$warnings)], character(0))
})

test_that("the plot types vignette lists every additional type of plot2()", {
  src <- paste(readLines(source_file("vignettes", "supported_types.Rmd")), collapse = "\n")
  for (type in c("barpercent", "dumbbell", "linedot", "back-to-back", "upset", "sankey", "spider", "beeswarm")) {
    expect_true(grepl(paste0("`\"", type, "\"`"), src), label = type)
  }
})

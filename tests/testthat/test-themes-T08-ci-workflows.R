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

# T08: CI workflows

test_that("T08.01 the CI matrix includes the declared minimum R version", {
  wf <- paste(readLines(source_file(".github", "workflows", "check.yaml")), collapse = "\n")
  min_r <- sub(".*>= *([0-9]+\\.[0-9]+).*", "\\1", read.dcf(source_file("DESCRIPTION"))[, "Depends"])
  expect_true(grepl(paste0("r: '", min_r), wf, fixed = TRUE))
})

test_that("T08.02 CI has a job without suggested packages", {
  wf <- paste(unlist(lapply(list.files(source_file(".github", "workflows"), full.names = TRUE), readLines)), collapse = "\n")
  expect_true(grepl("dependencies: .*hard|_R_CHECK_FORCE_SUGGESTS_", wf))
})

test_that("T08.03 workflow comments do not refer to files that do not exist", {
  wf <- unlist(lapply(list.files(source_file(".github", "workflows"), full.names = TRUE), readLines))
  refs <- unique(regmatches(wf, regexpr("R/[A-Za-z0-9_.-]+\\.R", wf)))
  expect_true(all(file.exists(file.path(dirname(source_file("DESCRIPTION")), refs))))
})

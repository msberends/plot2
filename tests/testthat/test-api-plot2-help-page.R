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

# plot2() description, details and examples

run_rd_examples <- function(rd, exclude = NULL) {
  ex <- tempfile(fileext = ".R")
  tools::Rd2ex(rd, ex, commentDontrun = TRUE)
  # one chunk per top-level expression, so that `exclude` drops single examples
  exprs <- vapply(parse(ex, keep.source = FALSE), function(x) paste(deparse(x), collapse = "\n"), "")
  if (!is.null(exclude)) exprs <- exprs[!grepl(exclude, exprs)]
  rmd <- tempfile(fileext = ".Rmd")
  writeLines(unlist(lapply(exprs, function(e) c("```{r}", e, "```"))), rmd)
  eval_chunks(rmd)
}

test_that("every example of ?plot2 runs without error or warning (font example excluded)", {
  skip_on_cran()
  withr::local_options(plot2.colour = NULL, plot2.colour_sf_fill = NULL)
  withr::local_pdf(NULL)
  skip_if_not_installed("sf")
  res <- run_rd_examples(source_file("man", "plot2.Rd"), exclude = "font = ")
  expect_identical(res$code[!is.na(res$error)], character(0))
  expect_identical(res$code[!is.na(res$warnings) & !grepl("mbcsToSbcs", res$warnings)], character(0))
})

test_that("the details state the actual number of arguments", {
  rd <- paste(readLines(source_file("man", "plot2.Rd")), collapse = " ")
  n <- as.integer(sub(".*currently has ([0-9]+) arguments.*", "\\1", rd))
  expect_identical(n, length(formals(plot2)))
})

test_that("the x.transform list in the help page names valid transformations", {
  rd <- paste(readLines(source_file("man", "plot2.Rd")), collapse = " ")
  expect_true(grepl("log2", rd))
  expect_geoms(plot2(iris, Sepal.Length, Sepal.Width, x.transform = "log2"), "point")
})

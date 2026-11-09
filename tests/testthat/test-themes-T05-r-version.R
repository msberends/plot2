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

# T05: support of the declared minimum R version


test_that("T05.01 R/ uses no base R function newer than the declared minimum R version", {
  fns <- c("%||%", "grepv", "sort_by", "Tailcall", "Exec", "array2DF", "Sys.setLanguage", "chkDots")
  files <- list.files(source_file("R"), full.names = TRUE)
  used <- unique(unlist(lapply(files, function(f) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    pd$text[pd$token %in% c("SYMBOL_FUNCTION_CALL", "SPECIAL")]
  })))
  defined <- ls(asNamespace("plot2"), all.names = TRUE)
  expect_identical(setdiff(intersect(used, fns), defined), character(0))
})

test_that("T05.02 the declared minimum R version is at least what the hard dependencies require", {
  desc <- read.dcf(source_file("DESCRIPTION"))
  ours <- package_version(sub(".*>= *([0-9.]+).*", "\\1", desc[, "Depends"]))
  imports <- trimws(sub("\\(.*", "", strsplit(gsub("\n", " ", desc[, "Imports"]), ",")[[1]]))
  req <- vapply(imports, function(p) {
    d <- utils::packageDescription(p)$Depends
    m <- regmatches(d %||% "", regexpr("R \\(>= [0-9.]+\\)", d %||% ""))
    if (length(m)) gsub("[^0-9.]", "", m) else "0.0"
  }, "")
  expect_true(all(package_version(req) <= ours))
})

test_that("T05.03 no suggested package requires a newer R than the declared minimum", {
  desc <- read.dcf(source_file("DESCRIPTION"))
  expect_false(grepl("grid \\(>= 4\\.5", desc[, "Suggests"]))
})

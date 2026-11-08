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

# R06 sheet T06: dependency footprint
# review_and_overhaul/reviews/R06_themes/T06_dependencies.md

desc_deps <- function(field) {
  desc <- read.dcf(source_file("DESCRIPTION"))
  trimws(sub("\\(.*", "", strsplit(gsub("\n", " ", desc[, field]), ",")[[1]]))
}

test_that("T06.01 every package imported in NAMESPACE is listed in Imports", {
  skip_open_finding("F108")
  ns <- readLines(source_file("NAMESPACE"))
  imported <- unique(sub("^import(From)?\\(([^,)]+).*", "\\2", grep("^import(From)?\\(", ns, value = TRUE)))
  expect_identical(setdiff(imported, desc_deps("Imports")), character(0))
})

test_that("T06.02 packages called on the default code path are in Imports", {
  skip_open_finding("F008")
  expect_true(all(c("cli", "rlang", "scales", "stringr") %in% desc_deps("Imports")))
})

test_that("T06.03 the Shiny-only packages are not needed outside the Shiny app", {
  shiny_only <- c("arrow", "bslib", "clipr", "DT", "fansi", "haven", "jsonlite", "nanoparquet", "pillar",
                  "readODS", "readxl", "rio", "shiny", "shinyjs", "xml2", "yaml")
  files <- setdiff(list.files(source_file("R"), full.names = TRUE), source_file("R", "shiny.R"))
  src <- unlist(lapply(files, readLines))
  src <- src[!grepl("^\\s*#", src)]
  hits <- shiny_only[vapply(shiny_only, function(p) any(grepl(paste0("\\b", p, "::"), src)), logical(1))]
  expect_identical(hits, character(0))
})

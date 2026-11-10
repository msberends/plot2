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

# T06: dependency footprint

desc_deps <- function(field) {
  desc <- read.dcf(source_file("DESCRIPTION"))
  trimws(sub("\\(.*", "", strsplit(gsub("\n", " ", desc[, field]), ",")[[1]]))
}

test_that("T06.01 every package imported in NAMESPACE is listed in Imports", {
  ns <- readLines(source_file("NAMESPACE"))
  imported <- unique(sub("^import(From)?\\(([^,)]+).*", "\\2", grep("^import(From)?\\(", ns, value = TRUE)))
  # base packages (e.g. grid, grDevices) need not be listed, R CMD check does not require them
  base_pkgs <- rownames(utils::installed.packages(priority = "base"))
  expect_identical(setdiff(imported, c(desc_deps("Imports"), base_pkgs)), character(0))
})

test_that("T06.02 packages called on the default code path are in Imports", {
  expect_true(all(c("cli", "rlang", "scales") %in% desc_deps("Imports")))
  # stringr is no longer used: sorting uses the internal natural_sort() (#20)
  src <- unlist(lapply(list.files(source_file("R"), full.names = TRUE), readLines))
  expect_false(any(grepl("stringr::", src[!grepl("^\\s*#", src)])))
})

test_that("T06.02b natural_sort() sorts like stringr::str_sort(numeric = TRUE)", {
  skip_if_not_installed("stringr")
  cases <- list(c("a10", "a2", "a1", "b", "B", "a"), c("item 10", "item 9", "Item 1", "item 1"),
                c("10", "9", "100", "1a", "1b", "a1"), c("abc", "Abc", "aBc", "ABC", "abd"),
                c("Ward 3", "Ward 12", "ward 2", "ICU"), c("a", NA, "b"), c("Q1 2024", "Q10 2023", "Q2 2023"))
  for (x in cases) {
    expect_identical(plot2:::natural_sort(x), stringr::str_sort(x, numeric = TRUE))
    expect_identical(plot2:::natural_sort(x, decreasing = TRUE), stringr::str_sort(x, numeric = TRUE, decreasing = TRUE))
  }
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

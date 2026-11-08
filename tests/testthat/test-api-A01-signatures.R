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

# R05 sheet A01: API surface and signatures
# review_and_overhaul/reviews/R05_api/A01_api-surface.md

rd_arguments <- function(rd_file) {
  rd <- tools::parse_Rd(rd_file)
  args <- rd[vapply(rd, attr, "", "Rd_tag") == "\\arguments"][[1]]
  items <- args[vapply(args, attr, "", "Rd_tag") == "\\item"]
  nms <- vapply(items, function(i) paste(unlist(i[[1]]), collapse = ""), "")
  trimws(unlist(strsplit(gsub("\\\\dots", "...", nms), ",")))
}

test_that("A01.01 every argument of the generic is documented on its help page", {
  documented <- rd_arguments(source_file("man", "plot2.Rd"))
  expect_setequal(setdiff(names(formals(plot2)), documented), character(0))
})

test_that("A01.02 every method accepts every argument of the generic; only the formula method takes `data` (F020)", {
  for (m in c("plot2.default", "plot2.data.frame", "plot2.formula", "plot2.matrix", "plot2.sf", "plot2.freq")) {
    expected <- if (m == "plot2.formula") character(0) else "data"
    expect_setequal(setdiff(names(formals(plot2)), names(formals(getFromNamespace(m, "plot2")))), expected)
  }
})

test_that("A01.03 methods use the defaults shown in the generic's usage", {
  g <- formals(plot2)
  f <- formals(plot2:::plot2.data.frame)
  common <- intersect(names(g), names(f))
  expect_identical(common[!mapply(identical, g[common], f[common])], character(0))
})

test_that("A01.04 every exported function has a help page with examples", {
  db <- tools::Rd_db(dir = source_file())
  has_ex <- vapply(db, function(rd) any(vapply(rd, attr, "", "Rd_tag") == "\\examples"), logical(1))
  aliases <- lapply(db, function(rd) unlist(rd[vapply(rd, attr, "", "Rd_tag") == "\\alias"]))
  exported <- setdiff(getNamespaceExports("plot2"), c("%>%", "all_of", "any_of", "ends_with", "everything", "first", "last", "matches", "n", "n_distinct", "starts_with", "where", "CoordSpider"))
  without <- exported[!vapply(exported, function(e) any(has_ex[vapply(aliases, function(a) e %in% a, logical(1))]), logical(1))]
  expect_identical(sort(without), character(0))
})

test_that("A01.05 passing the default shown in the usage gives the same plot as omitting it", {
  skip_open_finding("F094")
  d <- data.frame(x = paste0("v", 1:30), y = 1:30)
  expect_identical(length(datalabels_of(plot2(d, x, y, datalabels = TRUE))),
                   length(datalabels_of(plot2(d, x, y))))
})

test_that("A01.06 help pages are reproducible: no random or session-dependent text in roxygen", {
  skip_open_finding("F103")
  src <- readLines(source_file("R", "get_colour.R"))
  expect_false(any(grepl("^#'.*(sample\\(|R\\.version)", src)))
})

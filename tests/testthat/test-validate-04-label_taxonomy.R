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

# 04: validate_taxonomy(), replaced by label_taxonomy() (#70) (#157)

bugs <- data.frame(name = c("Escherichia coli", "E. coli", "Other", "Klebsiella pneumoniae"),
                   n = c(25, 18, 6, 12),
                   stringsAsFactors = FALSE)

test_that("04.01 a full species name is italic", {
  skip_if_not_installed("AMR")
  expect_identical(deparse(label_taxonomy("Escherichia coli")[[1]]), "paste(italic(\"Escherichia coli\"))")
})

test_that("04.02 an abbreviated genus with its species is italic as a whole", {
  skip_if_not_installed("AMR")
  expect_identical(deparse(label_taxonomy("E. coli")[[1]]), "paste(italic(\"E. coli\"))")
})

test_that("04.03 mixed labels: only the taxonomic names are italic", {
  skip_if_not_installed("AMR")
  lbl <- label_taxonomy(c("Other", "Klebsiella pneumoniae", "group coli"))
  expect_identical(deparse(lbl[[1]]), "paste(\"Other\")")
  expect_match(deparse(lbl[[2]]), "italic")
  # an ordinary word that is also an epithet stays upright (#70)
  expect_no_match(deparse(lbl[[3]]), "italic")
  # nothing taxonomic: plain character
  expect_identical(label_taxonomy(c("a", "b")), c("a", "b"))
})

test_that("04.04 sorting is kept when the labels are italicised", {
  skip_if_not_installed("AMR")
  p <- expect_plot2(plot2(bugs, name, n, x.sort = "freq-desc", x.labels = label_taxonomy))
  lbl <- axis_labels(p, "x")
  expect_identical(gsub("paste\\(|italic\\(|\\)|\"", "", lbl), c("Escherichia coli", "E. coli", "Klebsiella pneumoniae", "Other"))
  expect_identical(grepl("italic", lbl), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("04.05 a factor with unused levels does not error", {
  skip_if_not_installed("AMR")
  df <- bugs
  df$name <- factor(df$name, levels = c(df$name, "Staphylococcus aureus"))
  expect_plot2(plot2(df, name, n, x.labels = label_taxonomy))
})

test_that("04.06 label_taxonomy() requires AMR", {
  expect_match(deparse(body(label_taxonomy))[2], "check_installed(\"AMR\"", fixed = TRUE)
})

test_that("04.07 the removed x.lbl_taxonomy argument points to x.labels = label_taxonomy", {
  expect_error(plot2(bugs, name, n, x.lbl_taxonomy = TRUE), "use `x.labels = label_taxonomy` instead", fixed = TRUE)
})

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

# R03 sheet E14: restoring the mapping and returning the plot
# review_and_overhaul/reviews/R03_plot2_exec/E14_restore-mapping-and-return.md

all_mapping_text <- function(p) {
  p <- plot_of(p)
  maps <- c(list(p$mapping), lapply(p$layers, function(l) l$mapping))
  unlist(lapply(maps, function(m) vapply(m, rlang::as_label, character(1))))
}

test_that("E14.01 no mapping refers to internal direction names", {
  # datalabel layers map `_var_datalabels`, which is F069 (see E14.03)
  for (p in list(plot2(fx_cat_unique, x, y, category),
                 plot2(fx_facet, x, y, facet = facet),
                 plot2(data.frame(x = 1:30, y = sin(1:30 / 5)), x, y, smooth = TRUE))) {
    m <- all_mapping_text(p)
    expect_false(any(grepl("_var_", m[!grepl("label$", names(m))])))
  }
})

test_that("E14.02 p$data has no internal direction columns", {
  p <- plot2(fx_facet, x, y, facet = facet)
  expect_false(any(c("_var_x", "_var_y", "_var_category", "_var_facet") %in% colnames(p$data)))
  expect_true(all(c("x", "y", "facet") %in% colnames(p$data)))
})

test_that("E14.03 no internal columns or mappings at all (F069)", {
  skip_open_finding("F069")
  p <- plot2(fx_counts, group, n)
  expect_false(any(startsWith(colnames(p$data), "_")))
  expect_false(any(grepl("_var_", all_mapping_text(p))))
})

test_that("E14.04 p$data is a tibble", {
  expect_s3_class(plot2(fx_counts, group, n)$data, "tbl_df")
})

test_that("E14.05 users can add layers that refer to the original columns", {
  expect_plot2(plot2(fx_cat_unique, x, y, category) + ggplot2::geom_text(ggplot2::aes(label = category)))
})

test_that("E14.06 facet parameters refer to the original facet name", {
  expect_identical(names(plot2(fx_facet, x, y, facet = facet)$facet$params$facets), "facet")
})

test_that("E14.07 print = TRUE prints and returns the plot invisibly", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  res <- withVisible(plot2(fx_counts, group, n, print = TRUE))
  expect_false(res$visible)
  expect_s3_class(res$value, "gg")
})

test_that("E14.08 two identical calls give identical layer data", {
  p1 <- plot2(fx_cat_unique, x, y, category, facet = x)
  p2 <- plot2(fx_cat_unique, x, y, category, facet = x)
  expect_identical(layer_df(p1, 1), layer_df(p2, 1))
})

test_that("E14.09 several types leave no internal state behind", {
  local_known_deprecation("F078")
  df_smooth <- data.frame(x = 1:30, y = sin(1:30 / 5) + 1:30 / 10)
  expect_clean_state(plot2(fx_counts, group, n), check = c("state", "locale"))
  expect_clean_state(plot2(df_smooth, x, y, smooth = TRUE), check = c("state", "locale"))
  expect_clean_state(plot2(fx_cat_unique, x, y, category, type = "line"), check = c("state", "locale"))
  expect_clean_state(plot2(iris, Species, Sepal.Length), check = c("state", "locale"))
  expect_clean_state(plot2(fx_facet, x, y, facet = facet, type = "area"), check = c("state", "locale"))
})

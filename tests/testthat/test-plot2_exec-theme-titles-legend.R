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

# theme, titles and legend

guide_param <- function(p, aes, param) {
  plot_of(p)$guides$guides[[aes]]$params[[param]]
}

test_that("title, subtitle, caption and tag", {
  p <- expect_plot2(plot2(fx_counts, group, n, title = "T", subtitle = "S", caption = "C", tag = "A"))
  expect_labels(p, title = "T", subtitle = "S", caption = "C", tag = "A")
})

test_that("axis titles can be removed or replaced", {
  p <- plot2(fx_counts, group, n, x.title = FALSE, y.title = NULL)
  expect_true(label_text(p, "x") %in% c("", NA_character_))
  expect_true(label_text(p, "y") %in% c("", NA_character_))
  expect_labels(plot2(fx_counts, group, n, x.title = "Group", y.title = "Count"), x = "Group", y = "Count")
})

test_that("legend title: automatic for numeric categories, none for discrete", {
  expect_identical(label_text(plot2(fx_numcat, x, y, value, type = "point"), "colour"), "value")
  expect_identical(label_text(plot2(fx_cat_unique, x, y, category), "fill"), NA_character_)
})

test_that("category.title is used for colour and fill", {
  p <- plot2(fx_cat_unique, x, y, category, category.title = "Group")
  expect_labels(p, colour = "Group", fill = "Group")
})

test_that("legend position: top for discrete, right for numeric, explicit honoured", {
  expect_identical(legend_position(plot2(fx_cat_unique, x, y, category)), "top")
  expect_identical(legend_position(plot2(fx_numcat, x, y, value, type = "point")), "right")
  expect_identical(legend_position(plot2(fx_cat_unique, x, y, category, legend.position = "bottom")), "bottom")
})

test_that("legend.reverse = TRUE reverses the legend", {
  expect_true(isTRUE(guide_param(plot2(fx_cat_unique, x, y, category, legend.reverse = TRUE), "fill", "reverse")))
  expect_false(isTRUE(guide_param(plot2(fx_cat_unique, x, y, category), "fill", "reverse")))
})

test_that("horizontal plot with legend on top: legend reversed to match the bars", {
  expect_true(isTRUE(guide_param(plot2(fx_cat_unique, x, y, category, horizontal = TRUE), "fill", "reverse")))
})

test_that("legend.nrow reaches the guide", {
  expect_identical(as.numeric(guide_param(plot2(fx_cat_unique, x, y, category, legend.nrow = 2), "fill", "nrow")), 2)
})

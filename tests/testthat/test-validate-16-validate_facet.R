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

# I002, R02 sheet 16: validate_facet()
# review_and_overhaul/reviews/R02_validate/16_validate_facet.md

free <- function(p) unlist(facet_info(p)[c("free_x", "free_y")])

test_that("16.01 facets with different maxima: free x and free y, with a message (F052)", {
  expect_identical(free(plot2(fx_facet, x, y, facet = facet)), c(free_x = TRUE, free_y = TRUE))
  expect_plot2_message(plot2(fx_facet, x, y, facet = facet), "facet.fixed_y")
})

test_that("16.02 facets with similar maxima: fixed y without repeated labels, with messages", {
  expect_plot2_message(plot2(fx_facet_similar, x, y, facet = facet), "Assuming facet.fixed_y = TRUE")
  expect_plot2_message(plot2(fx_facet_similar, x, y, facet = facet), "Assuming facet.repeat_labels_y = FALSE")
  expect_identical(free(plot2(fx_facet_similar, x, y, facet = facet))[["free_y"]], FALSE)
})

test_that("16.03 facet.fixed_y = TRUE gives identical y ranges", {
  b <- built(plot2(fx_facet, x, y, facet = facet, facet.fixed_y = TRUE))
  ranges <- lapply(b$layout$panel_params, function(pp) pp$y.range)
  expect_identical(ranges[[1]], ranges[[2]])
})

test_that("16.04 facet.repeat_labels_x = FALSE shares the x scale", {
  expect_identical(free(plot2(fx_facet, x, y, facet = facet, facet.repeat_labels_x = FALSE))[["free_x"]], FALSE)
})

test_that("16.05 facet.nrow sets the number of rows", {
  expect_identical(facet_info(plot2(fx_facet, x, y, facet = facet, facet.nrow = 2))$nrow, 2L)
})

test_that("16.06 facet.position moves the strips", {
  p <- plot2(fx_facet, x, y, facet = facet, facet.position = "bottom")
  expect_identical(p$facet$params$strip.position, "bottom")
})

test_that("16.07 facet.relative = TRUE: panel widths follow the number of x values; with rows it warns", {
  d <- data.frame(x = c("a", "b", "c", "d", "a"), y = 1:5, f = c("p", "p", "p", "p", "q"))
  p <- plot2(d, x, y, facet = f, facet.relative = TRUE)
  expect_s3_class(p$facet, "FacetGrid")
  expect_true(p$facet$params$space_free$x)
  expect_warning(plot2(d, x, y, facet = f, facet.relative = TRUE, facet.nrow = 2), "facet.relative = TRUE", class = "plot2_warning")
})

test_that("16.08 NA in the facet: the plot builds with a panel for the missing values", {
  p <- suppressWarnings(plot2(fx_na, x, y, facet = facet, na.replace = "(missing)"))
  expect_identical(as.character(built(p)$layout$layout$facet), c("(missing)", "f1", "f2"))
})

test_that("16.09 horizontal plots swap the scale freedom", {
  expect_identical(free(plot2(fx_facet, x, y, facet = facet, facet.repeat_labels_x = FALSE, horizontal = TRUE)),
                   c(free_x = TRUE, free_y = FALSE))
})

test_that("16.10 facet.drop drops empty factor levels", {
  d <- fx_facet
  d$facet <- factor(d$facet, levels = c("f1", "f2", "f3"))
  expect_identical(facet_info(plot2(d, x, y, facet = facet, facet.drop = TRUE))$n_panels, 2L)
  # the empty panel gives R warnings when built (F139, test 16.10b)
  expect_identical(suppressWarnings(facet_info(plot2(d, x, y, facet = facet, facet.drop = FALSE)))$n_panels, 3L)
})

test_that("16.10b an empty panel renders without R warnings (F139)", {
  skip_open_finding("F139")
  d <- fx_facet
  d$facet <- factor(d$facet, levels = c("f1", "f2", "f3"))
  expect_plot2(plot2(d, x, y, facet = facet, facet.drop = FALSE))
})

test_that("16.11 an sf plot with a facet has fixed scales", {
  skip_if_not_installed("sf")
  nl <- netherlands
  nl$grp <- rep(c("a", "b"), length.out = nrow(nl))
  p <- expect_plot2(plot2(nl, facet = grp))
  expect_identical(free(p), c(free_x = FALSE, free_y = FALSE))
})

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

# T01: ggplot2 4.x internals and compatibility

test_that("T01.01 plots of all main types build and render without warnings", {
  skip_if_not_installed("patchwork")
  withr::local_pdf(NULL)
  plots <- list(plot2(fx_counts, group, n), plot2(iris, Sepal.Length, Sepal.Width), plot2(iris, Species, Sepal.Length),
                plot2(admitted_patients, age, type = "hist"), plot2(admitted_patients, age_group, n(), facet = ward, type = "b2b"),
                plot2(data.frame(x = c("a", "b", "c"), y = c(3, 5, 4)), x, y, type = "spider"))
  for (p in plots) expect_no_warning(ggplot2::ggplotGrob(p))
})

test_that("T01.02 titles are available through ggplot2's public get_labs()", {
  p <- plot2(fx_counts, group, n, title = "T", x.title = "X")
  labs <- ggplot2::get_labs(p)
  expect_identical(labs$title, "T")
  expect_identical(labs$x, "X")
})

test_that("T01.03 plot2 does not edit facet internals (p$facet$params)", {
  src <- unlist(lapply(list.files(source_file("R"), full.names = TRUE), readLines))
  expect_false(any(grepl("\\$facet\\$params", src[!grepl("^\\s*#", src)])))
})

test_that("T01.04 coord_spider() is added with + through ggplot_add", {
  p <- ggplot2::ggplot(data.frame(m = c("a", "b", "c"), v = c(1, 2, 3)), ggplot2::aes(m, v, group = 1)) +
    ggplot2::geom_polygon() + coord_spider()
  expect_identical(coord_class(p), "CoordSpider")
})

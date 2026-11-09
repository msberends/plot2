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

# Legacy smoke tests from the former test_plot2.R (F038), split by topic. They
# mostly check the class and are skipped on CRAN; the content tests are in the
# test-plot2_exec-*, test-modules-*, test-api-* and test-validate-* files.

test_that("max items and sorting work", {
  skip_on_cran()
  expect_equal(admitted_patients |>
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "asc") |> 
                 get_range_x(),
               c("A", "B", "C", "D"))
  expect_equal(admitted_patients |>
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "desc") |> 
                 get_range_x(),
               c("D", "C", "B", "A"))
  expect_equal(admitted_patients |>
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "freq-desc") |> 
                 get_range_x(),
               c("D", "B", "A", "C"))
  expect_equal(admitted_patients |>
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "freq-asc") |> 
                 get_range_x(),
               c("A", "C", "B", "D"))
  expect_equal(admitted_patients |>
                 plot2(x = hospital,
                       y = n(),
                       x.sort = "asc") |> 
                 get_range_x(),
               c("A", "B", "C", "D"))
  expect_equal(admitted_patients |>
                 plot2(x = format(date, "%Y"),
                       y = n(),
                       x.sort = "freq-desc",
                       x.max_items = 5) |>
                 get_range_x(),
               c("2010", "2003", "2017", "2016", "(rest, x12)"))
  
  expect_s3_class(admitted_patients |>
                    plot2(x = format(date, "%Y"), y = n(), category = hospital, facet = age_group,
                          x.max_items = 2, category.max_items = 2, facet.max_items = 2),
                  "gg")
  expect_equal(admitted_patients |>
                 plot2(gender, n(), hospital,
                       category.sort = c("A", "C", "D", "B"),
                       x.sort = c("M", "F")) |> 
                 get_range_x(),
               c("M", "F"))
  expect_equal(admitted_patients |>
                 plot2(gender, n(), hospital,
                       category.sort = c("A", "C", "D", "B"),
                       horizontal = TRUE,
                       x.sort = c("M", "F")) |> 
                 get_range_x(),
               c("F", "M"))
})

test_that("x scale works", {
  skip_on_cran()
  expect_s3_class(plotdata |> plot2(x = x_date), "gg")
  plotdata |> plot2(x = x_date, x.limits = c(Sys.Date() - 13, Sys.Date() + 2))
  expect_s3_class(plotdata |> plot2(x = x_char), "gg")
  expect_s3_class(plotdata |> plot2(n, type = "hist"), "gg")
  expect_s3_class(plotdata |> plot2(n, type = "density"), "gg")
  expect_s3_class(plotdata |> plot2(n, type = "jitter"), "gg")
  expect_s3_class(plotdata |> plot2(type = "line", category = NULL), "gg")
  expect_s3_class(plotdata |> plot2(type = "barpercent"), "gg")
  expect_s3_class(plotdata |> plot2(type = "linedot", category = NULL), "gg")
  expect_s3_class(plotdata |> plot2(x.transform = "log2"), "gg")
  expect_s3_class(mtcars |> plot2(mpg, hp, x.label_angle = 40), "gg")
  expect_s3_class(mtcars |> plot2(mpg, hp, x.label_angle = 200), "gg")
  expect_s3_class(runif(n = 100, min = 2.0004, max = 2.0006) |> plot2(type = "h"), "gg")
  expect_s3_class(suppressWarnings(mtcars |> plot2(mpg, hp, x.limits = c(10, 20))), "gg")
  
  # mics <- AMR::as.mic(c(1, 2, 8, 32))
  # # should print missing factors levels:
  # expect_equal(mics |> plot2(x.mic = TRUE) |> get_range_x(),
  #              c("<=1", "2", "8", ">=32"))
  # expect_equal(mics |> plot2(x.mic = TRUE, x.limits = c(2, 8)) |> get_range_x(),
  #              c("<=2", ">=8"))
  
  
  p <- plotdata |>
    plot2(x = x_date,
          x.limits = c(Sys.Date() - 13,
                       Sys.Date() + 2))
  expect_equal(p |> get_range_x() |> as.Date(origin = "1970-01-01"),
               c(Sys.Date() - 13 - 1, Sys.Date() + 2 + 1))
  
  plotdata2 <- data.frame(x = factor(1:30), y = rnorm(30, 30, 5), z = rep(letters[1:10], 3))
  expect_identical(plotdata2 |> plot2(facet = z, facet.fixed_x = TRUE) |> get_range_x(),
                   plotdata2 |> plot2(facet = z, x.drop = FALSE) |> get_range_x())
  expect_identical(plotdata2 |> plot2(facet = z, facet.fixed_x = FALSE) |> get_range_x(),
                   plotdata2 |> plot2(facet = z, x.drop = TRUE) |> get_range_x())
  
  # removed years on X-axis
  expect_s3_class(
    data.frame(date = seq(as.Date("2021-01-01"), as.Date("2023-12-31"), "1 day"),
               value = c(rep(1, 365), rep(2, 365), rep(3, 365))) |>
      plot2(date, value,
            type = "line",
            category = format(date, "%Y"),
            category.title = "year",
            linewidth = 2,
            x.date_remove_years = TRUE),
    "gg")
})

test_that("y scale works", {
  skip_on_cran()
  expect_error(data.frame(a = 1:10, b = letters[10]) |> plot2(x = a, y = b))
  expect_s3_class(plotdata |> plot2(y = n * 24, y.24h = TRUE), "gg")
  expect_s3_class(plotdata |> plot2(y = n * 12, y.age = TRUE), "gg")
  expect_s3_class(plotdata |> plot2(y = n * 10, y.scientific = TRUE), "gg")
  expect_s3_class(plotdata |> plot2(y.percent = TRUE), "gg")
  expect_s3_class(data.frame(a = letters[1:10], y = 10 ^ 1:10) |> plot2(), "gg")
  expect_s3_class(data.frame(a = letters[1:10], b = 10 ^ 1:10) |> plot2(y.transform = "log10", y.n_breaks = 10), "gg")
  expect_s3_class(data.frame(a = letters[1:10], b = 10 ^ 1:10) |> plot2(y.transform = "log10", y.n_breaks = 10), "gg")
  expect_s3_class(data.frame(a = letters[1:10], y = 1) |> plot2(y.percent = TRUE, y.percent_break = 500), "gg")
  expect_s3_class(data.frame(a = letters[1:10], y = 1) |> plot2(y.percent = TRUE), "gg")
  expect_s3_class(suppressWarnings(mtcars |> plot2(mpg, hp, y.limits = c(100, 200))), "gg")
  
  # multiple vars of y
  expect_s3_class(data.frame(x = letters[1:10],
                             y1 = 1:10,
                             y2 = 11:20) |>
                    plot2(x, c(y1, y2)),
                  "gg")
  expect_error(data.frame(x = letters[1:10],
                          y1 = 1:10,
                          y2 = 11:20) |>
                 plot2(x, c(y1, y2),
                       # category must not be set
                       category = 1))
  expect_s3_class(admitted_patients |>
                    plot2(x = hospital,
                          y = quantile(age, c(0.25, 0.5, 0.75))),
                  "gg")
})

test_that("category scale works", {
  skip_on_cran()
  # set as numeric
  expect_s3_class(plotdata[1:4, ] |> plot2(x = x_char, y = 1, category = n), "gg")
  # 2-colour scale
  expect_true(all(c(fill = "Petal.Length", colour = "Petal.Length") %in%
                    (plot2(iris, Sepal.Length, Sepal.Width, Petal.Length,
                           colour = c("red", "blue")) |>
                       get_mapping())))
  # 3-colour scale
  expect_true(all(c(fill = "Petal.Length", colour = "Petal.Length") %in%
                    (plot2(iris, Sepal.Length, Sepal.Width, Petal.Length,
                           colour = c("red", "blue", "green")) |>
                       get_mapping())))
  # multi-colour scale
  expect_true(all(c(fill = "Petal.Length", colour = "Petal.Length") %in%
                    (plot2(iris, Sepal.Length, Sepal.Width, Petal.Length,
                           colour = c("red", "blue", "green", "yellow")) |>
                       get_mapping())))
  expect_true(all(c(fill = "Petal.Length", colour = "Petal.Length") %in%
                    (plot2(iris, Sepal.Length, Sepal.Width, Petal.Length,
                           colour = "viridis") |>
                       get_mapping())))
  expect_s3_class(mtcars |> plot2(mpg, hp, as.character(cyl), category.focus = 2), "gg")
  expect_s3_class(mtcars |> plot2(mpg, hp, as.character(cyl), category.focus = "4"), "gg")
  # adding white to geoplot if only one colour set
  skip_if_not_installed("sf")
  expect_s3_class(netherlands |> plot2(colour_fill = "red"), "gg")
  expect_s3_class(netherlands |> plot2(colour_fill = "red", 
                                       category.transform = "log10",
                                       category.limits = c(NA, 10e3)),
                  "gg")
  # date class as category
  expect_s3_class(admitted_patients |>
                    plot2(hospital,
                          median(age),
                          date),
                  "gg")
  expect_s3_class(admitted_patients |>
                    plot2(hospital,
                          median(age),
                          date,
                          category.date_breaks = "5 years",
                          category.date_labels = "'yy"),
                  "gg")
})

test_that("facet scale works", {
  skip_on_cran()
  expect_s3_class(iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                Species), "gg")
  expect_s3_class(iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                Species, facet.relative = TRUE), "gg")
  expect_warning(p <- iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                     Species, facet.relative = TRUE, facet.nrow = 2),
                 "facet.relative", class = "plot2_warning")
  expect_s3_class(p, "gg")
  expect_s3_class(iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                Species,
                                facet.repeat_labels_x = TRUE,
                                facet.repeat_labels_y = FALSE), "gg")
  
  expect_s3_class(iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                Species,
                                facet.repeat_labels_x = FALSE,
                                facet.repeat_labels_y = TRUE), "gg")
  expect_s3_class(admitted_patients |> plot2(hospital,
                                             n(),
                                             ward,
                                             gender,
                                             stacked = TRUE,
                                             facet.fixed_y = TRUE), "gg")
})

test_that("secondary Y-axis works", {
  skip_on_cran()
  expect_s3_class(mtcars |> plot2(mpg, hp, y_secondary = disp), "gg")
  expect_s3_class(mtcars |> plot2(mpg, hp, y_secondary = disp ^ 2), "gg")
  
  # this function is being used to determine breaks, so checks its functionality
  p <- mtcars |> plot2(mpg, hp)
  expect_identical(ggplot_build(p)$layout$panel_params[[1]]$y$breaks,
                   as.double(c(0, 100, 200, 300, 400, NA)))
})

test_that("date labels work", {
  skip_on_cran()
  expect_equal(determine_date_breaks_labels(c(as.POSIXct("2023-01-01 10:00:00"), as.POSIXct("2023-01-01 10:50:00"))),
               list(breaks = "10 min", labels = "HH:MM"))
  expect_equal(determine_date_breaks_labels(c(as.POSIXct("2023-01-01 10:00:00"), as.POSIXct("2023-01-01 15:00:00"))),
               list(breaks = "1 hour", labels = "HH:MM"))
  expect_equal(determine_date_breaks_labels(c(as.POSIXct("2023-01-01 10:00:00"), as.POSIXct("2023-01-02 10:00:00"))),
               list(breaks = "2 hours", labels = "HH"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2023-01-31"))),
               list(breaks = "1 day", labels = "d mmm"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2023-02-28"))),
               list(breaks = "4 days", labels = "d mmm"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2023-06-30"))),
               list(breaks = "2 weeks", labels = "d mmm"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2023-12-31"))),
               list(breaks = "1 month", labels = "mmm"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-03-01"), as.Date("2024-01-31"))),
               list(breaks = "2 months", labels = "mmm yyyy"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2024-12-31"))),
               list(breaks = "3 months", labels = "mmm yyyy"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2025-12-31"))),
               list(breaks = "6 months", labels = "mmm yyyy"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2026-12-31"))),
               list(breaks = "1 year", labels = "mmm yyyy"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2027-12-31"))),
               list(breaks = "1 year", labels = "mmm yyyy"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2030-12-31"))),
               list(breaks = "1 year", labels = "yyyy"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2035-12-31"))),
               list(breaks = "2 years", labels = "yyyy"))
  expect_equal(determine_date_breaks_labels(c(as.Date("2023-01-01"), as.Date("2050-12-31"))),
               list(breaks = "5 years", labels = "yyyy"))
})

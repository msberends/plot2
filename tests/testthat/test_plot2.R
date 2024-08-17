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

plotdata <- data.frame(x = seq_len(8) + 10,
                       x_char = letters[1:8],
                       x_date = Sys.Date() - seq_len(8),
                       n = seq_len(8),
                       stringsAsFactors = FALSE)

`%or%` <- function(a, b) if (is.null(a)) b else a

get_mapping <- function(plot) gsub("~", "", sapply(plot$mapping, deparse))
get_layers <- function(plot) plot$layers
get_labels <- function(plot) unlist(plot$labels)
get_data <- function(plot) plot$data
get_range_x <- function(plot) {
  ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]$limits %or% 
    ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range
}
get_range_y <- function(plot) {
  ggplot2::ggplot_build(plot)$layout$panel_scales_y[[1]]$limits %or%
    ggplot2::ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
}

test_that("general types work", {
  expect_s3_class(plot2(rnorm(10, 10)), "gg")
  expect_s3_class(plot2(rnorm(10, 10), type = "l"), "gg")
  expect_s3_class(plot2(mtcars, mpg^2, hp^2), "gg")
  expect_s3_class(plot2(mtcars, mpg^2, hp^2, smooth = TRUE), "gg")
  expect_s3_class(iris |> plot2(Species), "gg")
  expect_s3_class(iris |> plot2(Species, type = "violin"), "gg")
  expect_s3_class(iris |> plot2(Species, type = "violin"), "gg")
  expect_s3_class(iris |> plot2(Species, type = "blank"), "gg")
  expect_s3_class(iris |> plot2(Species, type = "area"), "gg")
  expect_warning(iris |> plot2(Species, type = "dotplot"))
  # difftime coercion to double:
  expect_s3_class(data.frame(x = letters[1:10],
                             y = difftime(Sys.time(), Sys.time() - seq_len(10))) |>
                    plot2(),
                  "gg")
  expect_s3_class(admitted_patients |> plot2(hospital, n(), where(is.character)), "gg")
  expect_s3_class(admitted_patients |> plot2(hospital, n(), c(gender, ward)), "gg")
  
  expect_s3_class(plotdata |> plot2(y.transform = "log2"), "gg")
  # dumbbell:
  expect_s3_class(admitted_patients |> plot2(age_group, n_distinct(patient_id), ward, type = "d"), "gg")
  expect_s3_class(admitted_patients |> plot2(age_group, n_distinct(patient_id), ward, gender, type = "d"), "gg")
  # sankey:
  expect_s3_class(admitted_patients |>
                    plot2(x = c(hospital, ward),
                          y = n_distinct(patient_id),
                          category = age_group,
                          facet = gender,
                          type = "sankey"),
                  "gg")
})

test_that("na.rm works", {
  # as characters
  df <- data.frame(hp = mtcars$hp,
                   letters = letters[seq_len(nrow(mtcars))],
                   stringsAsFactors = FALSE)
  expect_lt(df |> plot2(na.rm = TRUE) |> get_data() |> nrow(), nrow(df))
  expect_lt(df |> plot2(na.rm = FALSE) |> get_data() |> nrow(), nrow(df))
  # as factors
  df <- data.frame(hp = mtcars$hp,
                   letters = letters[seq_len(nrow(mtcars))],
                   stringsAsFactors = TRUE)
  expect_lt(df |> plot2(na.rm = TRUE) |> get_data() |> nrow(), nrow(df))
  expect_lt(df |> plot2(na.rm = FALSE) |> get_data() |> nrow(), nrow(df))
})


test_that("infinite values are removed", {
  expect_equal(data.frame(x = letters[1:10],
                          y123 = c(1:9, Inf)) |>
                 plot2() |>
                 get_data() |> 
                 nrow(),
               9)
})

test_that("S3 implementations work", {
  # lm
  expect_s3_class(lm(mpg ~ hp, mtcars) |> plot2(), "gg")
  # freq
  freq <- cleaner::freq(admitted_patients$hospital)
  expect_s3_class(freq |> plot2(), "gg")
  # sf
  expect_s3_class(netherlands |> plot2(), "gg")
  expect_s3_class(netherlands |> plot2(crs = 28992, theme = theme_minimal2()), "gg")
  # bug_drug_combinations
  expect_s3_class(AMR::example_isolates |>
                    dplyr::select(mo, CIP, AMC) |>
                    AMR::bug_drug_combinations(FUN = AMR::mo_gramstain) |>
                    plot2(),
                  "gg")
  # sir_df
  expect_s3_class(AMR::example_isolates |>
                    dplyr::select(date, NIT, CIP, IPM, MEM) |>
                    dplyr::group_by(yr = format(date, "%Y")) |>
                    AMR::sir_df() |>
                    dplyr::filter(yr >= 2015) |>
                    plot2(),
                  "gg")
  # type should become boxplot here
  expect_s3_class(admitted_patients |> plot2(x = hospital, y = (age - mean(age)) / sd(age)), "gg")
  # this uses the format2_scientific function for the y axis
  expect_s3_class(admitted_patients |>
                    plot2(format(date, "%Y"),
                          (age - mean(age)) / sd(age),
                          hospital,
                          y.scientific = TRUE),
                  "gg")
  expect_s3_class(netherlands |> plot2(markdown = TRUE), "gg")
})

test_that("general mapping works", {
  expect_equal(plotdata |> plot2() |> get_mapping() |> names(),
               c("y", "x", "fill", "colour"))
  # remove x axis
  expect_s3_class(admitted_patients |> plot2(x = NULL, y = age), "gg")
  expect_s3_class(admitted_patients |> plot2(x = 1:250, y = age), "gg")
  
  # set category.labels = md_to_expression automatically
  expect_s3_class(iris |> plot2(category = paste0("*", Species, "*")), "gg")
})

test_that("adding mapping works", {
  p <- iris |> plot2(Sepal.Length, Sepal.Width)
  expect_length(p$mapping, 3)
  p2 <- p |> add_mapping(shape = Species)
  expect_length(p2$mapping, 4)
  expect_s3_class(p2, "gg")
})

test_that("adding types works", {
  
  expect_length(mtcars |> plot2(mpg, hp, cyl) |> get_layers(), 1)
  expect_length(mtcars |> plot2(mpg, hp, cyl) |> add_line() |> get_layers(), 2)
  expect_length(mtcars |> plot2(mpg, hp, cyl) |> add_point(shape = 4, size = 5) |> get_layers(), 2)
  expect_length(mtcars |> plot2(mpg, hp, cyl) |> add_col() |> get_layers(), 2)
  expect_error(mtcars |> plot2(mpg, hp, cyl) |> add_type(type = NULL))
  
  p <- data.frame(x = 1:100,
                  y = rnorm(100, 100, 25)) |> 
    plot2()
  expect_length(p |> get_layers(), 1)
  expect_length(p |> 
                  add_line(mean(y)) |>
                  get_layers(), 2)
  expect_length(p |>
                  add_line(mean(y)) |>
                  add_col(y / 5,
                          colour = "black",
                          colour_fill = "yellow",
                          width = 0.25) |>
                  get_layers(), 3)
})

test_that("titles work", {
  expect_true(all(c(caption = "caption",
                    tag = "tag",
                    subtitle = "subtitle",
                    title = "title",
                    y = "y.title",
                    x = "x.title")
                  %in% (mtcars |>
                          plot2(caption = "caption",
                                tag = "tag",
                                subtitle = "subtitle",
                                title = "title",
                                y.title = "y.title",
                                x.title = "x.title") |>
                          get_labels())))
})

test_that("max items and sorting work", {
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
  expect_s3_class(mtcars |> plot2(mpg, hp, x.lbl_angle = 40), "gg")
  expect_s3_class(mtcars |> plot2(mpg, hp, x.lbl_angle = 200), "gg")
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
  
  # removed years on x axis
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
  expect_s3_class(iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                Species), "gg")
  expect_s3_class(iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                Species, facet.relative = TRUE), "gg")
  expect_s3_class(iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                Species, facet.relative = TRUE, facet.nrow = 2), "gg")
  expect_s3_class(iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                Species,
                                facet.repeat_lbls_x = TRUE,
                                facet.repeat_lbls_y = FALSE), "gg")
  
  expect_s3_class(iris |> plot2(as.integer(Sepal.Length), Sepal.Width, Petal.Length,
                                Species,
                                facet.repeat_lbls_x = FALSE,
                                facet.repeat_lbls_y = TRUE), "gg")
  expect_s3_class(admitted_patients |> plot2(hospital,
                                             n(),
                                             ward,
                                             gender,
                                             stacked = TRUE,
                                             facet.fixed_y = TRUE), "gg")
})

test_that("blank plot works", {
  expect_s3_class(plotdata |> subset(n < 0) |> plot2(), "gg")
  expect_s3_class(data.frame() |> plot2(type = "blank",
                                        x.title = "test",
                                        y.title = "test",
                                        title = "test",
                                        subtitle = "test",
                                        tag = "test",
                                        caption = "test"), "gg")
})

test_that("misc elements works", {
  expect_s3_class(plotdata |> plot2(x_char, x.lbl_taxonomy = TRUE), "gg")
  expect_s3_class(AMR::example_isolates |>
                    cleaner::freq(AMR::mo_name(mo, "nl")) |>
                    plot2(type = "barpercent", x.lbl_taxonomy = TRUE), "gg")
})

test_that("completing data works", {
  expect_s3_class(data.frame(x = c(1, 5, 6, 9),
                             y = 10) |>
                    plot2(type = "l", x.complete = 5),
                  "gg")
})

test_that("get title works", {
  p <- plot2(mtcars, title = "Plotting **mpg** vs. **cyl**!")
  expect_equal(get_plot_title(p), "plotting_mpg_vs_cyl")
  expect_equal(get_plot_title(p, valid_filename = FALSE), "Plotting mpg vs. cyl!")
  expect_equal(get_plot_title(plot2(mtcars)), "cyl_per_mpg")
  expect_equal(get_plot_title(plot2(mtcars, title = NULL), default = "test"), "test")
})

test_that("type validation works", {
  library(dplyr, warn.conflicts = FALSE)
  df <- tibble(x = letters[1:10],
               y = 1:10,
               z = LETTERS[1:10],
               `_var_x` = x,
               `_var_y` = y,
               `_var_category` = z)
  expect_equal(validate_type(NULL, df), "geom_col")
  expect_equal(validate_type(NULL, df |> select(-x, -`_var_x`)), "geom_boxplot")
  expect_equal(validate_type("a", df), "geom_area")
  expect_equal(validate_type("b", df), "geom_boxplot")
  expect_equal(validate_type("c", df), "geom_col")
  expect_equal(validate_type("h", df), "geom_histogram")
  expect_equal(validate_type("j", df), "geom_jitter")
  expect_equal(validate_type("l", df), "geom_line")
  expect_equal(validate_type("p", df), "geom_point")
  expect_equal(validate_type("r", df), "geom_ribbon")
  expect_equal(validate_type("v", df), "geom_violin")
})

test_that("moving layer works", {
  expect_s3_class((mtcars |>
                     plot2(mpg, hp, cyl) +
                     geom_line(colour = "grey75")) |>
                    move_layer(-1), "gg")
})

test_that("messaging works", {
  expect_message(plot2_message("test", print = TRUE))
  expect_message(plot2_caution("test", print = TRUE))
  expect_warning(plot2_warning("test", print = TRUE))
})

test_that("date labels work", {
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

test_that("manual fonts work", {
  expect_s3_class(mtcars |> plot2(mpg, hp, font = "Rock Salt"), "gg")
  expect_s3_class(mtcars |> plot2(mpg, hp, font = "Rock Salt"), "gg") # already downloaded
  expect_s3_class(mtcars |> plot2(mpg, hp, font = "Courier"), "gg")
  expect_s3_class(mtcars |> plot2(mpg, hp, font = "Courier"), "gg") # already downloaded
})

test_that("Plotly works", {
  expect_error(as_plotly(mtcars))
  expect_s3_class(mtcars |> plot2(mpg, hp) |> as_plotly(), "plotly")
  expect_s3_class(mtcars |>
                    plot2(mpg, hp) |> 
                    as_plotly(dragmode = "pan") |>
                    plotly_style(marker.line.color = "red",
                                 hoverinfo = "y"),
                  "plotly")
  expect_s3_class(mtcars |> plot2(mpg, hp) |> plotly_style(hoverinfo = "y"), "plotly")
})

test_that("md to expression works", {
  expr1 <- md_to_expression("test1 *test2* **test3** ***test4*** _test5_ test6 **_test7_** _**test8**_ test_{9} test<sub>10</sub> test^{11} test<sup>12</sup>")
  expect_true(is.expression(expr1))
  expect_identical(as.character(expr1),
                   as.character(parse(text = "paste(\"test1 \", italic(\"test2\"), \" \", bold(\"test3\"), \" \", bolditalic(\"test4\"), \" _test5_ test6 \", bold(\"_test7_\"), \" _\", bold(\"test8\"), \"_ \", test[\"9\"], \" \", test[\"10\"], \" test\"^\"11\", \" \", test^\"12\")")))
  
  expr2 <- md_to_expression("test $alpha$")
  expect_true(is.expression(expr2))
  
  expr3 <- md_to_expression("$f[X](x)==frac(1, sigma*sqrt(2*pi))*plain(e)^{frac(-(x-mu)^2, 2*sigma^2)}$")
  expect_true(is.expression(expr3))
  
  expect_error(md_to_expression("test $**$"))
})

test_that("secondary y axis works", {
  expect_s3_class(mtcars |> plot2(mpg, hp, y_secondary = disp), "gg")
  expect_s3_class(mtcars |> plot2(mpg, hp, y_secondary = disp ^ 2), "gg")
  
  # this function is being used to determine breaks, so checks its functionality
  p <- mtcars |> plot2(mpg, hp)
  expect_identical(ggplot_build(p)$layout$panel_params[[1]]$y$breaks,
                   as.double(c(0, 100, 200, 300, 400, NA)))
})

test_that("matrices works", {
  expect_s3_class(mtcars |>
                    stats::cor() |>
                    plot2(), "gg")
  expect_s3_class(mtcars |>
                    stats::cor() |>
                    plot2(colour = c("blue4", "white", "red4"),
                          category.limits = c(-1, 1)), "gg")
})

test_that("errorbars work", {
  expect_s3_class(plotdata |>
                    dplyr::mutate(error1 = n * 0.9, error2 = n * 1.1) |> 
                    plot2(type = "c", colour = "pink") |>
                    add_errorbar(error1, error2), "gg")
})

test_that("aes works", {
  expect_identical(
    aes(x = some_var, y = `as.character(some_var)`),
    setup_aes(x = "some_var", y = "as.character(some_var)"))
})

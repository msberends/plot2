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

# Creates man/figures/plot_types.png: one panel per plot type, each with the single
# line of code that creates it (#4). Run from the package root after changes to the
# plot types, with the packages patchwork, ggforce, ggbeeswarm and sf installed:
#   source("data-raw/plot_types_overview.R")

devtools::load_all(".", quiet = TRUE)
library(patchwork)

options(plot2.silent = TRUE)

upset_data <- transform(mtcars,
                        manual = am == 1,
                        straight = vs == 1,
                        gears_4 = gear == 4,
                        mpg_20 = mpg > 20)

# each element: the plot type as title, and the code that creates the plot
panels <- list(
  "Column" = quote(plot2(admitted_patients, hospital, n())),
  "Bar" = quote(plot2(admitted_patients, hospital, n(), ward, type = "bar")),
  "Bar with percentages" = quote(plot2(ggplot2::mpg, manufacturer, type = "barpercent")),
  "Stacked to 100%" = quote(plot2(admitted_patients, age_group, n(), hospital, stacked_fill = TRUE)),
  "Line" = quote(plot2(admitted_patients, format(date, "%Y"), n(), ward, type = "line")),
  "Area" = quote(plot2(admitted_patients, format(date, "%Y"), n(), hospital, type = "area", stacked = TRUE, x.complete = 0)),
  "Point" = quote(plot2(iris)),
  "Beeswarm" = quote(plot2(iris, Species, Sepal.Length, type = "beeswarm")),
  "Boxplot" = quote(plot2(admitted_patients, hospital, age, gender)),
  "Violin" = quote(plot2(admitted_patients, ward, age, type = "violin")),
  "Histogram" = quote(plot2(admitted_patients, age, type = "histogram")),
  "Density" = quote(plot2(iris, Sepal.Length, category = Species, type = "density", colour_transparency = 0.5)),
  "Dumbbell" = quote(plot2(admitted_patients, hospital, median(age), gender, type = "dumbbell")),
  "Heatmap" = quote(plot2(cor(mtcars), colour = c("#2166AC", "white", "#B2182B"))),
  "Sankey" = quote(plot2(Titanic, c(Class, Age, Survived), category = Sex, type = "sankey")),
  "Spider" = quote(plot2(admitted_patients, age_group, n(), hospital, type = "spider")),
  "UpSet" = quote(plot2(upset_data, c(manual, straight, gears_4, mpg_20), type = "upset")),
  "Back-to-back" = quote(plot2(admitted_patients, age_group, n(), facet = gender, type = "back-to-back")),
  "Map" = quote(plot2(netherlands, datalabels = FALSE))
)

code_text <- function(expr) {
  txt <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
  # wrap long calls, so that the code fits the panel
  paste(strwrap(txt, width = 44, exdent = 2), collapse = "\n")
}

inner_theme <- ggplot2::theme(
  legend.text = ggplot2::element_text(size = 7),
  legend.title = ggplot2::element_blank(),
  axis.title = ggplot2::element_blank()
)

make_panel <- function(name, expr) {
  p <- eval(expr)
  if (inherits(p, "patchwork")) {
    # plot types made of several plots (back-to-back and UpSet)
    p <- p & inner_theme
  } else {
    p <- p + inner_theme
  }
  # every panel is wrapped as one element, so that the axes of the panels are not aligned with each other
  patchwork::wrap_elements(panel = p) +
    ggplot2::labs(title = name, subtitle = code_text(expr)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 8, family = "mono", colour = "grey30", hjust = 0, lineheight = 1.1),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}

plots <- Map(make_panel, names(panels), panels)

overview <- patchwork::wrap_plots(plots, ncol = 4) +
  patchwork::plot_annotation(
    title = "Plot types of plot2()",
    subtitle = "Each plot is created by the single line of code shown with it.",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"),
                           plot.subtitle = ggplot2::element_text(size = 12, colour = "grey30"))
  )

ggplot2::ggsave("man/figures/plot_types.png",
                overview,
                width = 16,
                height = 19,
                dpi = 72,
                bg = "white")

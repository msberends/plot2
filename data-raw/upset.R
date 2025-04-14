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

create_upset_plot <- function(df) {
  
  df_original <- utils::read.csv("~/Downloads/Randomized_Dataset.csv") |>
    mutate_all(as.logical)
  
  df_original$f <- runif(100)
  
  fn <- mean
  
  df_count <- df_original |>
    group_by(across(a:e)) |>
    reframe(n = n(),
            out = fn(f)) |>
    # we must not yet use tibble here
    as.data.frame()
  
  label_data <- df_original |>
    summarise(across(a:e, function(x) sum(x, na.rm = TRUE))) |>
    pivot_longer(a:e, names_to = "y", values_to = "n") |>
    arrange(desc(n))
  
  x_sorted <- rownames(df_count)[order(df_count$n, decreasing = TRUE)]
  y_sorted <- label_data$y
  
  df_grid <- expand.grid(x = seq_len(nrow(unique(df_count))),
                         y = colnames(df_count)) |>
    filter(!y %in% c("out", "n")) |>
    mutate(value = as.logical(df_count[cbind(x, match(y, colnames(df_count)))]),
           x = as.factor(x)) |>
    as_tibble()
  
  line_data <- df_grid %>%
    filter(value == TRUE, y %in% y_sorted) |>
    group_by(x) %>%
    summarise(y_min = min(match(y, y_sorted)),
              y_max = max(match(y, y_sorted))) %>%
    mutate(y_min_label = y_sorted[y_min],
           y_max_label = y_sorted[y_max])
  
  upper_left <- ggplot(data.frame(x = 3, y = 2), aes(x, y)) +
    geom_text(aes(label = "Function text `fn`"), angle = 90) +
    scale_x_continuous(limits = c(0, 3)) +
    scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank())
  
  upper_right <- df_count |>
    plot2(x = factor(unique(df_grid$x), levels = x_sorted, ordered = TRUE),
          y = out,
          type = "col",
          width = 0.5,
          x.title = NULL,
          y.title = NULL,
          x.remove = TRUE,
          title = "Title",
          subtitle = "SUBTITLE") +
    theme(axis.ticks.x = element_blank(),
          axis.line = element_blank())
  
  lower_left <- label_data |>
    plot2(x = y,
          y = n,
          type = "col",
          width = 0.33,
          x.sort = rev(y_sorted),
          horizontal = TRUE,
          datalabels = FALSE,
          x.title = NULL,
          y.title = NULL,
          x.remove = TRUE) +
    scale_y_continuous(transform = "reverse",
                       # name = "Set Size",
                       expand = expansion(mult = c(0.25, 0))) +
    scale_x_discrete(expand = c(0.05, 0.05)) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(0, 0, 0, 0),
          axis.ticks = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank())
  
  lower_right <- ggplot(df_grid,
                        aes(x = factor(x, levels = x_sorted, ordered = TRUE),
                            y = factor(y, levels = y_sorted, ordered = TRUE))) +
    geom_point(data = filter(df_grid, !value), colour = "grey90", size = 3) +
    geom_point(data = filter(df_grid, value), colour = upper_right$layers[[1]]$aes_params$colour, size = 3) +
    geom_segment(data = line_data,
                 aes(x = x, xend = x, y = y_min_label, yend = y_max_label),
                 colour = upper_right$layers[[1]]$aes_params$colour,
                 linewidth = 0.75,
                 inherit.aes = FALSE) +
    scale_y_discrete(expand = c(0.05, 0.05)) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(0, 0, 0, 0),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(hjust = 0.5),
          axis.title = element_blank())
  
  patchwork::wrap_plots(
    upper_left,
    upper_right,
    lower_left,
    lower_right,
    ncol = 2,
    nrow = 2,
    widths = c(0.2, 0.8),
    heights = c(0.7, 0.3)
  )
    
  
}

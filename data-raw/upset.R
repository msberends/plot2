
library(ggplot2)
library(dplyr)
library(tidyr)

df_original <- utils::read.csv("~/Downloads/Randomized_Dataset.csv") |>
  mutate_all(as.logical)

df_count <- df_original |>
  count(across(everything())) |>
  # we must not yet use tibble here
  as.data.frame()

label_data <- df_original |>
  summarise(across(everything(), function(x) sum(x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "y", values_to = "n") |>
  arrange(desc(n))

x_sorted <- rownames(df_count)[order(df_count$n, decreasing = TRUE)]
y_sorted <- label_data$y

df_grid <- expand.grid(x = seq_len(nrow(unique(df_count))),
                       y = colnames(df_count)) |>
  filter(y != "n") |>
  mutate(value = as.logical(df_count[cbind(x, match(y, colnames(df_count)))]),
         x = as.factor(x)) |>
  as_tibble()

line_data <- df_grid %>%
  filter(value == TRUE) |>
  group_by(x) %>%
  summarise(y_min = min(match(y, y_sorted)),
            y_max = max(match(y, y_sorted))) %>%
  mutate(y_min_label = y_sorted[y_min],
         y_max_label = y_sorted[y_max])



upper_left <- ggplot(data.frame(x = 3, y = 2), aes(x, y)) +
  geom_text(aes(label = "Intersection Size"), angle = 90) +
  scale_x_continuous(limits = c(0, 3)) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank())

upper_right <- ggplot(df_count,
                      aes(x = factor(unique(df_grid$x), levels = x_sorted, ordered = TRUE),
                          y = n)) +
  geom_col(width = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank())

lower_left <- label_data |>
  ggplot(aes(x = n,
             y = factor(y, levels = y_sorted, ordered = TRUE))) +
  geom_col(width = 0.5) +
  scale_x_continuous(transform = "reverse", name = "Set Size", expand = c(0,0)) +
  scale_y_discrete(expand = c(0.05, 0.05)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line())

lower_right <- ggplot(df_grid,
                      aes(x = factor(x, levels = x_sorted, ordered = TRUE),
                          y = factor(y, levels = y_sorted, ordered = TRUE))) +
  geom_point(data = filter(df_grid, !value), colour = "grey85", size = 3) +
  geom_point(data = filter(df_grid, value), colour = "black", size = 3) +
  geom_segment(data = line_data,
               aes(x = x, xend = x, y = y_min_label, yend = y_max_label),
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
  widths = c(0.25, 0.75),
  heights = c(0.7, 0.3)
)

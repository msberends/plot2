re <- admitted_patients |>
  plot2(age_group, n(), horizontal = TRUE, datalabels = FALSE, x.title = NULL, category = ward, facet = 456,
        y.title = "test", title = NULL, y.limits = c(0, 120), y.n_breaks = 10) +
  theme(axis.text.y = element_text(hjust = 0.5, margin = margin(l= 10, r = 10)),
        plot.margin = margin(5,5,5,0),
        legend.position = "none")

li <- admitted_patients |>
  plot2(age_group, n(), 
        horizontal = T, datalabels = FALSE, x.title = NULL, y.transform = "reverse", category = ward, facet = 123,
        y.title = "test", title = NULL, y.limits = c(0, 120), y.n_breaks = 10) +
  scale_x_discrete(position = "top") + # move ticks to other side
  theme(axis.text.y = element_blank(),
        plot.margin = margin(5,5,5,5),
        legend.position = "none")

patchwork::wrap_plots(li, re, nrow = 1, guides = "keep", axis_titles = "collect") + 
  patchwork::plot_annotation(title = md_to_expression("This *test*"), theme = theme_minimal2())


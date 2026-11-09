# E11 Theme, titles and legend

`R/plot2.R:2034-2141`

## Role

Adds the theme (`validate_theme()`), titles, axis titles (histogram exception), legend titles per category aesthetic, legend position (top for discrete, right for numeric), legend order and number of rows.

## Findings

F005, F070.

## End-to-end test cases

- [x] E11.01 title, subtitle, caption and tag.
- [x] E11.02 `x.title = FALSE` and `y.title = NULL` remove the titles; strings replace them.
- [x] E11.03 Numeric category: legend title shown automatically; discrete category: none.
- [x] E11.04 `category.title` is used for both colour and fill.
- [x] E11.05 Legend position: top for discrete, right for numeric categories; explicit value honoured.
- [x] E11.06 `legend.reverse = TRUE` reverses the legend.
- [x] E11.07 Horizontal plot with legend on top: legend reversed to match the bars.
- [x] E11.08 `legend.nrow = 2` reaches the guide.

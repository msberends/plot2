# E07 Sizes, widths and colour vectors

`R/plot2.R:1419-1471`

## Role

Sets default size, width and line width; decides whether ggplot2's own colours are kept (`original_colours`); applies spider defaults and `category.focus`; resolves secondary-axis colours; calls `validate_colour()`.

## Findings

F067 (`category.focus` ignored with default colours), F047, F077 (new: `y_secondary.colour` ignored unless `colour` is given), F068 (by default both series have the same colour).

## End-to-end test cases

- [x] E07.01 Defaults: point size 2, column width 0.5.
- [x] E07.02 Spider plots have a transparent fill by default.
- [x] E07.03 `category.focus = "a"` with `colour = "red"`: a red, others grey85.
- [x] E07.04 (skipped: F067) `category.focus` with default colours.
- [x] E07.05 `category.focus` not found: plot2 warning.
- [x] E07.06 `category.focus = 2` selects the second category.
- [x] E07.07 Secondary series takes `y_secondary.colour` (with `colour` given). Note: the default is `y_secondary.colour = colour`, i.e. the same colour as the primary series, not a second colour.
- [x] E07.07b (skipped: F077) `y_secondary.colour` applies without `colour`.

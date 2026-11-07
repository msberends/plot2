# 05 validate_x_scale()

`R/validate.R:736-946`

## Role

Builds the x scale: discrete, continuous, date or date-time, including limits, expansion, breaks, labels, transformation, position and fixed facet limits.

## Called from

`R/plot2.R:1943`.

## Contract

- In: prepared `df`, `values = get_x(df)`, all `x.*` arguments, `facet.fixed_x`, marks, `horizontal`.
- Out: one `scale_x_*()` object.
- Side effects: writes `plot2_env$x_lim` (used later for `coord_flip()`/`coord_cartesian()`); messages for automatic choices.

## Branches

1. `x.zoom`: limits `NA, NA`, expansion 0.5.
2. Expansion defaults (with and without `x.limits`, dates).
3. Automatic `facet.fixed_x` when the coefficient of variation of facet maxima < 25%.
4. `x.limits`: class check, length 1 or 2, Date and POSIXt conversion, padding by one unit.
5. Automatic date breaks and labels (`determine_date_breaks_labels()`).
6. Date, date-time, discrete, continuous scales; reversed for horizontal numeric x.
7. Continuous: breaks for small whole numbers; default labels via `format2()`.

## Findings

F041 (small-integer breaks drop negative values), F054 (`x.limits` silently ignored for discrete x), F052 (automatic fixed or free facet scales), F029 (date label formatting). By reading: expansion is computed twice for continuous x (lines 832-838 and 906-918); the POSIXt padding of one second (line 822) is meaningless; `min(values)` without `na.rm` at line 879.

## End-to-end test cases

- [ ] 05.01 Character x: discrete scale, `x.drop` respected.
- [ ] 05.02 Numeric x: breaks pretty; integer x from -3 to 4 shows negative breaks (F041).
- [ ] 05.03 `x.limits = c(0, 10)` on numeric x: panel range and message about `x.expand = 0`.
- [ ] 05.04 `x.limits` on discrete x: applied or a clear warning (F054).
- [ ] 05.05 `x.limits` of the wrong class: warning.
- [ ] 05.06 `x.limits` of length 3: error.
- [ ] 05.07 Date x: automatic `x.date_breaks` and `x.date_labels` messages; values for 1 week, 1 month, 1 year, 10 years ranges.
- [ ] 05.08 POSIXct x within one day: hour breaks and "HH:MM" labels.
- [ ] 05.09 `x.date_breaks = "1 month"`, `x.date_labels = "mmm yyyy"` honoured.
- [ ] 05.10 `x.transform = "log10"`: zero lower limit removed.
- [ ] 05.11 `horizontal = TRUE` with numeric x: reversed scale.
- [ ] 05.12 `x.position = "top"`.
- [ ] 05.13 Facets with similar maxima: `facet.fixed_x` assumed, message; dissimilar: free.
- [ ] 05.14 `x.zoom = TRUE`: limits follow the data.
- [ ] 05.15 `x.labels` function and `x.breaks` vector honoured.
- [ ] 05.16 `x.mic = TRUE` uses `AMR::scale_x_mic()` (skip without AMR).

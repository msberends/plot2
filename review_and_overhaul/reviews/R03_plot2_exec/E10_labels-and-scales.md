# E10 Axis labels and scale assembly

`R/plot2.R:1834-2032`

## Role

Sets default labels, resolves the font, adds the category scale (continuous via `validate_category_scale()` or discrete/manual with markdown detection), the x scale (or AMR MIC scale), the y scale and the secondary axis.

## Findings

F070 (density y title), F079 (new: `plot2(df, y = col)` crashes for a single-column data frame, so the no-x path is unreachable), F055, F041 to F044 (scale details, see R02).

## End-to-end test cases

- [x] E10.01 x and y titles are the column names; `y = n()` gives "n()".
- [x] E10.02 Histogram y title "Count".
- [x] E10.03 (skipped: F070) Density y title "Density".
- [x] E10.04 Numeric category gives a continuous colour scale and a colour bar.
- [x] E10.05 Category values with markdown are shown as expressions, with a message.
- [x] E10.06 (skipped: F079) Without x, the x-axis has no labels.
- [x] E10.07 Secondary axis is named after the secondary variable.
- [x] E10.08 Named colours: legend limited to the categories present.

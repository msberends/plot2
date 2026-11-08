# A02 plot2(): directions, type and titles

`R/plot2.R:22-79` (`.data`, `x`, `y`, `category`, `facet`, `type`, `y_secondary*`, titles); behaviour in `validate_type()` and `plot2_exec()`.

## Checked against behaviour

| Claim in `?plot2` | Result |
|---|---|
| Blank type: boxplot if at least 3 values per x | holds |
| Blank type: point for two numeric axes | holds |
| Blank type: boxplot "if there is no X-axis" | crashes for a data frame with only y (F079) |
| Blank type: option `plot2.default_type`, "defaults to `"col"`" | code default `"geom_col"`; `"line"` or `"l"` give `geom_abline` (F095, new) |
| `type = "bar"` sets `col` and `horizontal = TRUE` | holds |
| `"barpercent"`: horizontal, top 10, freq-desc, `"%n (%p)"` | holds with `y = n()`; without `y` it sums the first numeric column, no labels (F097, new) |
| Abbreviations a, b, c, h, j, l, p, r, v, d, ld, b2b, u, s, sp, bp | all map as documented; "bs" listed as a first-character abbreviation; beeswarm error misleading without ggbeeswarm (F098, new) |
| Undocumented aliases | `line-point` (used in the vignette), `radar`, `butterfly`, `tornado`, `population`, `UpSet` work (F098) |
| Dumbbell needs two category values | holds, clear error |
| `category.title` "defaults to `TRUE` if the legend items are numeric" | holds; hidden for discrete categories |
| Titles as calculations over `.data` | holds |
| `x.title` `TRUE`/`FALSE`/`NULL` | `FALSE` and `NULL` both remove; semantics of logical values not documented (F093 family) |
| `y_secondary` "will set the colour to the axis titles" | right title gets the primary colour while points stay black (F077, F068) |

## Findings

F095, F097, F098 (new); F031, F068, F077, F079 (existing).

## End-to-end test cases

- [x] A02.01 Blank type, two numeric axes: points.
- [x] A02.02 Blank type, at least three values per x: boxplot.
- [x] A02.03 Blank type otherwise: columns, or `plot2.default_type = "geom_line"`.
- [x] A02.04 (skipped: F095) `plot2.default_type` accepts `"line"` and `"l"`.
- [x] A02.05 `type = "bar"` gives horizontal columns.
- [x] A02.06 `"barpercent"`: horizontal, sorted by frequency, `"%n (%p)"` labels.
- [x] A02.07 (skipped: F097) `"barpercent"` without y counts rows.
- [x] A02.08 Documented abbreviations give the documented geoms.
- [x] A02.09 `"bs"` gives a beeswarm when ggbeeswarm is installed.
- [x] A02.10 (skipped: F098) Missing ggbeeswarm gives an install hint.
- [x] A02.11 Dumbbell requires exactly two category values.
- [x] A02.12 `category.title` shown for numeric, hidden for discrete legends.
- [x] A02.13 Titles accept a calculation over the data.
- [x] A02.14 `x.title = FALSE` removes the axis title.
- [x] A02.15 `"line-point"` works (to be documented, F098).

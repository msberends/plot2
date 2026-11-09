# A03 plot2(): axes and scales

`R/plot2.R:89-117` (`x.*`, `y.*`, `zoom`); behaviour in `validate_x_scale()`, `validate_y_scale()`.

## Checked against behaviour

| Claim | Result |
|---|---|
| `x.zoom`: `x.limits = c(NA, NA)` and `x.expand = 0` | expands by 50% on both sides (F092, new) |
| `y.zoom`: likewise with `y.expand = 0` | expands by 25% on both sides (F092) |
| `zoom`: sets `x.zoom` and `y.zoom` | gives a third result; expands below explicit `y.limits` (F092) |
| `x.expand` "defaults to 0.5", `y.expand` to 0.25 | 0.5 is an internal sentinel; passing it explicitly widens the axis (F093, new) |
| Details: "The Y-axis starts at 0 if possible"; vignette: x and y | holds for both axes of a scatter plot |
| `y.limits = c(0, NA)` starts at zero | holds without `zoom` |
| `y.percent_break` "A value on which the Y-axis should have breaks" | replaced silently-ish when it would give at most three breaks (F093) |
| `x.date_breaks` "2 years", `x.date_labels` Excel style | holds; labels in Dutch with a leading space for `d` (F003, F029) |
| `x.transform` list of transformations in Rd | list rendered; only strings accepted (F043) |
| `y.scientific` default above `10e5` | behaviour as documented; open decision F063 |
| `x.character` years and months | as documented; open decision F013 |
| `x.complete = TRUE` same as `0` | fails for numeric x converted to character (F048, F013); the example text fills x instead of y (F099) |

## Findings

F092, F093 (new); F003, F013, F029, F043, F048, F063 (existing).

## End-to-end test cases

- [x] A03.01 (skipped: F092) `x.zoom` gives the data range.
- [x] A03.02 (skipped: F092) `y.zoom` gives the data range.
- [x] A03.03 (skipped: F092) `zoom = TRUE` equals `x.zoom` plus `y.zoom`, also with `y.limits`.
- [x] A03.04 Zooming removes the zero baseline.
- [x] A03.05 (skipped: F093) Explicit `x.expand = 0.5` equals the default.
- [x] A03.06 The y axis starts at zero.
- [x] A03.07 `x.date_breaks = "2 years"`.
- [x] A03.08 `y.percent_break` sets the break distance.
- [x] A03.08b (skipped: F093) An explicit `y.percent_break` is honoured with few breaks.
- [x] A03.09 `y.limits = c(0, NA)` and `c(1, NA)` set the lower limit.
- [x] A03.10 `x.remove` removes axis labels.

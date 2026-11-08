---
id: F129
title: horizontal = TRUE with a numeric x removes all data: reversed scale with limits starting at 0
severity: high
type: bug
area: scales
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:876, R/validate.R:879]
related: [F126]
origin: I002 (R02 sheet tests)
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F129 horizontal = TRUE with a numeric x gives an empty plot

## Summary

For a numeric x with `horizontal = TRUE`, `validate_x_scale()` switches to `scales::reverse_trans()` (`R/validate.R:876`) and then sets the default limits `c(0, NA)` (`:879`). On a reversed scale these limits exclude every positive value, so all points are removed (ggplot2 warns "Removed 5 rows containing missing values or values outside the scale range").

## Reproduction

```r
p <- plot2(data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 4, 3, 8, 6)), x, y, horizontal = TRUE, type = "point")
print(nrow(stats::na.omit(ggplot2::ggplot_build(p)$data[[1]][, c("x", "y")])))
# Currently: 0. Expected after fix: 5
```

## Impact

A silently empty plot (only an R warning) for a documented argument.

## Proposed fix

Set the default limits in the order of the reversed scale (`c(NA, 0)`), or apply the reversal after the limits; add case 05.11.

## History

- 2026-11-08: filed while coding the I002 tests (case 05.11).

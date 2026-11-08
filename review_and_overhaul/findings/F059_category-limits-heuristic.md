---
id: F059
title: Numeric category limits use an undocumented rounding heuristic based on nchar() of the minimum
severity: low
type: design
area: scales
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1414]
related: []
origin: R02 sheet 07
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F059 Category limits heuristic

## Summary

`limits_fn()` in `validate_category_scale()` rounds the upper limit to `-nchar(lower)` digits (the number of characters of the printed minimum, including decimals and minus sign), forces the lower limit to 0 when it is within a fifth of the upper limit, and makes negative limits symmetric when close. The result is hard to predict and undocumented.

## Reproduction

```r
lim <- function(v) ggplot2::ggplot_build(plot2(data.frame(x = seq_along(v), y = 1, c = v), x, y, c, type = "point"))$plot$scales$get_scales("colour")$get_limits()
print(lim(c(12, 50, 87)))   # 0 100
print(lim(c(-0.37, 0.2, 0.5)))
# Expected after fix: documented behaviour; ideally data range with optional nice rounding.
```

## Proposed fix

Use the data range (optionally extended with `scales::extended_breaks()` to nice values); document; keep symmetric limits only for diverging (three-colour) scales around the midpoint.

## History

- 2026-11-07: filed in R02 (by reading).
- 2026-11-08: fixed: numeric category limits are the data range rounded outwards with pretty() (12 to 87 gives 0 to 100; -0.37 to 0.5 gives -0.4 to 0.6); with three colours and category.midpoint set, the limits are made symmetric around the midpoint; documented under category.limits. Tests 07.02 to 07.05 encoded the old limits (0 to 0.9 for values 0.1 to 0.9) and now check the colours at the ends and centre of the new limits (0 to 1). Tests: test-regression-phase4.R (F059).

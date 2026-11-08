---
id: F132
title: category.transform = "log10" errors with the default limits, which can start at 0
severity: medium
type: bug
area: scales
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1398, R/validate.R:1496]
related: [F043]
origin: fix of F043
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F132 Logarithmic category scale with default limits

## Summary

For a numeric category, `limits_fn()` of `validate_category_scale()` may start the default limits at 0 ("start at zero" heuristic). With `category.transform = "log10"` (string or object) the colour bar then asks for breaks from log10(0) = -Inf and building fails with "'from' must be a finite number". Only explicit `category.limits` containing 0 are handled.

## Reproduction

```r
d <- data.frame(x = c(1, 10, 100, 1000), y = c(3, 1, 4, 2), v = c(1, 10, 100, 1000))
print(tryCatch({ ggplot2::ggplot_build(plot2(d, x, y, v, category.transform = "log10")); "ok" }, error = conditionMessage))
# Expected after fix: "ok"
```

## Impact

Logarithmic colour scales do not work for ordinary data.

## Proposed fix

When the transformation cannot handle zero, replace a zero lower default limit by `NA` (data-driven), as the y scale does.

## History

- 2026-11-08: found while writing the regression test for F043.

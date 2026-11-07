---
id: F026
title: Factor y fails with a cryptic nchar() error from the datalabels code
severity: medium
type: bug
area: data-validation
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1013, R/validate.R:2450]
related: []
origin: R01 §3.2
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F026 Factor y

## Summary

`validate_y_scale()` returns a discrete scale for factor y, but `generate_datalabels()` calls `nchar()` on factor values and errors during rendering.

## Reproduction

```r
p <- plot2(data.frame(g = c("a", "b"), v = factor(c("x", "y"))), g, v)
print(tryCatch({ ggplot2::ggplot_build(p); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: "OK", or a clear early error explaining that y must be numeric.
```

## Impact

Unhelpful error at print time rather than at call time.

## Proposed fix

Decide whether factor y is supported; if not, error early with a clear message; if so, coerce labels with `as.character()`.

## History

- 2026-11-07: filed in R01, reproduced.

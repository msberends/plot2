---
id: F064
title: Back-to-back plots draw both halves on independent value scales
severity: high
type: bug
area: types
status: open
verification: verified
effort: M
api_change: no
locations: [R/back2back_plot.R:201, R/back2back_plot.R:335, R/back2back_plot.R:189]
related: [F065, F066]
origin: R03 sheet E08
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F064 Back-to-back halves on independent scales

## Summary

`back2back_plot()` builds the left and right halves as two separate `plot2()` calls and combines them with patchwork. Each half trains its own y (value) scale, so equal bar lengths on the two sides can represent very different values. A caution is only shown when the user sets `facet.fixed_x`/`facet.fixed_y`.

## Reproduction

```r
source("tests/testthat/helper-plot2.R")
pop <- data.frame(age = rep(c("0-9", "10-19", "20-29"), 2), sex = rep(c("M", "F"), each = 3), n = c(10, 20, 30, 100, 200, 300))
p <- plot2(pop, age, n, facet = sex, type = "b2b")
print(axis_range(p[[1]], "x")); print(axis_range(p[[2]], "x"))
# Currently: -37.5 0 and 0 375.
# Expected after fix: mirrored ranges with the same absolute maximum (e.g. -375 0 and 0 375).
```

## Impact

Population pyramids and other back-to-back comparisons are the main use of this type; independent scales make the visual comparison wrong.

## Proposed fix

Compute the common maximum over both halves (after summarising) and pass it as `y.limits` to both halves by default, unless the user sets `y.limits`. Alternatively build one plot with negated values on one side and `abs()` labels.

## History

- 2026-11-07: filed in R03, reproduced.

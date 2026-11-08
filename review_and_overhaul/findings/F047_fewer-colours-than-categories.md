---
id: F047
title: Supplying fewer colours than categories errors in the manual scale
severity: medium
type: bug
area: colours
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1827]
related: [F061]
origin: R02 sheet 09
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F047 Fewer colours than categories

## Summary

With `colour = c("red", "blue")` and three categories, `validate_colour()` passes two colours on, and `scale_*_manual()` fails with "Insufficient values in manual scale".

## Reproduction

```r
print(tryCatch({ ggplot2::ggplot_build(plot2(data.frame(g = c("a", "b", "c"), v = 1:3), g, v, g, colour = c("red", "blue"))); "OK" },
               error = function(e) conditionMessage(e)))
# Expected after fix: "OK" with a warning that colours were recycled (or extended).
```

## Proposed fix

Recycle or extend (`grDevices::colorRampPalette()`) with a warning that names the number of colours needed.

## History

- 2026-11-07: filed in R02, reproduced.
- 2026-11-08: fixed and verified (Phase 1). Unnamed colours are recycled to the number of categories with one plot2 warning; regression test 09.06.

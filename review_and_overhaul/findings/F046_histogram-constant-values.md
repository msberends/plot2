---
id: F046
title: Histogram of constant values computes binwidth 0 and fails in stat_bin
severity: medium
type: bug
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1690]
related: []
origin: R02 sheet 08
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F046 Histogram of constant values

## Summary

The automatic binwidth is `diff(range(values)) / (12 + ...)`, which is 0 for constant data. `stat_bin()` then fails with a warning and the plot is empty.

## Reproduction

```r
p <- plot2(data.frame(v = rep(5, 20)), type = "histogram")
w <- NULL; withCallingHandlers(invisible(ggplot2::ggplot_build(p)), warning = function(x) { w <<- conditionMessage(x); invokeRestart("muffleWarning") })
print(w)
# Expected after fix: no warning; one bar of height 20.
```

## Proposed fix

Fall back to `binwidth = 1` (or to ggplot2's default) when the range is 0 or not finite.

## History

- 2026-11-07: filed in R02, reproduced.

---
id: F022
title: Secondary y-axis scaling divides by zero, ignores negatives and builds the plot twice
severity: high
type: bug
area: scales
status: open
verification: verified
effort: M
api_change: no
locations: [R/validate.R:302, R/plot2.R:2021, R/plot2.R:1747]
related: [F025]
origin: R01 §2.15
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F022 Secondary y-axis scaling

## Summary

The secondary axis is rescaled by `max_secondary / max_primary` only when the maxima differ by 25%. A maximum of 0 gives a division by zero; negative ranges are not handled (minimum ignored). To obtain the primary breaks, the whole plot is built with `ggplot_build()`. The boxplot whiskers for the secondary axis use `lwd = size` instead of `linewidth`.

## Evidence

- `R/validate.R:302-310`.
- `R/plot2.R:2021`: `ggplot_build(p_added_y)`.
- `R/plot2.R:1747`: `lwd = size`.

## Reproduction

```r
print(tryCatch({ p <- plot2(data.frame(g = c("a", "b"), v = 1:2, w = c(0, 0)), g, v, y_secondary = w); ggplot2::ggplot_build(p); "OK" },
               error = function(e) conditionMessage(e)))
# Expected after fix: "OK"
```

## Impact

Crashes for all-zero secondary series; misleading secondary axes with negative values; doubled render cost.

## Proposed fix

Use a linear map between both ranges (min and max), guard against zero ranges, and compute primary breaks from the scale directly (`scale$get_breaks()` on trained limits) rather than building the plot.

## History

- 2026-11-07: filed in R01, reproduced.

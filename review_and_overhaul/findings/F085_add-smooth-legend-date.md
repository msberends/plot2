---
id: F085
title: add_smooth(legend.value = ) adds a Date dummy layer, which fails on non-date x axes
severity: low
type: bug
area: methods
status: verified
verification: verified
effort: S
api_change: no
locations: [R/add_type.R:605]
related: []
origin: R04 sheet M05
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F085 add_smooth(legend.value = ) adds a Date dummy layer, which fails on non-date x axes

## Summary

For the legend entry, `add_smooth()` draws an invisible line at `x = as.Date(Inf)` (`R/add_type.R:605`), whereas `add_line()` uses numeric `Inf`. On a discrete x-axis this fails with "Can't convert `x` <date> to <double>"; on a numeric axis it happens to work.

## Reproduction

```r
df <- data.frame(g = c("a", "b", "c"), n = c(1, 3, 2))
p <- plot2(df, g, n, type = "point")
print(tryCatch({ ggplot2::ggplot_build(add_smooth(p, legend.value = "Trend")); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: "OK".
```

## Proposed fix

Use the same legend construction as `add_line()` (numeric `Inf`), or better a key glyph via `show.legend` on the smooth layer itself.

## History

- 2026-11-08: filed in R04, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; the legend dummy of add_smooth() takes the class of the x axis (Date, POSIXct or numeric) via legend_dummy_x(). Test M05.12 now uses 12 points (loess warnings with 3) and also covers a date axis.

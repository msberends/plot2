---
id: F042
title: Percentage breaks start at the data minimum, giving unround breaks with negative values
severity: medium
type: bug
area: scales
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1095]
related: [F041]
origin: R02 sheet 06
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F042 Percentage breaks not anchored

## Summary

With `y.percent = TRUE`, breaks are `seq(min(0, x), max(x), by = y.percent_break)`. When the lower limit is negative and not a multiple of the break, all breaks are offset (-28%, -18%, -8%, 2%, ...).

## Reproduction

```r
p <- plot2(data.frame(g = c("a", "b", "c"), v = c(-0.13, 0.2, 0.47)), g, v, y.percent = TRUE)
print(na.omit(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$breaks))
# Currently: -0.28 -0.18 -0.08 0.02 ...
# Expected after fix: multiples of the break, e.g. -0.2 -0.1 0 0.1 ...
```

## Proposed fix

Anchor at zero: `seq(floor(min(0, x) / by) * by, ceiling(max(x) / by) * by, by = by)`. The same applies to the `stacked_fill`, `y.age` and `y.24h` break functions.

## History

- 2026-11-07: filed in R02, reproduced.

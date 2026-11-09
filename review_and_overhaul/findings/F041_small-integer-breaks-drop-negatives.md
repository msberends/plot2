---
id: F041
title: Breaks for small whole numbers start at 0, so negative values get no axis breaks
severity: high
type: bug
area: scales
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:898, R/validate.R:1101, R/validate.R:1388]
related: [F042]
origin: R02 sheets 05, 06, 07
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F041 Small-integer breaks drop negative values

## Summary

For whole-number data with a small maximum, the x, y and category scales use `function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 3))))`. The sequence always starts at 0, so negative values have no breaks or labels.

## Evidence

`R/validate.R:900` (x), `R/validate.R:1104` (y), `R/validate.R:1393` (category).

## Reproduction

```r
br <- function(p, ax) { pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]; as.vector(na.omit(if (ax == "y") pp$y$breaks else pp$x$breaks)) }
print(br(plot2(data.frame(g = c("a", "b", "c"), v = c(-3L, -1L, 2L)), g, v), "y"))
print(br(plot2(data.frame(x = c(-3L, 0L, 4L), v = c(1, 5, 9)), x, v, type = "point"), "x"))
# Currently: 0 2 and 0 5.
# Expected after fix: breaks covering the negative range, e.g. -3 -2 -1 0 1 2 and -2 0 2 4.
```

## Impact

Axes of differences, changes and scores below zero show no ticks for the negative part, so the plot is misleading.

## Proposed fix

Use `scales::breaks_pretty()` (or `breaks_extended(Q = c(1, 2, 5))`) and drop non-integers: `function(x) { b <- scales::breaks_pretty()(x); b[b == round(b)] }`. Factor the three copies into one helper.

## History

- 2026-11-07: filed in R02, reproduced.
- 2026-11-08: fixed and verified (Phase 1). One helper `breaks_whole_numbers()` (pretty breaks without decimals, also below zero) replaces the three copies for x, y and category; regression tests 05.02, 06.02, 07.10; full suite passes.

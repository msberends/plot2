---
id: F063
title: Axis labels switch to scientific notation automatically for ranges above one million
severity: low
type: design
area: scales
status: open
verification: verified
effort: S
api_change: possible
locations: [R/validate.R:1140]
related: []
origin: R02 sheet 06
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F063 Automatic scientific notation

## Summary

The default y labels switch to "2 • 10^6" style whenever the data range exceeds 1e6 or the formatted labels are not unique. For population counts and costs this is often not what readers expect; `big.mark` formatting ("2 000 000") is usually clearer.

## Reproduction

```r
p <- plot2(data.frame(g = c("a", "b"), v = c(500000, 2500000)), g, v)
print(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels())
# Expected after decision: labels with big marks unless y.scientific = TRUE (or a higher threshold).
```

**Decision needed:** threshold or opt-in for scientific notation.

## History

- 2026-11-07: filed in R02 (by reading).
- 2026-11-07: reproduced (labels such as "1.5 • 10^6").
- 2026-11-08: maintainer decided (AskUserQuestion): automatic scientific notation only above a higher threshold (1e9) or for very small values (below 1e-4); otherwise big.mark labels.

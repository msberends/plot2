---
id: F024
title: An explicit 0 in y.limits is replaced by the negative data minimum
severity: high
type: bug
area: scales
status: verified
verification: reading
effort: S
api_change: no
locations: [R/validate.R:1165]
related: []
origin: R01 §2.17
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F024 y.limits overwritten

## Summary

`limits_fn()` does `y.limits[y.limits == 0] <- min_value`, where `min_value` is `min(0, data)`. If the user sets `y.limits = c(0, 10)` and the data contain negative values, the lower limit silently becomes negative.

## Evidence

`R/validate.R:1159-1166`.

## Reproduction

```r
p <- plot2(data.frame(g = c("a", "b"), v = c(-5, 5)), g, v, y.limits = c(0, 10))
print(ggplot2::layer_scales(p)$y$limits)
# Expected after fix: 0 10
```

## Impact

User intent overridden; plots do not show the requested range.

## Proposed fix

Respect user limits literally; only handle `NA` entries (meaning "data-driven").

## History

- 2026-11-07: filed in R01 (by reading).
- 2026-11-08: fixed and verified (Phase 1). User limits are respected literally; only a 0 that the transformation cannot handle becomes data-driven; regression test 06.09.

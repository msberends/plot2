---
id: F040
title: add_type() silently returns the plot unchanged when the plot has no top-level data
severity: low
type: design
area: methods
status: verified
verification: verified
effort: S
api_change: no
locations: [R/add_type.R:161]
related: []
origin: R01 (noted while reading, not in the summary report)
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F040 add_type() silent no-op

## Summary

`add_type()` returns the input plot without any message when the plot has no top-level data (for example `ggplot() + geom_point(data = df, ...)`), so the requested layer is simply missing. It also resolves geoms only from ggplot2, so validated types from other packages (ggbeeswarm) fail.

## Reproduction

```r
p <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y), data = data.frame(x = 1:3, y = 1:3))
p2 <- add_type(p, type = "line", mapping = ggplot2::aes(x, y), data = data.frame(x = 1:3, y = 1:3))
print(length(p2$layers))
# Expected after fix: 2 (layer added because data was supplied), or a clear message.
```

## Proposed fix

Only skip when neither the plot nor the call provides data, and say so; resolve geoms through the same lookup as `generate_geom()`.

## History

- 2026-11-07: filed while reading in R01.
- 2026-11-08: fixed: add_type() skips only when neither the plot nor the call provides data, and then gives a plot2_warning; geoms are resolved with the new geom_function() (ggplot2 or ggbeeswarm), shared with generate_geom(). Tests: test-regression-phase4.R (F040).

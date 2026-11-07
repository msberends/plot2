---
id: F045
title: Area plots with a category are dodged (shifted sideways) when not stacked
severity: high
type: bug
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1589, R/validate.R:1617]
related: []
origin: R02 sheet 08
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F045 Area plots dodged by default

## Summary

`generate_geom()` uses `position_dodge2()` whenever neither `stacked` nor `stacked_fill` is set, also for `geom_area`. Dodging shifts each area horizontally, so areas no longer align with their x values.

## Reproduction

```r
p <- plot2(data.frame(x = rep(1:3, 2), y = 1:6, c = rep(c("a", "b"), each = 3)), x, y, c, type = "area")
print(class(p$layers[[1]]$position)[1])
# Currently: PositionDodge2
# Expected after fix: PositionIdentity (overlaid, with transparency) or PositionStack by default.
```

## Impact

Time-series area plots by group are drawn at the wrong x positions.

## Proposed fix

Restrict dodge to bar-like geoms; use identity (with fill opacity) or stack for areas, and decide which is the default.

## History

- 2026-11-07: filed in R02, reproduced.

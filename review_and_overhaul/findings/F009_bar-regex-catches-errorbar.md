---
id: F009
title: Any type containing "bar" (errorbar, crossbar) becomes horizontal columns
severity: high
type: bug
area: types
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:886]
related: [F031]
origin: R01 §2.2
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F009 "bar" regex catches errorbar and crossbar

## Summary

`if (type %like% "bar") { type <- "col"; horizontal <- TRUE }` runs before type validation and matches any type containing "bar".

## Evidence

`R/plot2.R:886`.

## Reproduction

```r
df <- data.frame(g = c("a", "b", "c"), v = 1:3)
p <- plot2(df, g, v, type = "errorbar")
print(vapply(p$layers, function(l) class(l$geom)[1], ""))
print(class(p$coordinates)[1])
# Expected after fix: an errorbar layer (or a clear error), not GeomBar with CoordFlip.
```

## Impact

Wrong plot type without any message.

## Proposed fix

Anchor the pattern: `^(bar|bars|geom_bar)$`.

## History

- 2026-11-07: filed in R01, reproduced.
- 2026-11-08: fixed and verified (Phase 1). Pattern anchored to `^(bar|bars|geom_bar)$`; regression test E03.09 (its data no longer passes unsupported `ymin`/`ymax` through `...`).

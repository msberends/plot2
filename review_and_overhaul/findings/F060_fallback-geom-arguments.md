---
id: F060
title: Fallback for other geoms passes width and size to every geom
severity: low
type: bug
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1747]
related: [F031]
origin: R02 sheet 08
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F060 Fallback geom arguments

## Summary

For types without a dedicated branch, `generate_geom()` passes `width` and `size` unconditionally, together with a warning that the type is "loosely supported". Geoms that do not take these parameters produce additional ggplot2 warnings, and lines get the deprecated `size`.

## Reproduction

```r
w <- character(0)
withCallingHandlers(invisible(ggplot2::ggplot_build(plot2(data.frame(x = 1:5, y = c(1, 3, 2, 5, 4)), x, y, type = "step"))),
                    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
print(w)
# Expected after fix: no "unknown parameters" warnings.
```

## Proposed fix

Pass only parameters the geom accepts (`intersect(names(args), geom$parameters(TRUE))` plus aesthetics in `geom$aesthetics()`).

## History

- 2026-11-07: filed in R02 (by reading).
- 2026-11-07: reproduced (deprecated `size` for lines, "Ignoring unknown parameters: `width`").

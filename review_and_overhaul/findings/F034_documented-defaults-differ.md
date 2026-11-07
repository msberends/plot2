---
id: F034
title: Documented defaults differ from method defaults; plot2.matrix ignores x/y/category
severity: medium
type: docs
area: methods
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:476, R/plot2-methods.R:1811]
related: [F035]
origin: R01 §3.10
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F034 Documented defaults differ

## Summary

The generic documents `y_secondary.colour = colour` and `y_secondary.colour_fill = colour_fill`; all methods actually use `get_colour(getOption("plot2.colour", "ggplot2"), 2)`. `plot2.matrix()` passes hard-coded `x = x, y = y, category = category`, ignoring user values silently. Other differing defaults per method (sf, freq, matrix) are intended but undocumented in one place.

## Reproduction

```r
print(formals(plot2)$y_secondary.colour)
print(formals(plot2.data.frame)$y_secondary.colour)
# Expected after fix: identical, or the difference documented.
```

## Proposed fix

Resolved structurally by F035; meanwhile align the generic's defaults with the methods and document per-method defaults.

## History

- 2026-11-07: filed in R01, checked with formals().

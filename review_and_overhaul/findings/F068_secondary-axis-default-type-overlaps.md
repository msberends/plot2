---
id: F068
title: Secondary y-axis defaults to the primary type, so two column layers overlap
severity: medium
type: design
area: types
status: verified
verification: verified
effort: S
api_change: possible
locations: [R/plot2.R:474, R/plot2.R:1751]
related: [F022]
origin: R03 sheet E09
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F068 Secondary y-axis type overlaps the primary layer

## Summary

`y_secondary.type` defaults to `type`. For the default column type, the secondary series is drawn as a second set of bars at exactly the same positions, hiding the primary bars.

## Reproduction

```r
source("tests/testthat/helper-plot2.R")
p <- plot2(data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20)), x, y, y_secondary = z)
print(geoms(p))
# Currently: GeomLabel GeomBar GeomBar GeomText.
# Expected after decision: a line (with points) for the secondary series when the primary type is col.
```

**Decision needed:** default type of the secondary series (suggestion: `"line"` for a column primary, otherwise the primary type).

## History

- 2026-11-07: R03 E07: by default `y_secondary.colour = colour`, so both series also get the same colour (columns `#595959`); together with the overlap the secondary series is hard to distinguish. With `colour = c("red", "blue")` the secondary layer was drawn in `#F8766D` (ggplot2's default hue), not in either given colour; filed separately as F077.

- 2026-11-07: filed in R03, reproduced.
- 2026-11-08: maintainer decided (AskUserQuestion): secondary series defaults to a line with points when the primary type is col or bar; otherwise the primary type.
- 2026-11-08: fixed as decided: y_secondary.type defaults to NULL; for a col or bar primary the secondary series is a line with points in the secondary colour (grouped over a discrete x, per category if any, without the fill aesthetic), otherwise the primary type. Tests: E09.05 adapted (two extra layers), E09.06 unparked, test-regression-phase4.R (F068).

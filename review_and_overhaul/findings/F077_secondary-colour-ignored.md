---
id: F077
title: y_secondary.colour is ignored unless colour is also given
severity: medium
type: bug
area: colours
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:1429, R/plot2.R:1460, R/plot2.R:1765, R/validate.R:1634]
related: [F068, F022]
origin: R03 sheet E07
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F077 Secondary-axis colours depend on the primary colour argument

## Summary

`original_colours` (keep ggplot2's own colours) is derived from `colour` and `colour_fill` only (`R/plot2.R:1429`). When it is `TRUE`, `generate_geom()` does not set any colour, also not for the secondary layer (`R/plot2.R:1765`, `R/validate.R:1634`). So `y_secondary.colour = "red"` without `colour` gives a black line. With `colour = "blue"` and no `y_secondary.colour`, the secondary line is drawn in `#F8766D` rather than in the documented default (`y_secondary.colour = colour`).

## Reproduction

```r
df <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20))
line_colour <- function(p) unique(as.character(ggplot2::layer_data(p, which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), TRUE)))$colour))
print(line_colour(plot2(df, x, y, y_secondary = z, y_secondary.type = "line", y_secondary.colour = "red")))
print(line_colour(plot2(df, x, y, y_secondary = z, y_secondary.type = "line", colour = "blue", y_secondary.colour = "red")))
print(line_colour(plot2(df, x, y, y_secondary = z, y_secondary.type = "line", colour = "blue")))
# Expected after fix: "#FF0000", "#FF0000", and the documented default for the third.
```

## Proposed fix

Decide `original_colours` separately for the secondary layer (an explicit `y_secondary.colour` always applies), and resolve the default of `y_secondary.colour` explicitly instead of through lazy evaluation of `colour` after it has been modified.

## History

- 2026-11-07: found by an R03 end-to-end test (E07.07), reproduced.
- 2026-11-08: R05 (sheet A02): `?y_secondary` says "Setting the secondary Y-axis will set the colour to the axis titles". With default colours the right axis title gets the primary colour (#F8766D) while both point layers stay black, so the title colours do not identify the series.

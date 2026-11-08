---
id: F050
title: legend.position does not support inside placement or FALSE
severity: medium
type: design
area: scales
status: open
verification: verified
effort: S
api_change: possible
locations: [R/validate.R:108]
related: []
origin: R02 sheet 02
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F050 legend.position options limited

## Summary

`validate_legend.position()` only accepts top, right, bottom, left and none. ggplot2 also supports `"inside"` with `legend.position.inside = c(x, y)`; users will also try `FALSE`. Both error.

## Reproduction

```r
d <- data.frame(g = c("a", "b"), v = 1:2, c = c("x", "y"))
print(tryCatch({ plot2(d, g, v, c, legend.position = FALSE); "OK" }, error = function(e) conditionMessage(e)))
print(tryCatch({ plot2(d, g, v, c, legend.position = c(0.9, 0.9)); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: "OK" for both (FALSE means none; numeric means inside at those coordinates).
```

## Proposed fix

Accept `FALSE` as "none"; accept a numeric vector of length 2 (map to `legend.position = "inside"`, `legend.position.inside = x`), and `"inside"`.

**Decision needed:** whether numeric positions belong in the API.

## History

- 2026-11-07: filed in R02, reproduced.
- 2026-11-08: maintainer decided (AskUserQuestion): accept FALSE (none), a numeric vector of length 2 (inside at those coordinates) and "inside".

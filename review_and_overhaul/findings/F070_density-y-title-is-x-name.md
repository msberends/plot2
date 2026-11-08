---
id: F070
title: Density plots get the x variable's name as y-axis title instead of Density
severity: medium
type: bug
area: scales
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:1846, R/plot2.R:2072]
related: []
origin: R03 sheet E10
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F070 Density plot y-axis title

## Summary

For density plots, `_var_y` is a copy of `_var_x`. The code first sets `labs(y = "Density")` (`R/plot2.R:1848`), but later `y.title = TRUE` is resolved to `get_y_name(df)`, which is the x variable, and overrides it (`R/plot2.R:2072-2079`). Histograms have an exception ("Count"), density plots do not.

## Reproduction

```r
source("tests/testthat/helper-plot2.R")
print(label_text(plot2(data.frame(x = c(1, 2, 2, 3, 5)), x, type = "density"), "y"))
# Currently: "x"
# Expected after fix: "Density"
```

## History

- 2026-11-07: filed in R03, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; y.title = TRUE gives 'Density' for density plots.

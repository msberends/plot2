---
id: F131
title: Violin plots error when a group has a single observation
severity: low
type: bug
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1682]
related: [F046]
origin: I002 (R02 sheet tests)
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F131 Violins with single-observation groups

## Summary

With a category that leaves one observation per group, `plot2(..., type = "violin")` fails with "non-numeric argument to mathematical function". Plain ggplot2 drops such groups with a warning and still draws the plot.

## Reproduction

```r
d <- data.frame(x = c("a", "a", "b", "b", "c", "c"), y = 1:6, category = c("u", "v", "u", "v", "u", "v"))
print(tryCatch({ ggplot2::ggplot_build(plot2(d, x, y, category, type = "violin")); "ok" }, error = conditionMessage))
# Expected after fix: "ok" (with ggplot2's warning about dropped groups, or a plot2 message)
```

## Impact

A crash on small or sparse data instead of a plot with a warning.

## Proposed fix

Locate the numeric operation on the violin layer's data that assumes at least two observations (likely in the limit or label calculation after `stat_ydensity()`), and guard it; or drop single-observation groups with a plot2 message.

## History

- 2026-11-08: filed while coding the I002 tests (case 08.07b).

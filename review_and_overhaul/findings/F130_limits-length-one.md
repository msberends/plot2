---
id: F130
title: x.limits and y.limits of length 1 are duplicated into a zero-width axis that removes all data
severity: medium
type: design
area: scales
status: needs-decision
verification: verified
effort: S
api_change: possible
locations: [R/validate.R:806]
related: [F093, D001]
origin: I002 (R02 sheet tests)
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F130 Limits of length 1

## Summary

The documentation says `x.limits` and `y.limits` "can be length 1 or 2" without saying what a single value means. The code repeats it (`rep(x.limits, 2)`, `R/validate.R:806`, and the same for y), giving an axis from 50 to 50: every bar is removed.

## Reproduction

```r
p <- plot2(data.frame(x = c("a", "b", "c"), y = c(12, 55, 97)), x, y, y.limits = 50)
print(nrow(stats::na.omit(ggplot2::ggplot_build(p)$data[[2]][, c("x", "y")])))
# Currently: 0 (with a warning). Expected after the decision: 3
```

## Impact

A documented form of the argument produces an empty plot.

## Proposed fix

Needs a decision on the meaning of a single value, for example the lower limit (`c(value, NA)`), which matches the common wish "start at zero"; or an error that asks for two values. Settle together with D001 and the documentation pass (F093).

## History

- 2026-11-08: filed while coding the I002 tests (case 06.10); status needs-decision.

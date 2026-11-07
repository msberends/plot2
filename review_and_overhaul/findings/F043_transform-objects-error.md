---
id: F043
title: x.transform and y.transform only accept strings; transformation objects error
severity: medium
type: bug
area: scales
status: open
verification: verified
effort: S
api_change: possible
locations: [R/validate.R:1043, R/validate.R:1160, R/validate.R:1260, R/validate.R:882]
related: [F044]
origin: R02 sheet 06
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F043 Transformation objects not supported

## Summary

The code compares `y.transform != "identity"` and `y.transform == "reverse"` in `if()` conditions. A transformation object (such as `scales::transform_log10()`, which ggplot2 accepts) is a list, so the condition has length > 1 and errors.

## Reproduction

```r
print(tryCatch({ ggplot2::ggplot_build(plot2(data.frame(g = c("a", "b"), v = c(10, 1000)), g, v, y.transform = scales::transform_log10())); "OK" },
               error = function(e) conditionMessage(e)))
# Expected after fix: "OK"
```

## Proposed fix

Normalise at the start: `tr <- scales::as.transform(y.transform)` and use `tr$name` in comparisons. Same for `x.transform` and `category.transform`.

## History

- 2026-11-07: filed in R02, reproduced.

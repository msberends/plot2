---
id: F067
title: category.focus has no effect with the default colours
severity: medium
type: bug
area: colours
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:1427, R/plot2.R:1443]
related: [F047]
origin: R03 sheet E07
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F067 category.focus ignored with default colours

## Summary

`original_colours` is decided before `category.focus` is processed. With the default `colour = "ggplot2"`, `original_colours` is `TRUE`, so the focus colour vector built afterwards is never used and all categories keep their default colours.

## Reproduction

```r
source("tests/testthat/helper-plot2.R")
d <- data.frame(x = c("p", "q", "r"), y = c(1, 3, 2), category = c("b", "a", "c"))
print(scale_values(plot2(d, x, y, category, category.focus = "a"), "fill"))
print(scale_values(plot2(d, x, y, category, category.focus = "a", colour = "red"), "fill"))
# Currently: three default colours; only with colour = "red" is the focus applied.
# Expected after fix: "a" in the first default colour, the others grey85, also without colour.
```

## Proposed fix

Process `category.focus` before deciding `original_colours`, or set `original_colours <- FALSE` when a focus is applied.

## History

- 2026-11-07: filed in R03, reproduced.

---
id: F066
title: Back-to-back halves assign category colours independently, so colours do not match between sides
severity: high
type: bug
area: colours
status: open
verification: verified
effort: S
api_change: no
locations: [R/back2back_plot.R:201, R/back2back_plot.R:335]
related: [F064]
origin: R03 sheet E08
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F066 Back-to-back category colours differ between halves

## Summary

Each half maps the category levels present in its own data to colours. If a level is missing on one side, the colours shift: the same category gets a different colour left and right.

## Reproduction

```r
source("tests/testthat/helper-plot2.R")
pop3 <- data.frame(age = rep(c("0-9", "10-19"), 2), sex = rep(c("M", "F"), each = 2), grp = c("a", "b", "b", "b"), n = c(100, 200, 300, 400))
p <- plot2(pop3, age, n, grp, facet = sex, type = "b2b")
print(scale_values(p[[1]], "fill")); print(scale_values(p[[2]], "fill"))
# Currently: a = #F8766D, b = #00BA38 on the left; b = #F8766D on the right.
# Expected after fix: b has the same colour on both sides.
```

## Proposed fix

Determine the category levels and colours once from the full data and pass a named colour vector to both halves (with `drop = FALSE` limits), and collect the legend with patchwork.

## History

- 2026-11-07: filed in R03, reproduced.

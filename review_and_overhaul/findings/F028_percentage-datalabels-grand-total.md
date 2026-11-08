---
id: F028
title: "%p" datalabels are relative to the grand total, not per facet or category
severity: medium
type: design
area: data-validation
status: open
verification: reading
effort: S
api_change: possible
locations: [R/validate.R:2951]
related: []
origin: R01 §3.4
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F028 Percentage datalabels denominator

## Summary

`format_datalabels()` computes `datalabels / sum(datalabels)` over the whole data. In faceted or stacked plots users usually expect percentages within the facet or within the x group.

## Reproduction

```r
p <- plot2(admitted_patients, hospital, n(), facet = gender, datalabels.format = "%p", datalabels = TRUE)
print(ggplot2::ggplot_build(p)$data[[length(p$layers)]]$label)
# Expected after fix (if per-facet is chosen): percentages summing to 100% within each facet.
```

## Proposed fix

Add an argument (for example `datalabels.percent_by = c("total", "facet", "x", "category")`) or document the current behaviour clearly.

**Decision needed:** default denominator.

## History

- 2026-11-07: filed in R01 (by reading).
- 2026-11-08: maintainer decided (AskUserQuestion): new argument (datalabels.percent_by: total, facet, x, category); default per x group within the facet, so stacked bars sum to 100% per bar.

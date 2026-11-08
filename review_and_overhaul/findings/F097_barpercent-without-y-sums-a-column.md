---
id: F097
title: type = "barpercent" without y sums the first numeric column (e.g. an ID) and shows no labels
severity: medium
type: bug
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:63, R/plot2.R:833]
related: [F012, F065, F079]
origin: R05 sheet A02
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F097 barpercent without y

## Summary

`"barpercent"` is documented as a shortcut for a frequency chart with `"%n (%p)"` labels. Without `y`, plot2 picks the first numeric column as `y` and sums it: for `admitted_patients` that is `patient_id`, giving bars of 5447, 6034, ... and no datalabels at all. With no numeric column it stops with "no numeric column found to use for y", although counting needs none.

## Reproduction

```r
p <- plot2(admitted_patients, hospital, type = "barpercent")
print(p$labels$y)                                    # "patient_id"
print(ggplot2::layer_data(p, 1)$y)                    # sums of IDs
print(tryCatch(plot2(data.frame(g = c("a", "b", "b")), g, type = "bp"), error = conditionMessage))
# Expected after fix: y = n() is used when y is missing; counts 49, 67, ... with "%n (%p)" labels.
```

## Impact

A wrong, plausible-looking chart for the most natural call of this type.

## Proposed fix

When `type` is `"barpercent"` and `y` is missing, set `y` to `n()` before the automatic y selection.

## History

- 2026-11-08: filed in R05; test A02.07 parked.

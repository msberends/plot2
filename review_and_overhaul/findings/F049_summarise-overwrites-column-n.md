---
id: F049
title: summarise_data() overwrites a user column named n and crashes
severity: high
type: bug
area: data-validation
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2895]
related: [F053]
origin: R02 sheet 23
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F049 summarise_data() overwrites column n

## Summary

`summarise_data()` stores y in a temporary column called `n` before grouping by the x, category and facet columns. If one of those is itself called `n`, it is overwritten with y values, the grouping is wrong and a later subset crashes.

## Reproduction

```r
print(tryCatch({ plot2(data.frame(n = c("a", "a", "b"), v = c(1, 2, 3)), n, v); "OK" }, error = function(e) conditionMessage(e)))
# Currently: "Can't subset columns that don't exist. Column `n` doesn't exist."
# Expected after fix: "OK", bars a = 3 and b = 3.
```

## Impact

`n` is the most common column name in count data (it is even preferred automatically as y).

## Proposed fix

Summarise the reserved `_var_y` column directly; never introduce user-namespace temporary names. Audit for similar temporary names (`count`, `value`, `name`, `out`).

## History

- 2026-11-07: filed in R02, reproduced.

---
id: F053
title: Non-scalar or NA results of summarise_function give unclear dplyr errors or drop groups
severity: low
type: bug
area: data-validation
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2885]
related: [F049]
origin: R02 sheet 23
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F053 summarise_function contract

## Summary

`summ_fn()` drops NA results (`out[!is.na(out)]`), so groups whose summary is NA vanish without notice, and functions returning more than one value (e.g. `range`) fail with a dplyr error about sizes.

## Reproduction

```r
print(tryCatch({ plot2(data.frame(g = c("a", "a", "b"), v = 1:3), g, v, summarise_function = range); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: an error stating that summarise_function must return a single value.
```

## Proposed fix

Validate the result length with a clear plot2 error; keep NA results (and let the NA handling of y warn).

## History

- 2026-11-07: filed in R02, reproduced.

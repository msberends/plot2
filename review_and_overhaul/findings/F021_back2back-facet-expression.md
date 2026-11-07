---
id: F021
title: Back-to-back check re-evaluates facet on raw data, so facet expressions fail
severity: high
type: bug
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:1408]
related: []
origin: R01 §2.14
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F021 Back-to-back facet expressions

## Summary

The check that the facet has exactly two values runs `.data |> select({{ facet }}) |> pull()`, which only works for bare column names, although the facet column has already been computed as `_var_facet`.

## Evidence

`R/plot2.R:1408`.

## Reproduction

```r
d <- data.frame(a = rep(c("x", "y"), 2), v = 1:4, s = c(1, 1, 2, 2))
print(tryCatch({ plot2(d, a, v, facet = s == 1, type = "b2b"); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: "OK"
```

## Impact

Expressions such as `facet = gender == "M"` do not work for population pyramids.

## Proposed fix

Use `n_distinct(get_facet(df))`.

## History

- 2026-11-07: filed in R01, reproduced.

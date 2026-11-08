---
id: F021
title: Back-to-back check re-evaluates facet on raw data, so facet expressions fail
severity: high
type: bug
area: types
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:1408]
related: []
origin: R01 §2.14
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F021 Back-to-back facet expressions

## Summary

The check that the facet has exactly two values runs `.data |> select({{ facet }}) |> pull()`, which only works for bare column names, although the facet column has already been computed as `_var_facet`.

## Evidence

`R/plot2.R:1408`.

## Reproduction

```r
d <- data.frame(a = rep(c("x", "y"), 2), v = 1:4, s = c("m", "m", "f", "f"))
print(tryCatch({ ggplot2::ggplot_build(plot2(d, a, v, facet = toupper(s), type = "b2b")); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: "OK"
```

## Impact

Expressions such as `facet = gender == "M"` do not work for population pyramids.

## Proposed fix

Use `n_distinct(get_facet(df))`.

## History

- 2026-11-07: filed in R01, reproduced.
- 2026-11-08: reproduction changed to a facet expression that keeps the values (`toupper(s)`); expressions that change the type of the facet (`s == 1`, `s * 2`) fail later in the back-to-back code, filed separately as F133.
- 2026-11-08: fixed and verified (Phase 1). The check uses the computed facet column (`n_distinct(get_facet(df))`); regression test E08.07; the remaining failure for type-changing expressions is F133.

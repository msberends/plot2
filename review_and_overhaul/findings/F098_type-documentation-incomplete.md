---
id: F098
title: Documentation of type and the sorting values is incomplete or inaccurate
severity: low
type: docs
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:53, R/plot2.R:57, R/plot2.R:66, R/plot2.R:126, R/plot2.R:169, R/validate.R:16]
related: [F031, F093]
origin: R05 sheet A02
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F098 type and sorting documentation

## Summary

- `"bs"` (beeswarm) is listed among the geoms that "can be abbreviated by their first character".
- Beeswarm needs `ggbeeswarm` (Suggests). Without it the error is 'plot type "beeswarm" is invalid since ggplot2::geom_beeswarm() does not exist', not an install hint.
- Accepted aliases are not documented: `"line-point"` (used in the supported types vignette), `"radar"`, `"butterfly"`, `"tornado"`, `"population"`, `"UpSet"`.
- The "Butterfly plot" link points to the Tornado diagram page; "distinquish" typo in the same item.
- The list of valid `x.sort` values omits `"alpha-desc"` (used in the examples) and any other aliases accepted by `validate_sorting()`.
- `binwidth` is "only useful for `geom = "histogram"`"; the argument is `type`.

## Reproduction

```r
print(tryCatch(plot2(iris, Species, Sepal.Length, type = "beeswarm"), error = conditionMessage)) # without ggbeeswarm
print(class(plot2(pressure, temperature, pressure, type = "line-point")$layers[[2]]$geom)[1])     # works, undocumented
# Expected after fix: an install hint; all accepted aliases and sort values documented.
```

## Impact

Users cannot discover valid values, and the beeswarm error points to the wrong package.

## Proposed fix

Generate the documented alias list from the alias table used by `validate_type()` and `validate_sorting()` (one source of truth, see I004), and check `requireNamespace("ggbeeswarm")` with an install hint.

## History

- 2026-11-08: filed in R05; test A02.10 parked.

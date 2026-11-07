---
id: F017
title: like() lowercases regex patterns, and data values are used as regex patterns
severity: high
type: bug
area: architecture
status: open
verification: verified
effort: M
api_change: no
locations: [R/utils.R:75, R/plot2.R:989, R/utils.R:232]
related: [F016]
origin: R01 §2.10
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F017 like() and data-as-regex

## Summary

`like()` implements case-insensitivity by lowercasing both input and pattern. Lowercasing a regex changes its meaning (`\\D` becomes `\\d`, `\\S` becomes `\\s`, `[A-Z]` ranges change). Separately, values from the data (column names, category values) are passed as patterns, so special characters break or alter matching.

## Evidence

- `R/utils.R:75-91`.
- `R/plot2.R:989`: `plot2_env$mapping_y %like% new_df$category` (data values as patterns).
- `R/utils.R:232`: `var_label %like% selected_cols`.

## Reproduction

```r
d <- data.frame(g = c("a", "b"), `rate (%)` = 1:2, `n[1` = 3:4, check.names = FALSE)
res <- tryCatch({ plot2(d, g, c(`rate (%)`, `n[1`)); "OK" }, error = function(e) conditionMessage(e))
print(res)
print("ABC" %like% "\\D")   # should be TRUE
# Expected after fix: "OK" and TRUE.
```

## Impact

Crashes on perfectly valid column names; subtle wrong matches across the package (`%like%` is used in about 200 places).

## Proposed fix

`grepl(pattern, x, ignore.case = TRUE, perl = TRUE)` without lowercasing. Where data values are compared, use `%in%`, `startsWith()` or `grepl(fixed = TRUE)`. Audit all `%like%` call sites for data-derived patterns.

## History

- 2026-11-07: filed in R01, reproduced.

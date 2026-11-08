---
id: F073
title: summarise_function = NULL crashes for duplicate x values, although plot2 itself recommends it
severity: high
type: bug
area: data-validation
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:818, R/plot2.R:985, R/plot2.R:1347, R/validate.R:2894]
related: [F053]
origin: R03 sheet E02
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F073 summarise_function = NULL fails when summarising is needed

## Summary

The documentation says `summarise_function = NULL` "will be converted to `function(x) x`" (`R/plot2.R:147`, `R/plot2.R:818`), and the message at `R/plot2.R:985` advises users to "use `summarise_function = NULL` to prevent this". For discrete types with duplicate x values, `summarise_data()` (`R/validate.R:2894`) calls `dplyr::summarise()`, which requires one value per group, so the identity function fails with "`n` must be size 1, not 2". The option only works when no summarising is needed, i.e. when it has no effect. The same holds for any user function that returns more than one value (contract question in F053).

## Reproduction

```r
df <- data.frame(x = c("a", "a", "b"), y = 1:3)
print(tryCatch(nrow(plot2(df, x, y, summarise_function = NULL)$data), error = function(e) conditionMessage(e)))
# Expected after fix: 3 (rows kept; ggplot2 stacks or dodges them), or a clear explanation.
```

## Proposed fix

When `summarise_function` is `NULL`, skip `summarise_data()` entirely (keep the rows, let the position adjustment handle duplicates). Validate that a user function returns a single value per group and say so in a clear error otherwise.

## History

- 2026-11-07: found by an R03 end-to-end test (E02.04), reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; summarise_function = NULL now skips summarising in plot2_exec() and validate_data(), so all values are kept.

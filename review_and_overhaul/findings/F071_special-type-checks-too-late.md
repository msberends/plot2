---
id: F071
title: Requirement checks of special types run after the code that needs them, giving cryptic errors
severity: low
type: bug
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:1149, R/plot2.R:1394, R/validate.R:593]
related: [F010]
origin: R03 sheets E05, E15
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F071 Requirement checks for special types run too late

## Summary

- Sankey: the check "Sankey plots require an X-axis of at least two variables" (`R/plot2.R:1396`) runs after `validate_data()` already reshaped the data for Sankey (`R/validate.R:593`), which fails first with a `pull()` error when only one x variable is given.
- UpSet: `x.sort == TRUE` (`R/plot2.R:1149`) errors with "the condition has length > 1" for a manual sort vector.

## Reproduction

```r
sk <- data.frame(from = c("A", "B"), n = c(5, 3), grp = c("p", "q"))
print(tryCatch({ plot2(sk, x = from, y = n, category = grp, type = "sankey"); "OK" }, error = function(e) conditionMessage(e)))
up <- data.frame(a = c(TRUE, FALSE, TRUE), b = c(TRUE, TRUE, FALSE))
print(tryCatch({ plot2(up, x = c(a, b), type = "upset", x.sort = c("1", "2")); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: the friendly Sankey message; UpSet either supports a manual order or errors clearly.
```

## Proposed fix

Validate type-specific requirements right after the type shortcuts are resolved, before data preparation; use `isTRUE()` for logical switches.

## History

- 2026-11-07: filed in R03, reproduced.

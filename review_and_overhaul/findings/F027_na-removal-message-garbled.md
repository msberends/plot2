---
id: F027
title: "Removed n rows" message uses a vector condition and is garbled
severity: medium
type: bug
area: messaging
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:487]
related: [F014]
origin: R01 §3.3
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F027 Garbled NA removal message

## Summary

`ifelse(rows_with_NA > 1, "s", "")` compares row indices instead of the count, producing a vector, which is pasted into "rows" followed by one extra "s" per removed row (for example "rowss").

## Reproduction

```r
Sys.setenv(IN_PKGDOWN = "1")   # makes plot2 print its messages
out <- capture.output(type = "message", invisible(plot2(data.frame(x = c("a", NA, NA, "b"), y = 1:4), x, y, na.rm = TRUE)))
Sys.unsetenv("IN_PKGDOWN")
print(out)
# Currently: "Removed 2 rowss since na.rm = TRUE"
# Expected after fix: "Removed 2 rows since na.rm = TRUE"
```

## Proposed fix

Use `length(rows_with_NA) > 1`.

## History

- 2026-11-07: filed in R01 (by reading); reproduced the same day ("rowss").
- 2026-11-08: fixed and verified (Phase 1). Count used instead of the row indices; regression test 03.17.

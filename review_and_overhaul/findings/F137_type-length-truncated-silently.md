---
id: F137
title: A type of length 2 or more is shortened to its first value without the intended warning
severity: low
type: bug
area: types
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:773, R/validate.R:66]
related: [F031]
origin: I002 sheet 01 (case 01.10)
commit: dcb82a8
created: 2026-11-08
updated: 2026-11-08
---

# F137 type of length 2 shortened silently

## Summary

`validate_type()` warns that `type` can only be of length 1, but `plot2_exec()` already shortens `type` with `tolower(type[1L])` before calling it, so the warning never appears and `type = c("col", "line")` silently draws columns.

## Reproduction

```r
tryCatch(plot2(data.frame(x = c("a", "b"), y = 1:2), x, y, type = c("col", "line")),
         warning = function(w) conditionMessage(w))
# returns a ggplot; expected: the warning "type can only be of length 1"
```

## Proposed fix

Warn in `plot2_exec()` before the type is shortened.

## History

- 2026-11-08: found while coding I002 sheet 01 (case 01.10); fixed in the same session: `plot2_exec()` warns before shortening. Test 01.10.

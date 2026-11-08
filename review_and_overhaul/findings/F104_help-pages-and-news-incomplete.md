---
id: F104
title: Some exported functions lack examples or return values, and NEWS misses features added since 1.0.0
severity: low
type: docs
area: housekeeping
status: open
verification: verified
effort: S
api_change: no
locations: [R/move_layer.R:15, R/plot2-methods.R:15, R/get_colour.R:196, R/get_colour.R:313, R/labellers.R:23, NEWS.md:1]
related: [F099, F083, F034]
origin: R05 sheets A01, A07, A10
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F104 Gaps in help pages and NEWS

## Summary

- `move_layer()`: no `@examples`, no `@return`; the default `move = -1` and its sign convention (negative is down) are not documented.
- `?plot2-methods`: no examples, no return value, and no overview of the method-specific defaults (sf, freq and matrix change titles, colours, expansion, theme and legend; F034).
- `register_colour()`, `unregister_colour()`: no `@return` (they return `NULL` invisibly) and no example of `unregister_colour()`.
- `euros()`, `dollars()`: the only examples are inside `\dontrun{}` although they run fine.
- `NEWS.md` stops at a 1.99.0.9000 heading that omits spider plots and `coord_spider()` (added February 2026), beeswarm plots (March 2025) and the `colour_opacity`, `category.focus` and `add_*()` changes since 1.0.0.

The project CLAUDE.md requires an `@examples` block for every exported function.

## Reproduction

```r
db <- tools::Rd_db(dir = ".")
has_ex <- vapply(db, function(rd) any(vapply(rd, attr, "", "Rd_tag") == "\\examples"), logical(1))
print(names(has_ex)[!has_ex])
print(any(grepl("spider", readLines("NEWS.md"), ignore.case = TRUE)))
# Expected after fix: only data and package pages without examples; NEWS mentions spider plots.
```

## Impact

Discoverability; users of `move_layer()` must guess the sign.

## Proposed fix

Add the missing roxygen sections, move the labeller examples out of `\dontrun{}`, and complete NEWS before the 2.0 release.

## History

- 2026-11-08: filed in R05; tests A01.04 and A10.04 parked.

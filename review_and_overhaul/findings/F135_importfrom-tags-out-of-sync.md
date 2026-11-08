---
id: F135
title: "@importFrom tags do not match the functions that use them"
severity: low
type: maintainability
area: dependencies
status: verified
verification: verified
effort: S
api_change: no
locations: [R/add_type.R, R/back2back_plot.R, R/format2.R, R/plot2-methods.R, R/plot2.R, R/utils.R, R/validate.R]
related: [F008, F108]
origin: maintainer request (2026-11-08)
commit: 279c8f1
created: 2026-11-08
updated: 2026-11-08
---

# F135 @importFrom tags out of sync with use

## Summary

Each function should declare in its own roxygen block exactly the non-base functions it calls without `pkg::`. At commit 279c8f1, 33 `@importFrom` entries were not used by the function they document, and 42 calls to imported functions were not declared in the calling function's block (they worked only because another function imported them into the namespace). Examples: `new_geom_data()` imported `dplyr::pull` and `dplyr::if_else` without using them and called `first()` without importing it; `add_line()` called `is_ggplot()` and `n_distinct()` without importing them.

## Reproduction

```r
# tests/testthat/test-themes-T09-imports.R defines import_audit()
# against the sources of 279c8f1: 42 missing, 33 unused
```

## Fix

The tags were rewritten per function (one grouped line per package, in place of the old tags) and `devtools::document()` was rerun; NAMESPACE imports no name from two packages. `tests/testthat/test-themes-T09-imports.R` checks this permanently:

- T09.01 every `@importFrom` entry is used by the function it documents (an imported generic counts as used by an S3 method of it in that block);
- T09.02 every function from a non-base package that is called without `pkg::` is declared in the calling function's `@importFrom` tags;
- T09.03 NAMESPACE imports no name from two packages;
- T09.04 a self-test of the audit on a fixture.

Calls with `pkg::` are not checked; whether such packages are in Imports is F008 and F108.

## History

- 2026-11-08: filed at the maintainer's request, fixed and verified (audit reports no issues; suite run).

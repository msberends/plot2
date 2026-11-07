---
id: F038
title: Test suite skips everything on CRAN and mostly checks only the object class
severity: high
type: test
area: tests
status: in-progress
verification: verified
effort: L
api_change: no
locations: [tests/testthat/test_plot2.R:36, tests/testthat/test_plot2.R:536]
related: []
origin: R01 §5.1
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F038 Weak test suite

## Summary

All 28 `test_that()` blocks start with `skip_on_cran()`, so CRAN runs no tests. Almost all assertions are `expect_s3_class(..., "gg")`, which none of the R01 defects would fail. Local run on 2026-11-07: 1 failure ("Plotly works", line 539) because plotly is not installed and the test lacks `skip_if_not_installed("plotly")`.

## Reproduction

```r
# From the package root:
#   Rscript -e 'devtools::test(reporter = "summary")'
# Expected after fix: no failures without optional packages; fast core tests run on CRAN.
```

## Impact

Regressions in a widely used package go unnoticed; refactoring (F035, F036) is unsafe without better tests.

## Proposed fix

1. Split tests by topic into several files.
2. Assert on built output: `ggplot2::get_labs()`, `ggplot2::layer_data()`, scale breaks and labels, number of layers.
3. Add `vdiffr` snapshot tests for each plot type.
4. Add one regression test per finding when fixing it.
5. Keep a fast core subset that runs on CRAN; guard optional packages with `skip_if_not_installed()`.

This should come before the large refactors.

## History

- 2026-11-07: filed in R01, test suite run locally.
- 2026-11-07: in progress via I001. Edition 3, helper and fixture files and helper self-tests added; plotly test now skips without plotly. Remaining: content-based tests (I002), splitting `test_plot2.R`, CRAN-run subset.

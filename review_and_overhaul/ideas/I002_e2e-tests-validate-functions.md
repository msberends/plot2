---
id: I002
title: End-to-end tests for every function in validate.R
area: tests
status: open
effort: XL
api_change: no
related: [I001, F038]
origin: user
created: 2026-11-07
updated: 2026-11-07
---

# I002 End-to-end tests for every validate.R function

## Idea

`R/validate.R` is the workhorse of the package: its 24 functions build the plot parts (data preparation, scales, geoms, colours, titles, theme, facets, datalabels, sorting, summarising), mostly called from `plot2_exec()`. Every one of them must get end-to-end tests in testthat files, together with a thorough review of each function (done as review round R02).

## Motivation

These functions decide what users see; most R01/R02 defects live here. They are also the main target of the planned refactors (F001, F035, F036), which need a safety net.

## Approach

- Specification per function: the sheets in `reviews/R02_validate/` (01 to 24). Each lists branches and numbered test cases (231 in total).
- One test file per function: `tests/testthat/test-<function>.R`, built on the I001 helpers.
- Each `test_that()` names its case ID; tick the case in the sheet when the test exists.
- Cases for open findings are written now as desired behaviour and skipped with the finding ID, so fixing a finding means removing the skip.
- Order of work: the functions involved in Phase 1 of the roadmap first (sheets 03, 05, 06, 07, 08, 09, 23), then the rest.
- Coverage target: every branch listed in the sheets; line coverage of `R/validate.R` reported by covr as a secondary measure.

## Open questions

- Should internal functions also get direct unit tests (`plot2:::sort_data()` etc.)? Recommended as a supplement for pure functions (sheets 10, 11, 12, 20, 21, 24).

## History

- 2026-11-07: filed from the maintainer's request; specification sheets created in R02.

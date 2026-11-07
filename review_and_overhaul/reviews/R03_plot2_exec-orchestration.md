---
id: R03
title: Stage-by-stage review of plot2_exec() with end-to-end tests
status: verified
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# R03 Stage-by-stage review of plot2_exec()

## Scope

`plot2_exec()` (`R/plot2.R:615-2270`), the generic `plot2()` (`R/plot2.R:398-610`) and `back2back_plot()` (`R/back2back_plot.R`), split into 15 stages following the function's own section markers (sheets E01 to E15 in `R03_plot2_exec/`). Format decided by the maintainer: sheets plus tests, review only (no changes to `R/`).

Deliberately left out (planned for R04 to R06, see `plans/review_rounds.md`): the S3 methods in detail, `coord_spider()`, the `add_*()` helpers, Shiny, theme internals, documentation and API, cross-cutting ggplot2 4.x and i18n questions.

## Method

- Reading each stage against its callers and the validate.R contracts from R02.
- Two probe scripts (about 120 calls) to establish actual behaviour before writing expectations.
- One test file per sheet, `tests/testthat/test-plot2_exec-E<nn>-<topic>.R`, written with the I001 helpers. Every test name starts with its case ID. Desired behaviour for open findings is written as a test and skipped with `skip_open_finding()`, so fixing a finding means deleting one line.
- Each unexpected test failure was investigated in a fresh process before deciding between "test wrong" and "package wrong"; the latter became a finding.

## Coverage

`R/plot2.R:398-2270` read in full; `R/back2back_plot.R` read in full (lines 150-516 in detail); the utilities in `R/utils.R` called from these stages read where used. Two new helper additions: `col_hex()` and a `check` argument for `expect_clean_state()`, both with self-tests.

## Outcome

16 new findings, F064 to F079:

- From reading and probing: F064 to F071 (back-to-back scales, numeric y vectors, back-to-back colours, `category.focus`, secondary type, internal columns, density title, late requirement checks).
- From writing the tests, i.e. found only because each expectation was run: F072 (long vector expressions crash), F073 (`summarise_function = NULL`, recommended by plot2's own message, crashes), F074 (identical x and y values give a histogram), F075 (UpSet tidyselect deprecation), F076 (explicit datalabels dropped), F077 (`y_secondary.colour` ignored), F078 (`fatten` deprecated), F079 (`plot2(df, y = col)` crashes).

Headline conclusions:

1. Data are tested where arguments should be: F074 and F079 join F007 as instances of one design problem (values compared instead of which arguments were given). A single fix pattern (decide on `missing()`/the `_misses.*` flags, never on values) covers all three.
2. Rules are duplicated between `validate_data()` and `plot2_exec()` and disagree (F076, also the datalabels and summarising rules). Each rule should live in one place.
3. Composite types (back-to-back, UpSet) are built by nested `plot2()` calls that cannot share scales or colours (F064, F066) and that interact with the global state (F001). A composite builder that computes shared scales once would solve these together.
4. Two soft deprecations (F075, F078) are silent for users but show in plot2's own checks and will become errors later.

Tests: 15 new files; the cases per sheet are listed in the sheets. Result of the full run: see JOURNAL.md (session 4).

Suggested next round: R04 (unreviewed files), as planned.

# Journal

Newest entry first. One entry per working session: what was done, what changed status, what is next.

## 2026-11-08 (session 6, continued: R06)

- Committed D001 as 2d262e9. Maintainer decided R06: theme sheets plus tests; review only; four extra themes (R version support, dependency footprint, graphics devices, CI workflows); current code reviewed, D001 names mentioned where relevant.
- Ran R06: review file `reviews/R06_cross-cutting-themes.md`, 8 sheets T01 to T08 in `reviews/R06_themes/`, 8 test files `tests/testthat/test-themes-T*.R`, benchmark `repro/perf_r06.R` with results `repro/perf_r06_results_2026-11-08.csv`.
- Temporary environments only (scratchpad libraries, a disposable `rocker/r-ver:4.1.3` container; nothing in the user library): full suite passes on ggplot2 4.0.3.9000; plot2 works on R 4.1.3 (test-suite issues only); plot2 works with hard dependencies only, except `lubridate` for one format.
- Filed F105 to F110 (4 medium, 2 low). Extended F003, F004, F008, F025, F029, F038.
- Test fix: the R04 tests of M08 used `@` on S7 objects, which fails before R 4.3; changed to `S7::prop()`. README test A10.01 now skips without ggforce.
- Updated roadmap, coverage (workflows now full), review_rounds, INDEX rebuilt.
- Full test suite: 364 tests, no failures, no warnings, 83 skips (80 parked findings; plotly, ggbeeswarm, ggplot2movies not installed).
- Next: R07 (Shiny app), format to be decided at its start; implementation of D001 before the first CRAN release.

## 2026-11-08 (session 6)

- Maintainer decided R05: format "Inventory + sheets + tests"; review only, also no documentation fixes; API changes collected in one idea plus one decision file (`plans/review_rounds.md`).
- New tool `tools/argument_inventory.R` generates `reviews/R05_api/A00_argument-inventory.md` and `.csv` (153 arguments; 76 never used in a test, 123 never shown in vignettes or README).
- Ran R05: review file `reviews/R05_api-and-documentation.md`, 10 sheets A01 to A10 in `reviews/R05_api/`, 10 test files `tests/testthat/test-api-A*.R`. New test helpers `source_file()` and `eval_chunks()` with a self-test.
- All examples, both vignettes and README run in detached processes; README `eval = FALSE` chunks run one by one. `ggplot2movies`, `ggbeeswarm` and `plotly` were installed only in a temporary library in the session scratchpad to render the supported types vignette; the user library is unchanged.
- Filed F092 to F104 (7 medium, 6 low), idea I004 (consolidated API proposal, needs-decision) and the first decision file D001 (scope and timing of renames and default changes). Extended F039 (CLAUDE.md lists a non-existent `plot2.lm` and an exported `format2()`) and F077. All 13 new reproductions run in fresh processes.
- Updated roadmap (new "Documentation pass" section; F092, F095 to F097 in Phase 1; F094 and I004 in Phase 5), coverage (roxygen, vignettes, README, NEWS now full), review README (tools), INDEX rebuilt.
- Full test suite: 329 tests, no failures, no warnings, 65 skips (62 parked findings; plotly, ggbeeswarm and ggplot2movies not installed). Two new tests needed `local_known_deprecation("F078")` for the boxplot `fatten` deprecation.
- Maintainer: plot2 is not on CRAN, is beta and has no known public users; breaking changes are allowed until (not later than) the first CRAN release. I004 and D001 revised (no deprecation path), F094 and F100 extended (README claims CRAN availability).
- Committed R05 as 6d51430. Then decided D001 item by item with the maintainer (19 items plus the naming rule D001.R: `<argument>.<setting_in_snake_case>`, dot only after a plot2() argument). D001 verified; I004 open for implementation before the first CRAN release. Maintainer's design principle recorded in D001. New idea I005 (more outspoken documentation tone).
- Next: R06 (cross-cutting themes), format to be decided at its start.

## 2026-11-08 (session 5)

- Project CLAUDE.md updated: testing section rewritten for the new infrastructure, new section "Review and Overhaul" so that every session starts from README, JOURNAL and INDEX; layout tree updated. Memory note refreshed.
- Maintainer decided R04: as R03 (sheets plus tests), review only, Shiny app deferred to a new round R07 (`plans/review_rounds.md`).
- Ran R04: review file `reviews/R04_unreviewed-files.md`, 10 sheets in `reviews/R04_modules/`, 10 test files `tests/testthat/test-modules-M*.R`.
- Filed F080 to F091 (1 high, 5 medium, 6 low) and idea I003 (spider facets). All 12 reproductions run in fresh processes.
- `setup.R` now also resets `plot2.colour_font_axis`, `plot2.colour_sf`, `plot2.colour_sf_fill`.
- Updated roadmap, coverage (all files except `R/shiny.R` now full), INDEX rebuilt.
- Full test suite: no failures, no warnings, 43 skips (42 parked findings, 1 plotly not installed). Observed once: the random "Use options(plot2.silent = TRUE)" note printed during a test run, an instance of F002 (output depends on the RNG).
- Next: R05 (API and documentation); format to be decided by the maintainer at its start.

## 2026-11-07 (session 4)

- Ran review round R03 (`plot2_exec()` orchestration, review only). Review file `reviews/R03_plot2_exec-orchestration.md`, 15 stage sheets in `reviews/R03_plot2_exec/`.
- Wrote 15 test files `tests/testthat/test-plot2_exec-E01-*.R` to `E15`, one per sheet, with the I001 helpers. Desired behaviour for open findings is parked with `skip_open_finding()` (27 such skips).
- Filed F064 to F079 (16 findings: 6 high, 6 medium, 4 low). Eight of them (F072 to F079) were found only because the tests were run, among them two crashes on common calls (F072 long vector expressions, F079 `plot2(df, y = col)`) and `summarise_function = NULL` failing although plot2 recommends it (F073).
- F075 and F078 first looked like user-facing warnings; confirmed to be soft deprecations that only show in plot2's own checks, so downgraded to low.
- Extended F005 (back-to-back locations) and F068 (default colour of the secondary series).
- I001 helpers extended: `col_hex()`, `expect_clean_state(check = )`, `local_known_deprecation()`.
- Updated roadmap (F064 to F079 placed), coverage (`R/plot2.R`, `R/back2back_plot.R`), review_rounds (R03 done); INDEX rebuilt; all new reproductions run.
- Full test suite: no failures, no warnings, 28 skips (27 parked findings, 1 plotly not installed).
- Next: R04 (unreviewed files); format to be decided by the maintainer at its start.

## 2026-11-07 (session 3)

- Maintainer approved testthat edition 3 and vdiffr; vdiffr installed in the user library.
- Built the I001 test infrastructure: `helper-plot2.R` (accessors, diagnostics capture, 10 expectations, snapshot and skip helpers), `helper-fixtures.R` (14 fixtures), `setup.R` (deterministic environment), `test-helpers.R` (76 self-test expectations). Details in I001 History.
- DESCRIPTION: edition 3, `testthat (>= 3.0.0)`, `vdiffr` and `withr` in Suggests.
- Full test suite green (1 skip: plotly not installed). I001 and F038 now in progress.
- Maintainer decided the review rounds: R03 = `plot2_exec()` orchestration, as sheets plus tests, review only; R04 to R06 = unreviewed files, API and documentation, cross-cutting themes. Recorded in `plans/review_rounds.md`.
- Next: start R03.

## 2026-11-07 (session 2)

- Maintainer idea: end-to-end tests for every `validate.R` function and a testthat overhaul with helper files. Filed as I001 (infrastructure, with a draft design of helpers, fixtures and conventions) and I002 (end-to-end tests).
- Ran review round R02: every function in `R/validate.R` reviewed in depth. One sheet per function in `reviews/R02_validate/` (role, call sites, contract, branches, findings, 231 numbered test cases).
- Filed F041 to F063 (4 high, 9 medium, 10 low); extended F031. New maintainer decisions: F050, F052, F063 (total now 8).
- Updated roadmap (Phase 0 now I001 and I002), coverage and README (round subfolders).
- Ran the reproductions of F031 and F041 to F063.
  - Confirmed and upgraded from "reading" to "verified": F055, F058, F060, F062, F063.
  - Corrected the chunks for F041, F057 and F059.
  - Closed F057 as `wontfix`: not reproducible, all text elements scale correctly.
- Next: maintainer decisions; then Phase 0, starting with the I001 helpers.

## 2026-11-07 (session 1)

- Ran review round R01 on commit `d09e04d` (see `reviews/R01_initial-full-review.md`).
- Set up this folder: structure, templates, `tools/build_index.R` (index and validation), `tools/run_repro.R` (runs reproduction chunks in fresh R processes).
- Filed F001 to F040; drafted `plans/roadmap.md`; recorded review depth in `coverage.md`.
- Added `^review_and_overhaul$` to `.Rbuildignore`.
- Ran all 40 reproductions with `tools/run_repro.R all`: every runnable chunk shows its defect. Corrected four chunks:
  - F014, F027: the first chunks used `rlang::local_interactive()`, which does not work at top level. Re-verified through `IN_PKGDOWN`; both defects confirmed.
  - F012: now reproduced; it crashes rather than merely choosing the wrong y.
  - F025: timings now taken after a warm-up call.
- Five findings await a maintainer decision: F003, F011, F013, F020, F028.
- Next: maintainer to add ideas (`ideas/`) and decide on the needs-decision items; then Phase 0.

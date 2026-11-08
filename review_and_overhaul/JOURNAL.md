# Journal

Newest entry first. One entry per working session: what was done, what changed status, what is next.

## 2026-11-08 (session 6, continued: Phase 2)

- Maintainer decided (AskUserQuestion): messages as conditions but printed only interactively, as now; warnings real R warnings, cautions as messages; diagnostics grouped at the end of the call in order of occurrence; F036 in Phase 2 only the per-call context, the guessing step (and any plot2.guess option) to Phase 5.
- Fixed and verified F001 and F014 in one commit (e884459): call_state_enter()/call_state_exit() in every plot2() method save, clear and restore plot2_env per call (nested calls safe, no leaks; only reg_cols shared; font cache moved to plot2_session); plot2_message()/plot2_caution()/plot2_warning() collect per call and emit conditions at the end of the outermost call, or at once outside a call.
- Real warnings exposed tests that hit plot2 warnings; those now expect them. Two real issues surfaced and were fixed: x.zoom set numeric limits on a discrete x axis; long plotmath titles warned about line wrapping. plot2_diagnostics() now classifies by condition class.
- F036 in-progress; remainder in the Phase 5 list of the roadmap.
- Checks: full suite (NOT_CRAN=true) 552 tests, 0 failures, 0 warnings, 68 skips; both vignettes render without warnings in their output; all examples run (plotly not installed here).
- Next: Phase 3 (dependencies, locale, fonts) per roadmap.

## 2026-11-08 (session 6, continued: Phase 1, third part)

- Fixed and verified: F048, F109 (CI checks R 4.1 and hard dependencies only), F103 (static colour.Rd), F102 (vignettes guard suggested packages), F099, F098 (plus an install hint for a missing ggbeeswarm), F104, F100, F101. F093 partly (points 3 and 5); points 1, 2, 4 and 6 depend on missing() and wait for F094.
- The 'ggplot2' palette now equals `scales::hue_pal()(n)` (maintainer: it stands for not setting a colour, so it must follow ggplot2's own colours).
- New at the maintainer's request: F135, an audit of `@importFrom` tags per function in both directions (unused tags, undeclared non-base calls). 42 missing and 33 unused entries corrected; permanent test `test-themes-T09-imports.R` (T09.01 to T09.04). Follow-up: `format2.percentage()` was never registered as an S3 method (798d526).
- Test infrastructure: `eval_chunks(exclude =)` now skips matching expressions instead of whole chunks; A10.01 failed when run on its own because the README set-up chunk (with `library(ggplot2)`) was excluded.
- F134 (euro sign on pdf()) remains needs-decision. `scales` is used unconditionally but is in Suggests: part of F008 (later phase).
- Tone rewrite of README and vignettes left to I005 (draft for the maintainer); F100 and F101 corrected facts only.
- Question from the maintainer: how finding IDs in tests stay traceable. Proposed (awaiting decision): keep `review_and_overhaul/` permanently, let `skip_open_finding()` print the finding title, optionally move open findings to GitHub issues at the end.
- Full suite (NOT_CRAN=true): 543 tests, 0 failures, 0 warnings, 75 skips. Without NOT_CRAN the suite skips 35 more tests; earlier totals in this journal used NOT_CRAN.
- Maintainer decided (AskUserQuestion): F094 NULL means automatic (API change consented; stays in Phase 5 with F035/I004 unless pulled forward); F092 5% expansion, fixed (50bd256); F134 documentation only (wontfix, T07.02b now tests the cairo_pdf advice); traceability: open findings move to GitHub issues at the end of the overhaul (roadmap).
- Full suite after F092: 543 tests, 0 failures, 0 warnings, 70 skips.
- Next: Phase 2 per roadmap; F093 points 1, 2, 4 and 6 with F094.

## 2026-11-08 (session 6, continued: Phase 1, second part)

- Fixed and verified (one commit per finding; shared code in one commit): F049+F053+F073, F051, F054, F056, F067, F069, F070, F071, F072, F074, F075, F076, F077, F078, F079, F080, F082, F083, F085, F086, F087, F090, F091, F095, F096, F097+F106, F107 (points 1, 2, 4), F131. Earlier in this stretch: F004, F007, F021, F026, F044 to F047, F065.
- History repaired: my zero-context hunk staging (`git apply --unidiff-zero`) put inserted lines at shifted positions in four local commits (F049 trio, F051, F054, F056). Reset to eb94d0c and recommitted with a helper that applies hunks by line number; all commits since 7a60828 checked for parse errors and relocated lines. Nothing had been pushed.
- Migrated to roxygen2 8.1.0 at the maintainer's request (034c5fa). roxygen2 8 groups importFrom() per package; R's NAMESPACE parser backticks non-syntactic names except the first in such a call, so the import of `%+replace%` broke installation and load_all(). theme_minimal2() now takes the operator from ggplot2 locally.
- New: F134 (euro sign on the default pdf() device), needs-decision, split from F107.
- Behaviour notes: colours keep the colour class after `[` (F090), also in built layer data; without y, plot2() counts rows when the data have no numeric column or the type is barpercent or sankey (replaces the F071 "Sankey requires y" error); F077 fixed only for an explicit y_secondary.colour.
- For the maintainer: F092 (zoom expansion), F130 (length-1 limits), area stacking default (F045 note), default of y_secondary.colour (methods use the second palette colour, plot2() documents `colour`), a default category for Sankey plots (would change E06.06/E15.03), F134, the 'ggplot2' palette differs from ggplot2's hue spacing (#00BA38 versus #00BFC4 for two groups), and F103 (colour.Rd changes at every document() run; I reverted that noise each time).
- Full suite after the last fix: 537 tests, 0 failures, 0 warnings, 82 skips.
- Next: remaining Phase 1 items F048, F093, F094 (possible API change), F098 to F104 (documentation, with I005), F109 (CI).

## 2026-11-08 (session 6, continued: Phase 0 and Phase 1)

- Maintainer decided: I002 Phase 1 sheets first; same branch; one commit per finding; autonomous.
- Phase 0 done (9430884): 103 I002 cases for sheets 03, 05 to 09, 23; new findings F126 to F131.
- Phase 1 fixed and verified so far (one commit each): F002, F005, F009, F010, F012, F015, F016, F023, F024, F027, F033, F041, F042, F043, F126, F127, F128, F129, and F132 (new, found while fixing F043). Part of the F016 change landed in the F033 commit.
- Deferred for a maintainer decision: F092 (intended zoom expansion; documentation says 0, code uses 0.5).
- Last full suite run (after F041): 521 tests, 0 failures. Not rerun after the later fixes; per-file tests pass.
- Next: remaining Phase 1 findings in roadmap order (F004, F007, F021, F026, F044 onwards); run the full suite first.

## 2026-11-08 (session 6, continued: R07)

- Maintainer decided R07: sheets plus `shiny::testServer()` tests; review only (F006 too); current code reviewed with D001 dependencies noted; server deployment in scope, every input treated as untrusted.
- Read `R/shiny.R` in full. Review file `reviews/R07_shiny-app.md`, README and 7 sheets S01 to S07 in `reviews/R07_shiny/`, 7 test files `tests/testthat/test-shiny-S*.R` (48 cases). New helpers `shiny_app()`, `shiny_app_env()`, `shiny_init()`, `shiny_change()`, `shiny_file()` with a self-test. Widget probe `repro/shiny_widgets_r07.R`.
- F006 verified: four routes to arbitrary code (direction selectors, text inputs, `summarise_function`, data set selector), each confirmed with a marker file in a temporary folder; reproduction replaced by an executable one.
- Filed F111 to F125 (3 high, 9 medium, 3 low); the maintainer then set all security findings (F111, F112, F113, F122) to critical, followed by F114 and F121 (their security aspects): 6 critical, 6 medium, 3 low. Notable: every `plot2()` call clears `plot2_env`, which holds the app's defaults and last plot (F112); `create_interactively(data = df)` uses `stats::df` (F114).
- Every parked test was run once without its skip and failed; all other cases pass. The maximum-size export was not executed (memory).
- Updated roadmap (Phase 6 ordered: security first, input specification with D001), coverage (`R/shiny.R` full), review_rounds (R07 done), project CLAUDE.md (test file families), INDEX rebuilt.
- Full test suite: 413 tests, no failures, no warnings, 112 skips (83 as before, 28 parked Shiny cases, and S05.02 because svglite is not installed).
- Next: no further review rounds planned; fixes per roadmap, D001 (I004) before the first CRAN release.

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

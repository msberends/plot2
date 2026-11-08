# Journal

Newest entry first. One entry per working session: what was done, what changed status, what is next.

## 2026-11-09 (session 7: F061, F139, label alignment)

- Maintainer asked for F061 and F139, and for rotated datalabels to keep a small, equal gap to the bars ("no large gaps"), and the same for `x.label_angle`.
- F140 extended (3545324): rotated datalabels (any angle) and the labels of horizontal plots are anchored exactly at the bar end, with a fixed two-space padding inside the label on the side of the bar; before, the gap was a share of the label's own length. Unrotated labels above vertical bars are unchanged. Snapshots bar-horizontal and back-to-back updated; `datalabels_of()` trims the padding.
- F142, new and fixed (3545324): rotated x labels used fixed angle rules for `hjust` only; at 90 and 270 degrees they were not centred on their ticks, and in horizontal plots they overlapped or shifted. Now `guide_axis(angle = )` (position `y` after `coord_flip()`), unless `x.label_align` is set. New test helper `axis_guide_angle()`.
- F143, new, needs-decision: long rotated datalabels can run beyond the panel, since the default `y.expand` assumes one line of text. The room needed depends on the device size; three options are listed in the finding.
- F139 fixed (c6de987): the remaining warning is ggplot2's own (factor x, `drop = FALSE`, free x scales, empty panel). plot2 now shares a discrete x scale, with a message, when `facet.drop = FALSE` leaves empty panels.
- F061 fixed: the only layer that needed the colour expansion was the boxplot whisker layer, which got the colours as fixed parameters; it now uses the colour scale with a category. The expansion block is removed. Besides the full check, the drawn groups and colours of 30 plots are identical before and after (compared from a temporary worktree of HEAD).
- Checks: full suite 764 tests, 0 failed, 7 skipped, 0 warnings; both vignettes and all examples run (only the intended "red123" warning); T09 import audit rerun after removing an unused import tag.
- Next: F143 (maintainer's choice); then the move of open findings to GitHub issues.

## 2026-11-08 (session 6, continued: the remaining open items)

- Maintainer decided (AskUserQuestion): F062 `%p` is the share of y and `x.max_items` counts the rest item; F110 is accepted and closed with a guard; I005 is written directly, in a separate commit.
- Fixed and verified:
  - F039 (f65a484): CLAUDE.md, codecov.yml, .Rbuildignore and .gitignore.
  - F062 (3b815e7, Rd 9d07281): `max_items` placeholders and counting rule.
  - F064 and F066 (eb3be7a): back-to-back halves share one value scale (built twice, the second time with the largest value of both halves, unless `y.limits` is set) and have matching colours and one merged legend.
  - F038 (5c4c8a4) and I001: `test_plot2.R` split into `test-legacy-*.R` with `helper-legacy.R`, guards for sf, AMR and ggforce, 21 vdiffr snapshots (`test-snapshots-types.R`), and a CRAN core: measured under CRAN conditions at 165 s with no failures, and the Shiny server tests (about 42 s) now skip on CRAN.
  - F110 (dcb82a8): closed as accepted (0.11 s to 0.058 s per small call); T03.06 checks that the complete theme is built once, T03.07 is a local timing guard.
  - I005 (d9f3357): README opening ("Plots for users, not for developers.", the ward example as one `plot2()` call, the `plot()` framing) and the vignette introduction. Separate commit, revertable as a whole.
  - I002: all 24 R02 sheets coded (`test-validate-<nn>-*.R`); the formerly parked cases are active. Coding them found five defects: F137 (a `type` of length 2 was shortened silently), F138 (`size`, `width`, `linewidth` unvalidated), F140 (`datalabels.angle = 270` overlapped the bars) and F141 (horizontal plots read in reverse for factors, `"alpha"` and `FALSE`), all fixed; F139 (empty facet panel warnings) partly fixed, open.
- F061: the attempt to remove the colour expansion block (c329cb2) broke A06.01 and A08.01 and was reverted (4ff67bf): when colours reach a geom as fixed parameters, ggplot2 needs one value per drawn group. Only the no-op line is removed and the block explained (0758173); open for the structural fix.
- Visible changes worth knowing: factors in horizontal plots (such as age groups in a dumbbell plot) now read in level order from the top (F141; the dumbbell snapshot was updated); bars without a category no longer use `position_dodge2()` (F139).
- Mistake on my side: the staged deletion of `test_plot2.R` slipped into the I005 commit; I noticed it before pushing and recommitted I005 without it.
- Checks: full suite 763 tests, 2 failed in the import audit (T09.01, T09.02: the `@importFrom` tags stayed on `sort_data()` after the split for F141), fixed and T09 rerun green; 8 skipped; 0 warnings; both vignettes and all examples run (only the intended "red123" warning).
- Next: F061 (structural), F139 (guide warning on an empty panel); then the move of open findings to GitHub issues.

## 2026-11-08 (session 6, continued: Phase 6, the Shiny app)

- Maintainer decided (AskUserQuestion):
  - F111: R serialisation formats are removed from the uploads altogether; `upload_tab = FALSE` means no upload at all.
  - F114: global data frames are offered only in an interactive session (no new argument).
  - F125: new argument `run = interactive()`; `run = FALSE` returns the app object for deployment.
- New finding, fixed: F136 (cff2c60), critical: column names were parsed as R code in back-to-back and UpSet plots and in `setup_aes()`, so a data set with a column named like an R call ran that call (verified with a marker file). Non-syntactic names failed in back-to-back plots. Fixed with `rlang::sym()` and `column_selection_expr()`.
- Fixed and verified:
  - F113 (479a78b): only a copy of the logo is served.
  - F111, F120 (3175610): uploads; part of F115.
  - F006, F112, F114, F115, F116, F118, F119, F121, F123, F124 (7329e26): the call is built from `shiny_input_spec()` (one row per argument of plot2(), checked against its formals) as a language object; typed values are parsed as constants, calculations only with `shiny_safe_functions()`, and the call is evaluated in an environment with base R as parent. Data sets are a registry looked up by key. No app state in `plot2_env`; "changed" means different from the starting value, without observers.
  - F117, F122 (1358c18): copying in the browser; export size checked on the server, raster exports up to 40 megapixels. clipr and fansi left Suggests.
  - F125 (bc7930e): dead code removed, `run`, `@return`, section 'Deployment'.
  - Also: `man/label_taxonomy.Rd` had not been regenerated with F058 (56ad573).
- Verified in headless Chromium (chromote) besides `shiny::testServer()`: plot, code, messages, a title with quotes and markup, and the copy button (the clipboard held the code). That run found a bug the server tests could not: the colour selector selected its first choice without a starting value, adding `colour_fill = "viridis"` to the code; fixed before the commit.
- Behaviour changes worth knowing:
  - With the default `upload_tab = FALSE` the app has no upload any more (before, the selector item revealed the hidden tab).
  - Passed data is referred to by its own name in the code (`df |> plot2()`) instead of `your_df`.
  - Calculations typed in the column selectors are limited to the safe functions; other calls are reported and left out.
  - Settings with an automatic default (e.g. `x.zoom`, `datalabels`) are Automatic/Yes/No selectors instead of switches.
- Residual risk, documented in 'Deployment': a visitor can still ask for plots that take long to compute (large data, a very small `binwidth`). Not fixed: this needs limits of the hosting platform.
- Mistake on my side: twice a `pgrep -f` pattern matched my own shell command; the first time I stopped a leftover launcher shell instead of the app, the second time my own command. No other processes were affected. I now select PIDs by process name (`ps -o comm=`).
- Checks: full suite 625 tests, 0 failed, 0 warnings, 9 skipped (was 36: the Shiny tests are unparked); one error in the self-test of the Shiny helpers (it read the removed `changed_inputs`), fixed and rerun on its own; both vignettes and all examples run (only the intended "red123" warning).
- Next: Phase 6 is done; all Shiny findings are closed. Still open: F038, F039, F061, F062, F064, F066, F110 (in progress), I001, I002, I005; then the move of open findings to GitHub issues.

## 2026-11-08 (session 6, continued: Phase 5)

- Maintainer decided (AskUserQuestion):
  - F094: `summarise_function` defaults to `NULL` (automatic, sum), and `FALSE` turns summarising off. `NULL` no longer means "do not summarise".
  - D001.3: `x.lbl_taxonomy` is replaced by the exported labeller `label_taxonomy()`. `x.mic` is removed; users add `+ AMR::scale_x_mic()` instead.
  - F036: the automatic choices are documented in one place, without a central guessing function or a `plot2.guess` option.
- Fixed and verified:
  - F035, F105 and F034 (60aa87a): the arguments are defined once, in the generic. `R/zzz.R` generates the formals of the methods and `plot2_exec()` (`plot2_method_formals()`) and builds the forwarding call (`plot2_exec_call()`). Default titles come from quosures, with `{{ }}`, `!!` and `.data` resolved.
  - F094 and F093 (6a997c0): automatic defaults are `NULL` and the hidden `_misses.*` arguments are gone. The Shiny app offers Automatic/Always/Never for `datalabels`.
  - D001/I004 (a153afd): all renames, the new `legend` argument, `stacked = "fill"`, the error for `category.title` together with `legend.title`, and `label_taxonomy()`. Old argument names give an error that names the replacement. The naming rule is in `?plot2` and CLAUDE.md.
  - F025 (b8c10ea): datalabels are formatted once, on the final rows and only when drawn.
  - F037 (1755e22): no more editing of ggplot2 internals (facet params, labels).
  - F036 (this session's last commit): the 'Automatic choices' section in `?plot2`, with one test per rule (A11).
- F110 is partly fixed (in-progress):
  - The default theme, the validated theme and the list of geoms are cached per session.
  - Installed build, five rows: 0.058 s per call, against 0.11 s before. The target of well below 0.05 s is not reached; the remaining profile is flat (dplyr verbs, ggplot2 layers, `group_sizes()` via `dplyr::group_by(.drop = FALSE)`, which I left as it is because of its empty-group semantics).
- Timings, installed build (F025):
  - 2e5 rows: 0.17 s (was 1.5 to 2.9 s).
  - Scatter plot of 2e5 rows: 0.39 s (was 2.0 s).
  - 1e6 rows: 0.70 s (was 7.4 s).
- Found on the way:
  - The summarising caution said "summarise_function = sum was set" when nothing was set.
  - The `markdown` default is `TRUE`, not automatic, so it is not listed as an automatic choice.
- F037 remainder: `p$layers`, `p$mapping` and `p$data` still use the `$` accessors that ggplot2 4.x keeps for its S7 classes. These are covered by T01 and by the daily check against development versions. I do not know whether ggplot2 will deprecate these accessors.
- Checks: full suite 609 tests, 0 failed, 0 errors, 0 warnings, 36 skipped (open findings and optional dependencies); both vignettes and all examples run (only the intended "red123" warning in `?colour`).
- Next: end of the roadmap's Phase 5. Remaining open items are listed in INDEX.md, and their move to GitHub issues at the end of the overhaul is still to come.

## 2026-11-08 (session 6, continued: Phase 4)

- Maintainer decided (AskUserQuestion): F020 `data` only in plot2.formula; F013 year/month guess only for discrete geoms, and only when `type` is unset; F028 percentages per x through a new argument `datalabels.percent_by`; F068 secondary axis as line plus points when the primary is a column/bar type; F050 legend.position accepts FALSE, numeric and "inside"; F052 heuristic kept, with a message; F063 scientific notation only at 1e9 or above, or below 1e-4; F030 lookup in the caller's environment, documented; F130 limits must have length 2 (error).
- Fixed and verified: F017, F018, F019+F081, F020, F030, F031, F013, F028, F022, F068, F032+F057, F040, F050, F052, F055, F059, F060, F063, F084, F088, F089, F130, I003.
- Notes on how decisions were implemented:
  - F020: the generic keeps a `data` formal, so a formula call still dispatches; any other use gives a clear error.
  - F028: `datalabels.percent_by = "x"` uses the facet as the group when there is no category.
  - F060: the "loosely supported" notice is now a caution.
  - F130: the length rule applies to category.limits as well.
  - F089: titles are taken from dimnames only when those are named.
- Pre-existing bugs found on the way:
  - The secondary series was never drawn rescaled (F022).
  - Spider coordinates were replaced by Cartesian coordinates when datalabels were on (I003).
  - A partial theme was overridden by `background` (F032).
- Rplots.pdf was committed by mistake and removed again in c5e84ea.
- plotly and the other suggested packages are now installed on this server, so examples and tests that need them run as well.
- Checks: full suite (NOT_CRAN=true) 586 tests, 0 failures, 0 warnings, 43 skips; both vignettes render; all examples run (the colour example warns on purpose about the unregistered name "red123", F088).
- Next: Phase 5 (structure and performance), awaiting the maintainer's go-ahead.

## 2026-11-08 (session 6, continued: Phase 3)

- Maintainer decided (AskUserQuestion): F003 system locale plus a `plot2.language` option; F011 automatic download, scoped, if CRAN allows it, otherwise opt-in; F108 broom to Suggests, cleaner replaced, crayon dropped for cli, font packages to Suggests; F029 month formats always character.
- CRAN Repository Policy (revision 6985) allows a small, actively managed user cache in `tools::R_user_dir()` and requires graceful failure of Internet use, so F011 keeps automatic download.
- Fixed and verified: F008 and F108 (9f88bab, 7ab4db4, dbf918c: Imports are now cli, dplyr, forcats, ggplot2, grDevices, grid, rlang, scales, tidyselect, tidyr; internal natural_sort() and percentage formatting replace stringr and cleaner), F003 and F029 (f15c408), F011 (dbf918c: fonts cached in R_user_dir with 90-day pruning; showtext switched on per drawing through the `plot2_font` class instead of `showtext_auto()`).
- Tests and the main vignette set `R_USER_CACHE_DIR` to a temporary folder; the ?plot2 font example runs only interactively.
- Note: the showtextdb installation on this server still holds Rock Salt, Lobster and Titan One from runs before the fix; plot2 no longer writes there.
- Checks: full suite (NOT_CRAN=true) 562 tests, 0 failures, 0 warnings, 61 skips; both vignettes render without warnings; all examples run (plotly not installed here).
- Next: Phase 4 (input handling and parsing) per roadmap, with decisions needed on F020 and F013.

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

---
id: I001
title: Overhaul the testthat infrastructure with dedicated helpers, fixtures and per-topic files
area: tests
status: in-progress
effort: L
api_change: no
related: [F038, I002]
origin: user
created: 2026-11-07
updated: 2026-11-07
---

# I001 testthat infrastructure overhaul

## Idea

The maintainer wants testthat itself to be improved substantially, including dedicated helper functions in a helper file, so that plots can be tested on their actual content rather than on their class.

## Motivation

The current single test file mostly asserts `expect_s3_class(..., "gg")` and skips everything on CRAN (F038). None of the R01 and R02 defects would be caught. A refactor of `plot2_exec()` and `validate.R` (F001, F035, F036) is unsafe without a test suite that checks output.

## Proposal (draft for discussion)

### Files

```
tests/testthat/
├── helper-plot2.R        accessors and expectations (below), loaded automatically
├── helper-fixtures.R     small canonical data sets
├── setup.R               global options for the test run (e.g. plot2.silent, locale, seed checks)
├── test-validate_type.R  one file per validate.R function (I002), named after the function
├── test-...R
├── test-plot2-types.R    per plot type
├── test-methods.R        S3 methods
├── test-colours.R, test-format2.R, test-messages.R, test-state.R, ...
└── _snaps/               vdiffr and snapshot output
```

### Accessors (helper-plot2.R)

All operate on a built plot and hide ggplot2 internals, so tests survive ggplot2 changes (F037):

- `built(p)`: cached `ggplot_build()`.
- `geoms(p)`: geom class names per layer, e.g. `c("GeomBar", "GeomLabel", "GeomText")`.
- `layer_df(p, i)`: `ggplot2::layer_data()`.
- `labels_of(p)`: `ggplot2::get_labs()` as a named list.
- `axis_breaks(p, "x"|"y")`, `axis_labels(p, ...)`, `axis_range(p, ...)`, `axis_class(p, ...)` (discrete, continuous, date).
- `scale_of(p, aes)`: trained scale for colour, fill, size, ... with `breaks`, `labels`, `limits`, `palette` values.
- `legend_position(p)`, `theme_el(p, "axis.text.x")` (via `calc_element()` on the completed theme).
- `facet_info(p)`: type, free scales, nrow, number of panels.
- `coord_class(p)`, `position_class(p, i)`.
- `plot_messages(expr)`: captures plot2 messages, cautions and warnings as a character vector (works with the current queue via `IN_PKGDOWN`, and with real conditions after F014).

### Expectations

- `expect_plot2(expr)`: evaluates, builds, fails on any R warning or error during building, returns the plot invisibly.
- `expect_geoms(p, c(...))`, `expect_labels(p, x = , y = , title = )`, `expect_breaks(p, "y", c(...))`, `expect_axis_labels()`, `expect_scale_values(p, "fill", c(...))`, `expect_facets(p, n = , free_y = )`.
- `expect_plot2_message(expr, regexp)`, `expect_no_plot2_message(expr)`.
- `expect_clean_state(expr)`: `plot2_env` contains only registered colours afterwards, `.Random.seed` and `Sys.getlocale()` unchanged, showtext not enabled (F001, F002, F003, F011).

### Fixtures (helper-fixtures.R)

Small, documented data sets covering the shapes needed: `fx_xy` (numeric x, y), `fx_cat` (character x, category, duplicates), `fx_facet`, `fx_dates`, `fx_datetimes` (with time zone), `fx_na` (NA in every direction), `fx_neg` (negative and integer values), `fx_wide` (several y columns), `fx_counts` (column `n`), `fx_special_names` (`rate (%)`, `n[1`, apostrophes, accents), `fx_large` (generated lazily for performance tests).

### Conventions

- Every test description starts with the R02 case ID and lists finding IDs, e.g. `test_that("06.13 reverse transform swaps expansion (F044)", ...)`.
- Tests for open findings describe the desired behaviour and are skipped with `skip("open: F044")` until the fix lands, so the skip list doubles as a to-do list.
- Fast content tests run on CRAN; `vdiffr` snapshots and slow tests use `skip_on_cran()`; optional packages use `skip_if_not_installed()`.
- testthat edition 3 (`Config/testthat/edition: 3`), parallel-safe tests (no reliance on shared state).

## Open questions

- Edition 3 changes some expectation semantics (e.g. `expect_equal()` uses waldo); acceptable?
- `vdiffr` as a new Suggests dependency?
- Should the helpers be internal to tests only, or partly exported (e.g. for downstream packages that test plot2 output)?

## History

- 2026-11-07: filed from the maintainer's request; draft proposal added.
- 2026-11-07: maintainer approved testthat edition 3 and vdiffr. Built:
  - `tests/testthat/helper-plot2.R`:
    - building: `built()`, `rendered()` (to a gtable, so render-time code runs);
    - layer accessors: `geoms()`, `layer_stats()`, `positions()`, `layer_index()`, `layer_df()`, `layer_params()`, `datalabels_of()`, `plot_data()`;
    - labels: `labels_of()`, `label_text()`;
    - axes: `axis_breaks()`, `axis_labels()`, `axis_range()`, `axis_class()`;
    - other scales: `scale_of()`, `scale_breaks()`, `scale_labels()`, `scale_limits()`, `scale_values()`;
    - theme, coordinates, facets: `theme_of()`, `theme_el()`, `legend_position()`, `coord_class()`, `facet_info()`;
    - diagnostics: `plot2_diagnostics()`;
    - expectations: `expect_plot2()`, `expect_geoms()`, `expect_labels()`, `expect_axis_breaks()`, `expect_axis_labels()`, `expect_scale_values()`, `expect_facets()`, `expect_plot2_message()`, `expect_no_plot2_message()`, `expect_clean_state()`;
    - snapshots and skips: `expect_plot2_snapshot()` (vdiffr), `skip_open_finding()`.
  - `tests/testthat/helper-fixtures.R`: 14 fixtures (`fx_xy`, `fx_cat`, `fx_cat_unique`, `fx_facet`, `fx_facet_similar`, `fx_dates`, `fx_datetimes`, `fx_na`, `fx_neg`, `fx_wide`, `fx_counts`, `fx_special`, `fx_numcat`, `fx_pct`) plus `fx_large()`.
  - `tests/testthat/setup.R`: UTC time zone, C locale for LC_TIME and LC_COLLATE, plot2 options reset, `OutDec = "."`.
  - `tests/testthat/test-helpers.R`: self-tests of every helper against plain ggplot2 plots (76 expectations, all passing).
  - DESCRIPTION: `Config/testthat/edition: 3`, `testthat (>= 3.0.0)`, `vdiffr` and `withr` added to Suggests.
- 2026-11-07: deviations from the draft:
  - `expect_labels()` takes `.plot` as its first argument, so that it cannot clash with a label called `x`.
  - Every expectation reports exactly one result, as testthat 3.3 requires.
  - Plots are rendered to a gtable, not only built, because ggplot2 4.x raises some warnings (e.g. removed rows) only at render time and plot2's label functions run then.
- 2026-11-07: the existing `test_plot2.R` passes unchanged under edition 3. Added `skip_if_not_installed("plotly")` to its plotly test. Full suite green, 1 skip.
- 2026-11-07 (R03): first real use, 15 test files for `plot2_exec()`. Additions: `col_hex()` (normalises colour names and hex codes), a `check` argument for `expect_clean_state()` ("state", "rng", "locale", so a state test is not hidden behind F002), and `local_known_deprecation(id)` (silences soft deprecations caused by plot2 for one test, always with a finding ID). All with self-tests.
- 2026-11-08 (R04): 10 test files `test-modules-M*.R`; `setup.R` resets three more options.
- Still to do: split `test_plot2.R` into per-topic files and convert its `expect_s3_class()` checks to content checks; first vdiffr snapshots.

---
id: R04
title: Review of the files not covered before, with end-to-end tests
status: verified
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# R04 Review of the files not covered before

## Scope

The S3 methods in full (`plot2.default`, `.formula`, `.matrix`, `.sf`, `.freq`, `.data.frame`), `add_type()` and all `add_*()` helpers, `move_layer()`, `add_mapping()`, `coord_spider()`, `theme_minimal2()` and the package options, the `colour` class methods and colour registration, re-exports and bundled data. Ten sheets in `R04_modules/` (M01 to M10).

Maintainer's decisions (2026-11-08): format as R03 (sheets plus tests), review only. The Shiny app was moved to a separate round R07.

## Method

As R03: reading, probe scripts in fresh processes, one test file per sheet (`tests/testthat/test-modules-M<nn>-*.R`), each unexpected result classified as a test error or a package defect. Line references were checked against the source after filing.

## Coverage

Read in full: `R/plot2-methods.R` (method bodies), `R/add_type.R`, `R/move_layer.R`, `R/add_mapping.R`, `R/coord_spider.R`, `R/theme_minimal2.R`, `R/get_colour.R` (methods and registration), `R/options.R`, `R/reexports.R`, `R/data.R`, `R/aa_pkg.R`. Not covered: `R/shiny.R` (R07).

Test infrastructure: `setup.R` now also resets `plot2.colour_font_axis`, `plot2.colour_sf` and `plot2.colour_sf_fill` (all options read in the code are documented, test M08.06 keeps it so).

## Outcome

12 new findings, F080 to F091 (1 high, 5 medium, 6 low), and one idea (I003):

- High: F082, `add_errorbar()` fails on every plot without a category; the only documented example happens to use one.
- Medium: F080 (`plot2(sf, x = col)` crashes), F081 (`df |> plot2(y ~ x)` fails), F083 (`move_layer()` with a positive move duplicates a layer), F084 (`add_sf()`: `datalabels.colour` ignored, fixed geometry name, a label offset that suits only the Dutch projection, even unsuitable for the bundled data), F088 (one invalid colour turns the whole colour vector into one grey; invalid colours can be registered).
- Low: F085, F086, F087, F089, F090, F091.
- I003: spider plots with facets work in plain ggplot2; plot2's restriction can probably be lifted.

Headline conclusions:

1. The `add_*()` helpers infer data from the finished plot by name (`group`, `geometry`, the x mapping). This breaks as soon as plot2 or the user names things differently (F082, F084). Passing plot2's own context (F036) to these helpers would make them robust.
2. Several functions force or test NSE arguments with base R idioms (`is.null(x)`, `get()`), which fails for column names (F080, F030). A single rule, "never evaluate a direction argument outside the data mask", would prevent the class.
3. Invalid input is often replaced silently by a default (F088 colours, F089 matrix columns), with a message that is only visible interactively (F014).

Test result: see JOURNAL.md (session 5).

Suggested next round: R05 (API and documentation).

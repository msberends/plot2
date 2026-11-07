---
id: R01
title: Initial full review of internals (correctness, robustness, performance, maintainability)
status: verified
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# R01 Initial full review

## Scope

Whole package at commit `d09e04d` (version 1.99.0.9031), with the emphasis on the inner workings of the core pipeline: `plot2()` generic, `plot2_exec()`, the S3 methods, data validation, scales, colours, themes, formatting, messaging. Requested by the maintainer with a view to a user base of hundreds of thousands; API changes allowed where strictly required.

Not in scope: vignettes, README, pkgdown site, NEWS, the visual quality of the defaults.

## Method

1. Reading the source, core files in full.
2. Comparing the formals of the generic, `plot2_exec()` and all methods programmatically.
3. About 60 targeted experiments in R 4.x with ggplot2 4.x (`devtools::load_all()`), building every plot with `ggplot_build()`.
4. Profiling with `Rprof()` on 2e5 and 1e6 rows.
5. Running the full test suite (`devtools::test()`): 1 failure (F038).

## Coverage

See `coverage.md`. Read in full: `plot2.R` (function bodies), `validate.R`, `utils.R`, `format2.R`, `get_colour.R` (main function), `dec_mark.R`, `labellers.R`, `md_to_expression.R`, `plotly.R`, `get_plot_title.R`. Partly read: `plot2-methods.R`, `back2back_plot.R`, `add_type.R`, `shiny.R`, `theme_minimal2.R`, test file. Not reviewed: `coord_spider.R` body, `add_mapping.R`, `move_layer.R`, `options.R`, `data.R`, `reexports.R`, `aa_pkg.R`, roxygen documentation text, vignettes.

## Outcome

40 findings: 7 critical, 18 high, 12 medium, 3 low (at filing). Mapping from the sections of the original report to findings:

| Report section | Finding |
|---|---|
| 1.1 to 1.7 | F001 to F007 |
| 2.1 to 2.17 | F008 to F024 |
| 3.1 to 3.10 | F025 to F034 |
| 4.1 to 4.3 | F035 to F037 |
| 5.1, 5.2 | F038, F039 |
| (noted while reading) | F040 |

Headline conclusions:

- The most serious structural problem is the shared, mutable `plot2_env` (F001), which also drives the messaging design (F014) and makes the code hard to change (F036).
- Several defects silently produce wrong figures rather than errors (F001, F004, F007, F009, F013, F024); for a scientific audience these matter most.
- Reproducibility and session hygiene: RNG consumption (F002), locale changes (F003), global showtext and font downloads (F011).
- The test suite would not have caught any of the above (F038), so test improvement should precede the large refactors.

Suggested next rounds: R02 on the not-yet-reviewed files (`coord_spider.R`, Shiny app in full, `add_*` helpers) and documentation accuracy; R03 on API design and naming once the maintainer's ideas are filed.

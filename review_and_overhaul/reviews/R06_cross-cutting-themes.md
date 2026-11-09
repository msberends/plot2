---
id: R06
title: Review of cross-cutting themes, with end-to-end tests and benchmarks
status: verified
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# R06 Cross-cutting themes

## Scope

Eight themes across all files: ggplot2 4.x internals and compatibility, internationalisation, performance, embedding (functions, knitr, Quarto, Shiny), and, added by the maintainer, R version support, dependency footprint, graphics devices and CI workflows. The Shiny app itself is R07.

Maintainer's decisions (2026-11-08): theme sheets with tests; review only; current code reviewed, future D001 names mentioned where relevant.

## Method

Probes in fresh processes; five temporary environments (see `R06_themes/README.md`): ggplot2 development version in a scratch library, R 4.1.3 in a disposable container, a library with only the hard dependencies, three locales, an installed build for benchmarks. A scaling benchmark (`repro/perf_r06.R`, results in `repro/perf_r06_results_2026-11-08.csv`). One sheet and one test file per theme (`test-themes-T*.R`).

Corrections made during the round, recorded for transparency: a first benchmark labelled automatic boxplots as column plots (fixed by setting `type`); a suspected byte-compilation artefact turned out not to exist; a first hard-dependencies run was invalid because `.libPaths()` keeps the site library (rerun with `R_LIBS_SITE` empty).

## Coverage

All of `R/` per theme; `.github/workflows/` (first review); `DESCRIPTION` and `NAMESPACE` for dependencies. Not covered: Quarto (not installed), the Shiny app (R07).

## Outcome

6 new findings, F105 to F110 (4 medium, 2 low). Extended: F003, F004, F008, F025, F029, F038.

- Medium: F105 (titles show code in programmatic use), F106 (no counting without a numeric column), F107 (plot2's own characters fail on `pdf()`; console-dependent scientific labels), F109 (CI gaps).
- Low: F108 (dependency footprint), F110 (0.1 s fixed overhead per call).

Headline conclusions:

1. **Compatibility is better than feared.** The full suite passes against ggplot2's development version and plot2 works on R 4.1.3; plot2 works with only its hard dependencies (only `lubridate` missing for one format). The risks are structural (F037, F008), not current breakage.
2. **Programmatic use is the weakest context** (F105): every wrapper function or loop gets code in its titles. This matters for the reports plot2 is meant for.
3. **Performance problems have one dominant cause** (datalabels formatted per raw row, F025), now shown for scatter plots too, plus a fixed 0.1 s per call (F110).
4. **Internationalisation is consistent except for the hard-coded Dutch defaults** (F003), which also produce an OS warning on systems without the matching locale.
5. **PDF output**, the format journals ask for, loses characters that plot2 itself generates (F107).

Test result: see JOURNAL.md (session 6).

Suggested next round: R07 (the Shiny app), after or alongside the D001 implementation.

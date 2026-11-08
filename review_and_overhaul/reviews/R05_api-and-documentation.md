---
id: R05
title: Review of the API and the documentation against behaviour, with end-to-end tests
status: verified
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# R05 API and documentation

## Scope

Argument names and defaults of `plot2()` and its methods; every roxygen text of `R/plot2.R` and of the exported helpers against the behaviour; examples; both vignettes; README, NEWS, DESCRIPTION and the pkgdown configuration. The Shiny app's help text only (the app itself is R07).

Maintainer's decisions (2026-11-08): format "Inventory + sheets + tests"; review only, including no documentation-only fixes; API changes collected in one idea with one decision file.

## Method

1. A generated inventory of all 153 arguments (`tools/argument_inventory.R`, output `R05_api/A00_argument-inventory.md` and `.csv`): default in the generic and in each method, the `@param` block, sentences on defaults, the `validate_*()` calls receiving it, and use in tests, vignettes and README.
2. Executable documentation run in fresh, detached processes: all examples (`tools::Rd2ex`), both vignettes and README rendered; README chunks with `eval = FALSE` run one by one. Missing suggested packages (`ggplot2movies`, `ggbeeswarm`, `plotly`) were installed in a temporary library in the session scratchpad only, not in the user library.
3. R's own documentation checks (`tools::codoc`, `checkDocFiles`, `checkDocStyle`, `undoc`, `checkRd`), `pkgdown::check_pkgdown()` and aspell (en_GB) over `man/` and vignettes.
4. Each documented claim checked with a probe; ten sheets (A01 to A10) and one test file per sheet. Two test helpers added: `source_file()` and `eval_chunks()` (with self-tests).

## Coverage

All roxygen in `R/` except the Shiny app's internals; `man/` (as generated); `vignettes/plot2.Rmd`, `vignettes/supported_types.Rmd`; `README.Rmd` and `README.md`; `NEWS.md`; `DESCRIPTION`; `_pkgdown.yml`. Not covered: `.github/workflows/` (R06), the Shiny app (R07).

## Outcome

13 new findings, F092 to F104 (0 high, 7 medium, 6 low), one idea (I004, consolidated API proposal) and the first decision file (D001). F039 and F077 extended.

- Medium: F092 (zoom arguments do the opposite of their documentation), F093 (seven argument descriptions that differ from behaviour), F094 (18 arguments whose visible default acts as a sentinel), F095 (`plot2.default_type = "line"` gives `geom_abline`), F096 (`colour_opacity` ignored for default colours and all R palettes), F097 (`barpercent` without y sums an ID column), F100 (README examples broken).
- Low: F098, F099, F101, F102, F103, F104.

Formal checks are clean: no undocumented arguments, no usage mismatches, no Rd errors, pkgdown index complete, README.md in sync.

Headline conclusions:

1. **The documentation is broadly accurate for the main paths** (type selection, sorting, datalabels, legend placement, sizes and widths all behave as written). The errors concentrate where plot2 decides something automatically: zooming, expansion, percentage breaks, default type option, opacity with default colours.
2. **Visible defaults are not the real defaults** for 18 arguments (F094). This one design choice explains most mismatches between usage and behaviour and blocks the single argument definition (F035). It is the main input for D001.
3. **Unevaluated examples rot.** Every broken example found was in a chunk that nothing runs (`eval = FALSE` in README, prose in the vignette). The new tests A06.01, A07.06, A08.01, A09.01, A10.01 and A10.02 now run all of them.
4. **Naming is consistent within groups and inconsistent between them** (I004): `lbl` versus `labels`, `facet.fill` versus `colour_fill`, `jitter_seed` versus `smooth.*`, `colour_opacity` meaning transparency, two unprefixed options.

Test result: see JOURNAL.md (session 6).

Suggested next round: R06 (cross-cutting themes), after the maintainer's decision D001, which may change the API that R06 reviews.

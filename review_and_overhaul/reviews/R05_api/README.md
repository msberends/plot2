# R05 sheets: API and documentation

Round R05 (maintainer's decisions of 2026-11-08: inventory plus sheets plus tests; review only, also no documentation fixes; API changes collected in one idea, I004, with one decision file, D001). Case IDs `A<sheet>.<case>`; every sheet has a test file `tests/testthat/test-api-A<nn>-*.R`, and every `test_that()` description starts with its case ID. Cases for open findings describe the desired behaviour and are skipped with `skip_open_finding()`.

- `A00_argument-inventory.md` (and `.csv`) is **generated** by `tools/argument_inventory.R`; do not edit by hand.
- Tests that read package sources (README, vignettes, `man/`) use `source_file()` and `eval_chunks()` from `helper-plot2.R`; they skip when the source tree is absent (R CMD check on an installed package) and the heavy ones skip on CRAN.

Line references are for commit `d09e04d` (R/, man/, vignettes/ and README are unchanged at `7a60828`).

| Sheet | Unit | Test file |
|---|---|---|
| A00 | Argument inventory (generated) | (none) |
| A01 | API surface: signatures, help-page coverage, defaults as sentinels | test-api-A01-signatures.R |
| A02 | `plot2()`: directions, `type`, titles | test-api-A02-directions-type-titles.R |
| A03 | `plot2()`: axes and scales | test-api-A03-axes.R |
| A04 | `plot2()`: category, legend, colours, facets | test-api-A04-category-legend-colour-facet.R |
| A05 | `plot2()`: data handling, datalabels, geoms, appearance | test-api-A05-data-labels-geoms-appearance.R |
| A06 | `?plot2`: description, details, examples | test-api-A06-plot2-help-page.R |
| A07 | Help pages of helpers, options and data | test-api-A07-helper-help-pages.R |
| A08 | Vignette "An Introduction to plot2()" | test-api-A08-vignette-plot2.R |
| A09 | Vignette "Supported Plot Types" | test-api-A09-vignette-supported-types.R |
| A10 | README, NEWS, DESCRIPTION, pkgdown | test-api-A10-readme-package-files.R |

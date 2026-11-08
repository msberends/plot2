# Planned review rounds

Decided by the maintainer on 2026-11-07. Each round gets a file in `reviews/` (and, if useful, a subfolder of working sheets) when it starts. Order may change; scope may grow as new ideas arrive.

| Round | Scope | Format | Code changes |
|---|---|---|---|
| R01 | Initial full review (done) | findings | none |
| R02 | `R/validate.R`, function by function (done) | sheets with test cases, findings | none |
| R03 | (done) `plot2_exec()` orchestration in `R/plot2.R`: data preparation, special types (UpSet, Sankey, dumbbell, spider, back-to-back), scale and theme assembly, `restore_mapping()` and other helpers it relies on in `R/utils.R` | sheets with test cases **and the end-to-end tests themselves**, written with the I001 helpers; tests for open findings parked with `skip_open_finding()` | review only, no changes to `R/` |
| R04 | (done) Files not yet reviewed: `coord_spider.R`, `add_type.R` and the `add_*()` helpers, `theme_minimal2.R`, `move_layer.R`, `add_mapping.R`, `get_colour.R` methods, the S3 methods in full (Shiny moved to R07) | sheets with test cases and the end-to-end tests, as R03 | review only, no changes to `R/` |
| R05 | (done) API and documentation: argument names and defaults, roxygen text versus behaviour, vignettes and README examples, consistency across the ~150 arguments | generated argument inventory, sheets per documentation unit, and tests asserting documented behaviour (as R03/R04) | review only, also no changes to roxygen, vignettes or README |
| R06 | (done) Cross-cutting themes across all files: ggplot2 4.x internals and compatibility, internationalisation, performance, behaviour in Shiny, knitr and Quarto; added by the maintainer: R version support, dependency footprint, graphics devices, CI workflows | theme sheets with test cases and a test file each (as R03 to R05); benchmarks as scripts in `repro/` | review only |
| R07 | (done) The Shiny app (`create_interactively()`, `R/shiny.R`) in full, including F006; deferred from R04 by the maintainer | sheets per part of the app with test cases and `shiny::testServer()` tests | review only |

Decisions for R03, recorded verbatim from the maintainer's answers:

- Scope: "plot2_exec orchestration"; the other suggestions "need all to go into review round 4, 5, and 6".
- Format: sheets plus tests.
- Fixes: review only.

Decisions for R04 (2026-11-08), from the maintainer's answers: format "As R03"; fixes "Review only"; Shiny app "Defer to later round" (now R07).

Decisions for R05 (2026-11-08), from the maintainer's answers: format "Inventory + sheets + tests"; changes "Review only" (no documentation-only fixes either); API changes "Ideas + one decision file": inconsistencies are filed as findings, proposed renames and default changes are collected in one consolidated idea with a draft deprecation path and one decision file marked needs-decision.

Decisions for R06 (2026-11-08), from the maintainer's answers: format "Theme sheets + tests"; "Review only"; extra themes "R version support, Dependency footprint, Graphics devices, CI workflows"; arguments that D001 will rename: "Review current code" (findings mention the future D001 name where relevant).

Decisions for R07 (2026-11-08), from the maintainer's answers: format "Sheets + server tests"; "Review only" (F006 too); D001: "Review current code"; deployment: "Also server deployment", so input must be treated as untrusted.

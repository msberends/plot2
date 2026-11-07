# Planned review rounds

Decided by the maintainer on 2026-11-07. Each round gets a file in `reviews/` (and, if useful, a subfolder of working sheets) when it starts. Order may change; scope may grow as new ideas arrive.

| Round | Scope | Format | Code changes |
|---|---|---|---|
| R01 | Initial full review (done) | findings | none |
| R02 | `R/validate.R`, function by function (done) | sheets with test cases, findings | none |
| R03 | (done) `plot2_exec()` orchestration in `R/plot2.R`: data preparation, special types (UpSet, Sankey, dumbbell, spider, back-to-back), scale and theme assembly, `restore_mapping()` and other helpers it relies on in `R/utils.R` | sheets with test cases **and the end-to-end tests themselves**, written with the I001 helpers; tests for open findings parked with `skip_open_finding()` | review only, no changes to `R/` |
| R04 | (done) Files not yet reviewed: `coord_spider.R`, `add_type.R` and the `add_*()` helpers, `theme_minimal2.R`, `move_layer.R`, `add_mapping.R`, `get_colour.R` methods, the S3 methods in full (Shiny moved to R07) | sheets with test cases and the end-to-end tests, as R03 | review only, no changes to `R/` |
| R05 | API and documentation: argument names and defaults, roxygen text versus behaviour, vignettes and README examples, consistency across the ~150 arguments | to be decided | to be decided |
| R06 | Cross-cutting themes across all files: ggplot2 4.x internals and compatibility, internationalisation, performance, behaviour in Shiny, knitr and Quarto | to be decided | to be decided |
| R07 | The Shiny app (`create_interactively()`, `R/shiny.R`) in full, including F006; deferred from R04 by the maintainer | to be decided | to be decided |

Decisions for R03, recorded verbatim from the maintainer's answers:

- Scope: "plot2_exec orchestration"; the other suggestions "need all to go into review round 4, 5, and 6".
- Format: sheets plus tests.
- Fixes: review only.

Decisions for R04 (2026-11-08), from the maintainer's answers: format "As R03"; fixes "Review only"; Shiny app "Defer to later round" (now R07).

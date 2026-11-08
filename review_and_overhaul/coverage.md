# Review coverage

Depth per source file and the round in which it was reviewed. Update after every round.

Depth: **full** (every function body read), **partial** (main paths read, rest skimmed), **skim** (structure only), **none**.

| File | Lines (d09e04d) | Depth | Round | Notes |
|---|---|---|---|---|
| R/plot2.R | 2270 | full (bodies and roxygen) | R01, R03, R05 | roxygen checked claim by claim in R05 (sheets A02 to A06); end-to-end tests per stage in `test-plot2_exec-E*.R`, per argument group in `test-api-A*.R` |
| R/plot2-methods.R | 2167 | full | R01, R04 | method bodies in R04 (sheets M01 to M04, M10) |
| R/validate.R | 2977 | full, per function | R01, R02 | R02: every function reviewed in depth, sheets in `reviews/R02_validate/` |
| R/utils.R | 826 | full | R01 | |
| R/format2.R | 295 | full | R01 | |
| R/get_colour.R | 462 | full | R01, R04 | methods and registration in R04 (sheet M09) |
| R/shiny.R | 1270 | full | R01, R07 | every line read in R07 (sheets S01 to S07); server driven with `shiny::testServer()` in `test-shiny-S*.R`; UI in the browser not tested (shinytest2 not installed) |
| R/back2back_plot.R | 516 | full | R01, R03 | lines 150-516 in detail in R03 (sheet E08) |
| R/add_type.R | 701 | full | R01, R04 | all add_*() helpers in R04 (sheet M05) |
| R/coord_spider.R | 433 | full | R01, R04 | ggproto implementation in R04 (sheet M07) |
| R/theme_minimal2.R | 103 | full | R01, R04 | sheet M08 |
| R/get_plot_title.R | 153 | full | R01 | |
| R/md_to_expression.R | 123 | full | R01 | |
| R/dec_mark.R | 71 | full | R01 | |
| R/labellers.R | 60 | full | R01 | |
| R/plotly.R | 57 | full | R01 | |
| R/move_layer.R | 51 | full | R04 | sheet M06 |
| R/add_mapping.R | 33 | full | R04 | sheet M06 |
| R/options.R | 49 | full | R04 | sheet M08; all options used in the code are documented |
| R/data.R | 35 | full | R04 | sheet M10 |
| R/reexports.R | 61 | full | R04 | sheet M10 |
| R/aa_pkg.R | 16 | full | R04 | |
| tests/testthat/test_plot2.R | 600 | partial | R01 | run in full; content sampled |
| DESCRIPTION, NAMESPACE | | full | R01 | |
| vignettes/, README.Rmd, NEWS.md | | full | R05 | all chunks run, including README `eval = FALSE` chunks (sheets A08 to A10) |
| man/ (generated), _pkgdown.yml | | full | R05 | R documentation checks, pkgdown check, aspell |
| roxygen of all other files | | full | R05, R07 | sheet A07; Shiny help text checked against behaviour in R07 |
| .github/workflows/ | | full | R06 | sheet T08 |

Cross-cutting (R06): every file reviewed per theme (ggplot2 internals, i18n, performance, embedding, R version, dependencies, devices, CI); sheets in `reviews/R06_themes/`.

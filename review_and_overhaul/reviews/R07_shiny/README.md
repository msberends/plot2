# R07 sheets: the Shiny app

Round R07 (maintainer's decisions of 2026-11-08: sheets per part of the app with `shiny::testServer()` tests; review only, F006 included; current code reviewed, D001 dependencies noted; server deployment in scope, so every input is treated as untrusted). Case IDs `S<sheet>.<case>`; one test file per sheet, `tests/testthat/test-shiny-S<nn>-*.R`. Cases for open findings describe the desired behaviour and are skipped with `skip_open_finding()`; each of them was run once without the skip and failed, and every other case passes.

Method: `R/shiny.R` read in full (1,270 lines); the server driven with `shiny::testServer()` (shinytest2 is not installed; a browser-level test was not needed for the server logic). Helpers added to `helper-plot2.R`: `shiny_app()`, `shiny_app_env()`, `shiny_init()`, `shiny_change()`, `shiny_file()`.

Threat model for server deployment: a visitor controls every input value (also values outside the choices of a selector, and values outside a `numericInput`'s min/max), every uploaded file and its name, and column names inside uploaded files. The R process and its file system are what needs protecting, followed by other visitors' sessions.

| Sheet | Part of the app | Test file |
|---|---|---|
| S01 | Construction: arguments, logo, inputs built from `formals(plot2)` | test-shiny-S01-construction.R |
| S02 | Data sets and upload | test-shiny-S02-data-and-upload.R |
| S03 | Code generation (`call_string()`) | test-shiny-S03-code-generation.R |
| S04 | Evaluation, messages and errors | test-shiny-S04-evaluation-and-messages.R |
| S05 | Export and copy button | test-shiny-S05-export-and-copy.R |
| S06 | Sessions and deployment | test-shiny-S06-sessions-and-deployment.R |
| S07 | Helpers (`make_input()`, `create_field()`, `label_with_dims()`, `install_shiny_deps()`) and D001 coupling | test-shiny-S07-helpers.R |

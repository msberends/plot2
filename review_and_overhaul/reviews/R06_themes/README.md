# R06 sheets: cross-cutting themes

Round R06 (maintainer's decisions of 2026-11-08: theme sheets with tests, review only, four extra themes, current code reviewed with future D001 names mentioned where relevant). Case IDs `T<sheet>.<case>`; one test file per sheet, `tests/testthat/test-themes-T<nn>-*.R`. Cases for open findings describe the desired behaviour and are skipped with `skip_open_finding()`. Timings are not tested; they come from `repro/perf_r06.R`.

Environments used (all temporary, nothing installed in the user library):

- R 4.5 on this server, CRAN ggplot2 4.0.3, for most probes;
- ggplot2 4.0.3.9000 (development, from tidyverse.r-universe.dev) in a scratch library, full test suite;
- R 4.1.3 in a disposable `rocker/r-ver:4.1.3` container, full test suite;
- a scratch library with only the hard dependencies (site library disabled);
- an installed, byte-compiled build for benchmarks.

| Sheet | Theme | Test file |
|---|---|---|
| T01 | ggplot2 4.x internals and compatibility | test-themes-T01-ggplot2-internals.R |
| T02 | Internationalisation | test-themes-T02-i18n.R |
| T03 | Performance | test-themes-T03-performance.R |
| T04 | Embedding: functions, knitr, Quarto, Shiny | test-themes-T04-embedding.R |
| T05 | R version support | test-themes-T05-r-version.R |
| T06 | Dependency footprint | test-themes-T06-dependencies.R |
| T07 | Graphics devices | test-themes-T07-graphics-devices.R |
| T08 | CI workflows | test-themes-T08-ci-workflows.R |

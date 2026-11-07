# R04 sheets for files not reviewed before

Round R04 (maintainer's decision: as R03, review only; the Shiny app moved to R07). One sheet per file or function group, M01 to M10; case IDs `M<sheet>.<case>`. Every sheet has a test file `tests/testthat/test-modules-M<nn>-<topic>.R`; every `test_that()` description starts with its case ID. Cases for open findings are written as desired behaviour and skipped with `skip_open_finding()`.

Line references are for commit `d09e04d`.

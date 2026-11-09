# R03 stage sheets for plot2_exec()

`plot2_exec()` (`R/plot2.R:615-2270`, commit `d09e04d`) is one function of about 1,650 lines. It is reviewed here per stage, following its own section markers, plus the generic `plot2()` and `back2back_plot()`. Sheets are numbered E01 to E15; case IDs are `E<sheet>.<case>` (e.g. `E05.03`), distinct from the R02 IDs.

Unlike R02, the end-to-end tests were written during the review: each sheet has a test file `tests/testthat/test-plot2_exec-E<nn>-<topic>.R`, and every `test_that()` description starts with its case ID. Cases for open findings are present as desired behaviour and skipped with `skip_open_finding()`.

Sheet sections are as in R02: Role, Lines, Contract, Branches, Findings, End-to-end test cases (ticked when the test exists; "(skipped: Fxxx)" when parked).

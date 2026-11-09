# T05 R version support

## Checked

- Static scan of `R/`: no base R function newer than R 4.1 (`str2lang()` is R 3.6; `|>` and `\(x)` are R 4.1; no `_` pipe placeholder, no base `%||%`).
- The recursive hard dependencies require at most R 4.1 (broom, dplyr, forcats, ggplot2, tidyr, purrr, stringr, scales).
- **R 4.1.3 container** (`rocker/r-ver:4.1.3`, current CRAN binaries): plot2 installs and its code works. The suite gave 1 failure and 8 errors, all from the tests: four legacy tests use `sf`, `AMR` or `ggforce` without skipping (F038), the README test needs `ggforce` (now skipped), and the R04 tests of M08 used `@` on S7 objects, which fails before R 4.3 (corrected in this round with `S7::prop()`).
- `grid (>= 4.5.0)` in Suggests contradicts `R (>= 4.1.0)` (F008).
- CI does not test R 4.1 (F109).

## Findings

F109 (new); F008, F038 extended.

## End-to-end test cases

- [x] T05.01 No base function newer than the declared R version.
- [x] T05.02 Declared R version covers the hard dependencies.
- [x] T05.03 (skipped: F008) No Suggests requiring a newer R.

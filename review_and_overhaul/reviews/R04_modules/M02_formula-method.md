# M02 plot2.formula()

`R/plot2-methods.R:399-764`

## Role

`plot2(y ~ x + category + facet, data = df)`: splits the formula text and calls `plot2_exec()` with the columns selected.

## Findings

F019 (parsing), F020 (`data`), F081 (new: formula after a pipe fails), F033 (`stop()` without `call. = FALSE`).

## End-to-end test cases

- [x] M02.01 `y ~ x` with `data`: points, axis titles from the formula.
- [x] M02.02 Third term is the category.
- [x] M02.03 Fourth term is the facet.
- [x] M02.04 Missing `data` errors with a clear message.
- [x] M02.05 (skipped: F081) `df |> plot2(y ~ x)`.
- [x] M02.06 (skipped: F019) Computed terms, e.g. `log(y) ~ x`.

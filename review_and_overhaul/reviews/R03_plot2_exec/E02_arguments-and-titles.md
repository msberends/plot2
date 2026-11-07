# E02 Argument handling, missing-argument flags, title evaluation

`R/plot2.R:768-824`

## Role

Collects extra arguments for the geom (`dots_geom`), reads the hidden `_misses.*` flags set by the methods, evaluates all titles on the raw data, and validates `summarise_function`.

## Contract

- Extra arguments not known to plot2 go to the geom, with a plot2 message listing them.
- Titles may be calculations on the data (`paste("n =", n())`).
- `summarise_function` must be a function or `NULL` (no summarising).

## Findings

F035/F036 (hidden `_misses.*` and `_label.*` arguments), F033 (`stop()` without `call. = FALSE` for `summarise_function`), F020 (`data` argument), F073 (new: `summarise_function = NULL` crashes whenever summarising is needed, although plot2's own message recommends it).

## End-to-end test cases

- [x] E02.01 An extra argument (`alpha = 0.3`) reaches the geom; message names it.
- [x] E02.02 A misspelt argument (`colr`) is reported by plot2 (message) and by ggplot2 (R warning).
- [x] E02.03 `summarise_function = "mean"` errors with a clear message.
- [x] E02.04 (partly skipped: F073) `summarise_function = NULL`: duplicates are not summed.
- [x] E02.05 Title computed from the data.
- [x] E02.06 `category.title` takes precedence over `legend.title`.
- [x] E02.07 Title referring to a missing column: plot still produced, title empty, plot2 warning.

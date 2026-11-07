# E01 Generic plot2(), dispatch and empty data

`R/plot2.R:398-610`

## Role

The exported generic. Returns an empty plot for data without rows (with titles and theme), forces the sf method for data frames with an `sfc` geometry column, and otherwise dispatches to the S3 methods.

## Contract

- Empty data: plot2 warning "No observations", `ggplot()` plus theme and only the titles that were given; `print = TRUE` prints and returns `invisible(NULL)`.
- sf detection uses `utils::installed.packages()` (F025).

## Findings

F010 (inconsistent return of `print = TRUE`), F025, F014 (the "No observations" warning is not an R warning), F002, F072 (new: long vector expressions crash).

## End-to-end test cases

- [x] E01.01 data.frame input dispatches and builds; grouped data are ungrouped (same result).
- [x] E01.02 tibble input gives the same layer data as data.frame input.
- [x] E01.03 Empty data: empty plot with the given title, plot2 warning "No observations".
- [x] E01.04 Empty data: x and y titles only set when given.
- [x] E01.05 (skipped: F010) Empty data with `print = TRUE` returns the plot invisibly.
- [x] E01.06 data.frame with an sf geometry column is plotted as a map (skip without sf).
- [x] E01.07 Atomic vector input (character) gives counted columns.
- [x] E01.07b (skipped: F072) Character vector given as an expression longer than 30 characters.
- [x] E01.08 (skipped: F002) Empty data leaves no internal state behind (its warning path consumes the RNG).

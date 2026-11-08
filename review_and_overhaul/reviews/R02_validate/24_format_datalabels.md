# 24 format_datalabels()

`R/validate.R:2917-2977`

## Role

Formats datalabel values: zeros and empty strings hidden; percentages for `y.percent`; `%n` and `%p` placeholders in `datalabels.format`; user formatting functions.

## Called from

`validate_data()` (raw rows), `summarise_data()`.

## Contract

- In: values, `datalabels.round`, `datalabels.format`, marks, `y.percent`.
- Out: character vector (or the function's output).
- Side effects: messages about rounding and ignored formats.

## Findings

F025 (row-wise `mapply(gsub)` on raw data is the main performance cost), F028 (`%p` denominator). By reading: zeros are always hidden with no option to show them; the rounding hint compares against `eval(formals(plot2)$datalabels.round)`, which depends on a variable `y.percent` existing in the calling frame.

## End-to-end test cases

- [x] 24.01 Default "%n": rounded to 1 decimal, decimal mark honoured (`decimal.mark = ","`).
- [x] 24.02 `datalabels.format = "%n (%p)"`: both parts correct.
- [x] 24.03 `y.percent = TRUE`: percent labels, `datalabels.round` honoured; other formats ignored with message.
- [x] 24.04 `datalabels.format = function(x) paste0("~", x)`.
- [x] 24.05 Zero values not labelled.
- [x] 24.06 `big.mark` in labels of large values.
- [x] 24.07 Character datalabels (`datalabels = name`) unchanged.
- [x] 24.08 Performance: labels formatted only for drawn rows (F025).

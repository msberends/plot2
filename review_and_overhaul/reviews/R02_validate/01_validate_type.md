# 01 validate_type()

`R/validate.R:16-106`

## Role

Normalises the `type` argument to a ggplot2 geom name, or chooses a type automatically from the data when `type` is unset.

## Called from

`R/plot2.R:1339` (main), `R/plot2.R:1345` (secondary y), `R/add_type.R:164`, and inside `validate_data()` (`R/validate.R:131`, `294`, `430` commented, `624`) for "quick" and preliminary validation.

## Contract

- In: `type` (character or NULL), `df` (prepared data or NULL).
- Out: geom name such as `"geom_col"`, or `""` when `type` is unset and `df` is NULL.
- Side effects: `plot2_message()` for automatic choices; reads `plot2_env$x_variable_names`, `getOption("plot2.default_type")`; calls `utils::installed.packages()` on every call.

## Branches

1. Unset type, no x: boxplot.
2. Unset type, numeric x identical to y: histogram; numeric x otherwise: point.
3. Unset type, all groups of size >= 3: boxplot; otherwise option `plot2.default_type` (default col); Sankey hint when several x variables.
4. Single-letter aliases (a, b, c, h, j, l, p, r, v, bs); "column" to col; `ggplot2::geom_x()` prefix stripping; plural "s" stripping; `geom_` prefixing.
5. Valid ggplot2 or ggbeeswarm geom; fuzzy partial match; error.
6. Length > 1: warning, first element used.

## Findings

F009 ("bar" handled before this function), F031 (fuzzy matching, whitespace or punctuation-only types become `geom_abline`), F025 (`installed.packages()` per call).

## End-to-end test cases

- [x] 01.01 No x, numeric y: boxplot layer and message "since there is no X-axis".
- [x] 01.02 Numeric x and y: point layer.
- [x] 01.03 Only one numeric variable as x: histogram.
- [x] 01.04 Character x, all groups >= 3 rows: boxplot; groups < 3: col.
- [x] 01.05 `options(plot2.default_type = "geom_point")` respected.
- [x] 01.06 Every alias (a, b, c, h, j, l, p, r, v, bs) maps to the documented geom.
- [x] 01.07 "points", "Column", "ggplot2::geom_col()" normalised.
- [x] 01.08 Unknown type errors with a message naming the type (desired, F031: suggestions, no silent fuzzy match).
- [x] 01.09 `type = " "` and `type = "()"` error (desired, F031; currently `geom_abline`).
- [x] 01.10 `type = c("col", "line")` warns and uses col.
- [x] 01.11 "beeswarm" with ggbeeswarm installed (skip otherwise); clear error without it.
- [x] 01.12 Several x variables and two categories: Sankey hint message.

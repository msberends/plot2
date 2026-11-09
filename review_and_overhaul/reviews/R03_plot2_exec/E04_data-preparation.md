# E04 Data preparation

`R/plot2.R:910-1100`, with `add_direction()`, `summarise_variable()`, `set_plot2_env()`, `get_column_name()` in `R/utils.R`

## Role

Adds the anonymous direction columns (`_var_x`, `_var_category`, `_var_facet`), evaluates y in one of four ways, adds secondary y and datalabels, then calls `validate_data()`.

## Branches for y

1. Several columns (`y = c(a, b)`, `where(is.numeric)`): reshaped long, column names become the category, `summarise_function` becomes identity, y title removed.
2. A single value per group (`n()`, `n_distinct(id)`, `mean(x)`): computed per x, category and facet.
3. A vector of other length (`range(x)`): auto-generated categories "range (1)", "range (2)", caution.
4. Row-wise values (columns, `y * 2`).

## Findings

F065 (numeric vectors are taken as column positions), F074 (new: numeric x and y with identical values give a histogram), F017 (column names used as regex patterns), F001 and F036 (state passed through `plot2_env` and `<<-`).

## End-to-end test cases

- [x] E04.01 `y = c(a, b)`: one category per column, no y title.
- [x] E04.02 `y = c(a, b)` together with `category` errors.
- [x] E04.03 `y = where(is.numeric)` selects all numeric columns.
- [x] E04.04 `y = n()` counts rows per x.
- [x] E04.05 `y = n_distinct(col)` per x and category.
- [x] E04.06 `y = range(y)`: categories "range (1)" and "range (2)", caution message.
- [x] E04.07 `y = y * 2` computed per row; y title is the expression.
- [x] E04.08 `x = c(a, b)` pasted with `sep`.
- [x] E04.09 `facet = where(is.character)` combines columns, message.
- [x] E04.10 (skipped: F065) `y = c(2, 3)` is not taken as columns 2 and 3.
- [x] E04.11 (skipped: F017) Column names with regex characters in `y = c(...)`.
- [x] E04.12 (skipped: F074) Numeric x and y with identical values give points, not a histogram.

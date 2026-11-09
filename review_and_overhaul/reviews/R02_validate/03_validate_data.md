# 03 validate_data()

`R/validate.R:126-676` (550 lines; the largest function in the package)

## Role

Turns the user's data plus the evaluated directions (`_var_x`, `_var_y`, `_var_category`, `_var_facet`, `_var_y_secondary`, `_var_datalabels`) into the prepared data frame used by every later step. It guesses missing directions, coerces classes, handles NA and infinite values, completes data, sorts, summarises duplicates, reshapes for Sankey plots and applies `*.max_items`.

## Called from

`R/plot2.R:1068` only (receives most `plot2()` arguments through `...`).

## Contract

- In: `df` with `_var_*` columns; `misses_x`, `misses_category`; about 30 arguments through `...`.
- Out: prepared data frame (tibble or data.frame), sorted by x.
- Side effects: writes `plot2_env$mapping_*` via `set_plot2_env()`, `plot2_env$y_secondary_factor`, `plot2_env$na_replaced*`; many messages, cautions and warnings.

## Branches (in order of execution)

1. Choose y: prefer column `n`; else first numeric column; several sub-cases depending on x, type and the number of numeric columns (lines 140-218).
2. Coerce y (difftime, AMR mic, other numeric-mode classes) and x to double.
3. Choose x when missing (first eligible column, skipping the one used for y).
4. Choose category when x and category are missing (first character or factor column); for sf, first numeric column.
5. Drop category, facet or datalabels when given `FALSE`, or datalabels for continuous geoms.
6. Secondary y-axis rescaling factor.
7. Surrogate label columns named after the expressions (`"c(a, b)"`, `"n()"`).
8. Datalabels: `TRUE` resolved to y, category (tiles) or first character column (sf); formatting.
9. x and category to character for years (2000-2050), months (1:12), discrete geoms, or when `x.sort` is set.
10. Remove infinite y; convert integer64.
11. Complete x, category, facet (`complete_direction()`).
12. NA in x, category, facet: remove (`na.rm`) or replace (`na.replace`); NA in y: removed with warning.
13. Remove years from dates when the category holds the years.
14. Sort x, category, facet (`sort_data()`).
15. Sankey reshaping.
16. Summarise non-distinct rows with `summarise_function`.
17. `set_max_items()`, then sort by x.

## Findings

F007, F012, F013, F022, F025, F027, F036, F049 (via summarise), F048 (via complete), F017 (data as regex in labels). By reading, not filed separately: a user column literally named `y` is never chosen as x (line 239); `get_x_name(df) == numeric_cols` at line 198 errors with "argument is of length zero" if the name cannot be recovered; the function is too large to test or reason about and should be split along the branches above (see F036).

## End-to-end test cases

- [x] 03.01 No y, column `n` present: y = n, message.
- [x] 03.02 No y, no x, two numeric columns: x = first, y = second.
- [x] 03.03 No y, one numeric column, no other columns: histogram.
- [x] 03.04 No y, x is the only numeric column, explicit non-histogram type: error message suggests histogram.
- [x] 03.05 difftime y coerced to double, message.
- [x] 03.06 POSIXct column before the numeric one is never chosen as y (F012). (parked on F012 until it was fixed)
- [x] 03.07 Missing x: first non-y column chosen, message.
- [x] 03.08 Missing x and category: first character column becomes category.
- [x] 03.09 `category = FALSE`, `facet = FALSE`, `datalabels = FALSE` remove the direction.
- [x] 03.10 Constant 0 or 1 numeric columns as category or datalabels are kept as data (F007). (parked on F007 until it was fixed)
- [x] 03.11 Category or datalabels containing NA do not crash (F007). (parked on F007 until it was fixed)
- [x] 03.12 Surrogate column for `y = n()` and for `x = c(a, b)` present in `p$data`, labels match.
- [x] 03.13 `datalabels = TRUE` uses y; for tiles uses category.
- [x] 03.14 Years heuristic only for discrete geoms (F013, desired behaviour after decision). (parked on F013 until it was fixed)
- [x] 03.15 Infinite y removed with message; -Inf too.
- [x] 03.16 integer64 y converted (skip without bit64).
- [x] 03.17 `na.rm = TRUE` removes rows with NA in x, category or facet; message count correct (F027). (parked on F027 until it was fixed)
- [x] 03.18 `na.replace = "Unknown"` for character and factor columns; numeric NA untouched.
- [x] 03.19 NA in y removed with a warning that can be caught (F014). (parked on F014 until it was fixed)
- [x] 03.20 Date x with category equal to the year: years unified, message.
- [x] 03.21 Non-distinct rows summarised with `summarise_function`, caution message; `summarise_function = NULL` disables. (parked on F073 until it was fixed)
- [x] 03.22 Sankey data reshaped: `_sankey_x` levels follow the order of `x` variables.
- [x] 03.23 Output sorted by x.
- [x] 03.24 tibble, data.frame, data.table and grouped_df inputs give identical plots.

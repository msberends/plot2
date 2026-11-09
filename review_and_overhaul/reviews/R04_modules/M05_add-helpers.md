# M05 add_type() and the add_*() helpers

`R/add_type.R`

## Role

`add_type()` adds any ggplot2 geom by name; `add_line()`, `add_point()`, `add_col()`, `add_errorbar()`, `add_smooth()` compute their data from the plot data (per category and facet), with optional legend entries (`legend.value`); `add_sf()` adds another sf layer with optional labels.

## Contract

- `add_line(y = 3)` gives a horizontal line, `x = 2` a vertical line, `y = mean(y)` one line per category or facet; without arguments a line through the data.
- Empty plots are returned unchanged (F040).

## Findings

F040 (silent no-op), F082 (new: `add_errorbar()` without category fails), F084 (new: `add_sf()` colours, geometry name, nudge default), F085 (new: `add_smooth()` legend on non-date x).

## End-to-end test cases

- [x] M05.01 `add_line(y = 3)`: a horizontal line at 3.
- [x] M05.02 `add_line(x = 2)`: a vertical line.
- [x] M05.03 `add_line(y = mean(y))` per category and per facet.
- [x] M05.04 `add_line()` without values: a line through the data.
- [x] M05.05 `add_line(colour = "red")` and `legend.value` (legend entry).
- [x] M05.06 `add_line()` on a plain ggplot2 plot.
- [x] M05.07 `add_point(x, y)`: one new point.
- [x] M05.08 `add_col()` adds a column layer.
- [x] M05.09 `add_errorbar()` with a category: one error bar per row.
- [x] M05.10 (skipped: F082) `add_errorbar()` without a category.
- [x] M05.11 `add_smooth()` adds a smooth layer.
- [x] M05.12 (skipped: F085) `add_smooth(legend.value = )` on a discrete x-axis.
- [x] M05.13 `add_type("line")`, `move`, invalid and missing type.
- [x] M05.14 add_*() on an empty plot returns it unchanged.
- [x] M05.15 `add_sf()` adds an sf layer (skip without sf).
- [x] M05.16 (skipped: F084) `add_sf()` uses `datalabels.colour` and accepts other geometry column names.

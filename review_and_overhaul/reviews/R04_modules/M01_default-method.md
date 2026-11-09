# M01 plot2.default(): vectors, tables and other objects

`R/plot2-methods.R:23-397`

## Role

Entry point for everything that is not a data frame, matrix, formula, sf or freq object. Atomic vectors become a small data frame (counts for character, factor and logical; index and value for numeric); other objects go through `fortify_df()` (e.g. `lm`, `ts`, tables). A single string naming a data frame is looked up with `get()` (for `ellmer::tool()`). Without `type`, vectors use `getOption("plot2.default_type", "geom_col")`.

## Findings

F030 (`get()` lookup; numeric vector with `y` leaves `new_df` undefined), F072 (long expressions), F035 (argument duplication).

## End-to-end test cases

- [x] M01.01 Numeric vector: columns with x = index, y = values.
- [x] M01.02 Character vector: counts per value, sorted alphabetically.
- [x] M01.03 Factor vector: level order kept.
- [x] M01.04 Logical vector: counts for FALSE and TRUE.
- [x] M01.05 `plot2.default_type` option sets the type for vectors.
- [x] M01.06 One-dimensional table: counts as y.
- [x] M01.07 `lm` object: points (via fortify).
- [x] M01.08 A function errors with a clear message.
- [x] M01.09 (skipped: F030) Numeric vector with `y` given.
- [x] M01.10 (skipped: F030) A single string is plotted as a value, not looked up as an object.

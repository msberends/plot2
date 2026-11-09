# 19 complete_direction()

`R/validate.R:2601-2632`

## Role

Implements `x.complete`, `category.complete`, `facet.complete`: adds missing values of the direction (via `tidyr::complete()` and `full_seq()`) and fills y with a value (default 0).

## Called from

`validate_data()` (`R/validate.R:472-474`).

## Contract

- In: `df`, direction name, whether it exists, filler (`TRUE` means 0).
- Out: completed `df`.

## Branches

Double values (period from the number of decimals), Date (1 day), other (period 1).

## Findings

F048 (fails for character and factor values; POSIXct completed per second, exploding the data). By reading: datalabels of the added rows are not filled; `names(fill_list)[2] <- get_y_name(df)` errors if the name is NULL.

## End-to-end test cases

- [x] 19.01 Integer x with gaps, `x.complete = TRUE`: missing x added with y 0.
- [x] 19.02 `x.complete = NA`: added rows have y NA (then removed with warning, behaviour to decide).
- [x] 19.03 Date x with gaps: daily completion.
- [x] 19.04 POSIXct x: sensible period, not seconds (F048).
- [x] 19.05 Character x: completes over observed values within categories, or a clear error (F048).
- [x] 19.06 `category.complete = TRUE` with numeric category.
- [x] 19.07 Added rows get datalabels consistent with the filler.

Note (2026-11-08): 19.02 codes the current behaviour (added NA rows are removed with a warning); 19.07 codes that added rows get no datalabel (NA), so the zero bars stay unlabelled. Both are open to the maintainer's view.

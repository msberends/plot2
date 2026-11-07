---
id: F008
title: Core functionality depends on packages listed only in Suggests
severity: high
type: dependency
area: dependencies
status: open
verification: reading
effort: S
api_change: no
locations: [DESCRIPTION, R/validate.R:2730, R/validate.R:2736, R/utils.R:158, R/validate.R:876, R/validate.R:902, R/format2.R:177]
related: [F029]
origin: R01 §2.1
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F008 Runtime dependencies listed in Suggests

## Summary

Several packages in `Suggests` are needed on the default code path: `stringr` (default sorting of character x), `cli` (every message), `rlang` (41 calls), `scales` (x and y scales), `lubridate` (automatically chosen "mmm"/"mmmm" date labels). `grid (>= 4.5.0)` in Suggests contradicts `R (>= 4.1.0)`.

## Evidence

- `R/validate.R:2730`: `rlang::check_installed("stringr")` in `sort_data()`, reached for every character x because `x.sort` defaults to `TRUE` for characters (`R/validate.R:548`).
- `R/utils.R:158`: `cli::cli_alert_info()` unconditionally.
- `R/validate.R:876`, `902`: `scales::reverse_trans()`, `scales::breaks_pretty()`.
- `R/format2.R:177-206`: `lubridate::month()` and `lubridate::quarter()`.

## Reproduction

```r
# Manual: in a library without stringr, run
#   Rscript -e 'plot2::plot2(data.frame(g = c("b", "a"), v = 1:2), g, v)'
# Expected after fix: plot is produced without prompting to install anything.
```

## Impact

Fails or prompts for installation in minimal or locked-down environments (CI, servers, Docker), and in non-interactive sessions it errors.

## Proposed fix

Move `cli`, `rlang`, `scales` to Imports (they are already hard dependencies of ggplot2/dplyr, so no extra install cost). Replace `stringr::str_sort(numeric = TRUE)` with an internal natural sort, or move `stringr` to Imports. Replace `lubridate::month()`/`quarter()` with base R. Fix the `grid` version requirement.

## History

- 2026-11-07: filed in R01 (by reading).

---
id: R02
title: Function-by-function review of R/validate.R with end-to-end test specification
status: verified
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# R02 Function-by-function review of R/validate.R

## Scope

All 24 functions in `R/validate.R` (2,977 lines), requested by the maintainer together with ideas I001 and I002. For each function: role, call sites, contract and side effects, branches, defects, and the end-to-end test cases needed. Builds on R01, which had already read the file in full; R02 went deeper per function and probed behaviour experimentally.

## Method

1. Inventory of functions and call sites (grep over `R/`).
2. Per function: re-reading of the body, listing of branches and side effects.
3. About 35 targeted experiments through the public `plot2()` API, built with `ggplot_build()`, plus direct calls to internals where useful.
4. Writing one sheet per function in `R02_validate/` (specification for I002).

## Coverage

All 24 functions reviewed in depth. Sheets: `R02_validate/01_validate_type.md` to `R02_validate/24_format_datalabels.md`.

| # | Function | Lines | Findings |
|---|---|---|---|
| 01 | validate_type | 16-106 | F009, F025, F031 |
| 02 | validate_legend.position | 108-122 | F050 |
| 03 | validate_data | 126-676 | F007, F012, F013, F017, F022, F025, F027, F036, F048, F049 |
| 04 | validate_taxonomy | 679-733 | F018, F058 |
| 05 | validate_x_scale | 736-946 | F029, F041, F052, F054 |
| 06 | validate_y_scale | 951-1301 | F001, F003, F022, F024, F041, F042, F043, F044, F052, F063 |
| 07 | validate_category_scale | 1305-1556 | F003, F004, F023, F041, F059 |
| 08 | generate_geom | 1559-1758 | F045, F046, F060 |
| 09 | validate_colour | 1760-1888 | F016, F047, F061 |
| 10 | validate_size | 1890-1903 | |
| 11 | validate_width | 1905-1914 | |
| 12 | validate_linewidth | 1916-1931 | |
| 13 | validate_markdown | 1933-1968 | F018 |
| 14 | validate_title | 1971-2044 | F018, F025, F056 |
| 15 | validate_theme | 2047-2248 | F005, F032, F051, F057 |
| 16 | validate_facet | 2251-2330 | F052 |
| 17 | generate_datalabels | 2333-2501 | F026, F028, F055 |
| 18 | validate_font | 2506-2598 | F001, F011 |
| 19 | complete_direction | 2601-2632 | F048 |
| 20 | validate_sorting | 2634-2659 | |
| 21 | sort_data | 2662-2771 | F008, F013 |
| 22 | set_max_items | 2775-2870 | F062 |
| 23 | summarise_data | 2873-2914 | F049, F053 |
| 24 | format_datalabels | 2917-2977 | F025, F028 |

## Outcome

- 23 new findings, F041 to F063 (4 high, 9 medium, 10 low at filing); 3 need a maintainer decision (F050, F052, F063). F031 extended with new evidence.
- New high-severity defects: F041 (negative values get no axis breaks for small integers), F045 (area plots dodged sideways), F048 (`x.complete` fails for categories and explodes date-times), F049 (a column named `n` crashes summarising).
- Structural observations: `validate_data()` (550 lines, 17 stages) should be split along its stages; the file mixes pure helpers (sizes, sorting, formatting) with functions that write shared state; three copies of the same break logic exist (F041).
- 231 end-to-end test cases specified, forming the backlog for I002.

Suggested next round: R03 on `plot2_exec()` itself (the orchestration in `R/plot2.R`), which calls these functions and holds the remaining special-type logic (UpSet, Sankey, dumbbell, spider, back-to-back).

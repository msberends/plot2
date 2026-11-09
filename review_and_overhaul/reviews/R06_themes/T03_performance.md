# T03 Performance

`repro/perf_r06.R`, run detached on this server (16 cores, R 4.5, ggplot2 4.0.3) against an installed, byte-compiled build (`PLOT2_LIB`). Median of three runs after a warm-up; "call" is `plot2()`, "build" is `ggplot_build()`.

## Results (seconds)

| Scenario | 1e3 rows | 1e4 | 1e5 | 1e6 |
|---|---|---|---|---|
| col, default datalabels | 0.11 | 0.17 | 0.77 | 7.12 |
| col, `datalabels = FALSE` | 0.10 | 0.10 | 0.13 | 0.30 |
| boxplot | 0.10 | 0.10 | 0.16 | 0.59 (+0.74 build) |
| point | 0.10 | 0.17 | 0.97 | 8.69 |
| histogram | 0.10 | 0.12 | 0.28 | 1.79 |
| date x, `n()` | 0.12 | 0.14 | 0.14 | 0.15 |

- **Fixed overhead:** 0.10 s per call on five rows, against 0.003 s for plain ggplot2; 38% in `validate_theme()` (F110, new).
- **Datalabels formatted per raw row and then discarded** dominate large data: 7.1 s versus 0.3 s for columns, and 8.7 s for scatter plots that never show labels (F025, extended).
- Number of x values, category values and facets (1e5 rows, `type = "col"`): the call stays at 0.7 to 0.8 s (the F025 cost); build grows with ggplot2's own cost (10,000 bars: 1.7 s; 50 facets: 0.64 s, plain ggplot2 0.23 s on aggregated data).
- Special types on 1e4 rows: Sankey 0.19 + 0.33 s, UpSet 0.34 s, back-to-back 0.44 s, spider 0.13 s.
- `devtools::load_all()` gives practically the same timings as the installed build (checked: the earlier suspicion of a byte-compilation artefact was wrong).
- Found while benchmarking: Sankey diagrams and all other types cannot be drawn from data without a numeric column (F106, new).

## Findings

F106, F110 (new); F025 extended.

## End-to-end test cases (structural, no timings)

- [x] T03.01 (skipped: F025) No datalabel formatting for scatter plots.
- [x] T03.02 1,000 x values: no datalabels.
- [x] T03.03 A date axis on 1e4 rows is aggregated to at most 31 bars.
- [x] T03.04 (skipped: F106) Without y or a numeric column, rows are counted.
- [x] T03.05 (skipped: F106) Sankey from raw rows.

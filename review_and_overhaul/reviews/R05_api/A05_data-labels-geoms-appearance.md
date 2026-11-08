# A05 plot2(): data handling, datalabels, geoms and appearance

`R/plot2.R:84-85, 124-188`.

## Checked against behaviour

| Claim | Result |
|---|---|
| `datalabels` left blank: at most 25 labels | holds; but usage shows `datalabels = TRUE`, which removes the limit (F093, F094) |
| `datalabels.format = NULL` does not transform | holds (`datalabels.round` then ignored) |
| `x.sort` manual vector, missing values appended | holds |
| `x.max_items` with `freq-desc` gives top n | holds (n includes the rest group; F062) |
| `*.max_txt` placeholders `%n`, `%p` | `%p` counts rows (F062) |
| Size defaults: 2 point, 4 UpSet, 5 dumbbell, 0.75 otherwise | point checked |
| Width defaults: 0.66 boxplot/violin/jitter, 0.5 otherwise | holds |
| Linewidth defaults: 0.25 area, 0.5 line | holds |
| Histogram smooth default `TRUE` | holds |
| `theme = NULL` gives `theme_grey()` | holds |
| `jitter_seed` reproducible | holds (RNG side effect F002) |
| `na.replace` | holds |
| `summarise_function = NULL` | crashes (F073) |
| `font`: informative error in R Markdown without `fig.showtext` | holds; downloads without consent (F011) |
| `print = TRUE` | prints and returns the plot |

## Findings

No new; F002, F011, F062, F073, F093, F094 (existing).

## End-to-end test cases

- [x] A05.01 Datalabels left blank: shown up to 25, not beyond.
- [x] A05.02 `datalabels = TRUE` forces labels beyond 25.
- [x] A05.03 `datalabels.format = NULL`.
- [x] A05.04 Manual `x.sort` with missing values appended.
- [x] A05.05 `x.max_items` with `freq-desc`.
- [x] A05.06 Size, width and linewidth defaults.
- [x] A05.07 Histogram density smooth by default, removable.
- [x] A05.08 `theme = NULL`.
- [x] A05.09 `jitter_seed`.
- [x] A05.10 `na.replace`.
- [x] A05.11 (skipped: F073) `summarise_function = NULL`.

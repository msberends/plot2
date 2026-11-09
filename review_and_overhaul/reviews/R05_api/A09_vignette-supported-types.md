# A09 Vignette "Supported Plot Types"

`vignettes/supported_types.Rmd` (217 lines).

## Checked

- Rendering on this server stops at the set-up chunk: `ggplot2movies` (Suggests) is not installed and is used unconditionally (F102). After installing `ggplot2movies`, `ggbeeswarm` and `plotly` into a temporary library, the vignette renders without warnings.
- Uses `type = "line-point"`, not documented in `?plot2` (F098).
- Types shown: column, bar, line, line-point, point, area, violin, boxplot, histogram, sf, beeswarm, back-to-back, Sankey, spider, UpSet, dumbbell. Not shown: `barpercent`, `linedot` under that name, jitter, ribbon, tile (matrix).

## Findings

F102 (new); F098.

## End-to-end test cases

- [x] A09.01 Every chunk runs without error or warning (skips without the suggested packages).
- [x] A09.02 (skipped: F102) The vignette guards its suggested data package.

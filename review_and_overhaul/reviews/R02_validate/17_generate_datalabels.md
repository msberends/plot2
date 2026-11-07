# 17 generate_datalabels()

`R/validate.R:2333-2501`

## Role

Adds data labels as two layers: a background `geom_label()` (made wider by padding the text with dashes) and a `geom_text()` on top, positioned per stacking, orientation, transformation, tiles and sf.

## Called from

`R/plot2.R:2161`.

## Contract

- In: plot, `df`, `type`, `width`, stacking, datalabel styling, `datalabels.centroid` (sf), `font`, `reverse`, `y.transform`, `horizontal`, `misses_datalabels`, `markdown`.
- Out: plot with two extra layers; the background layer moved to the bottom.
- Side effects: caution when more than 25 labels would be drawn by default.

## Branches

1. More than 25 rows and datalabels not requested: none added.
2. Background colour from the theme panel or transparent; opacity 40% or 75%.
3. Offsets for vertical, horizontal or 90-degree labels; reverse transformation.
4. Position fill, stack, dodge2.
5. sf: `geom_sf_label()`/`geom_sf_text()` with centroid or point-on-surface and invalid-geometry repair.

## Findings

F055 (background label aes captures an external vector; fragile when data change), F026 (factor values crash `nchar()`), F028 (percentages of the grand total). By reading: only `datalabels.angle == 90` is treated as vertical (270 and -90 are not); the comment says "back + 1" but the label layer is moved to the very back.

## End-to-end test cases

- [ ] 17.01 Columns with up to 25 bars: text and label layers present, labels equal formatted y.
- [ ] 17.02 26 bars: no labels, caution; `datalabels = TRUE` forces them.
- [ ] 17.03 `stacked = TRUE`: labels centred in segments (position stack, vjust 0.5).
- [ ] 17.04 `stacked_fill = TRUE`: labels centred.
- [ ] 17.05 `horizontal = TRUE`: labels to the right of the bars.
- [ ] 17.06 `datalabels.angle = 90` and `270` (both vertical, desired).
- [ ] 17.07 `datalabels.colour`, `datalabels.colour_fill`, `datalabels.size`, `datalabels.lineheight`.
- [ ] 17.08 Tile plot: labels from the category, centred.
- [ ] 17.09 Background label layer is drawn first (layer order).
- [ ] 17.10 Replacing the plot data afterwards (`p %+% new_data`) does not error (F055).
- [ ] 17.11 sf plot: labels at centroids; `datalabels.centroid = FALSE` uses point on surface (skip without sf).

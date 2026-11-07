# 08 generate_geom()

`R/validate.R:1559-1758`

## Role

Creates the main geom layer with plot2's defaults per geom (width, size, line width, position, colours, outliers, quantiles, binwidth) and merges extra user arguments from `...`.

## Called from

`R/plot2.R:1720` (primary), `R/plot2.R:1751` (secondary y).

## Contract

- In: `type`, `df`, `stacked`, `stacked_fill`, `horizontal`, `width`, `size`, `linetype`, `linewidth`, `reverse`, `na.rm`, `violin_scale`, `jitter_seed`, `binwidth`, `cols`, `original_colours`, `dots_geom`, `mapping`.
- Out: a ggplot2 layer.
- Side effects: message for the automatic binwidth; warning for loosely supported types.

## Branches

col/bar, area, line/path, point, jitter, beeswarm, boxplot, violin, histogram (automatic binwidth), density/polygon (also spider), sf, blank, tile, raster, fallback for any other geom. Position: stack, fill or dodge2.

## Findings

F045 (areas dodged instead of overlaid or stacked), F046 (histogram of constant values), F060 (fallback passes `width` and `size` to every geom), F009/F031 (which type arrives here). By reading: user arguments in `...` override plot2's defaults silently and may also be applied to the secondary-axis layer.

## End-to-end test cases

- [ ] 08.01 One test per supported type: correct geom class in the built layer, no warnings.
- [ ] 08.02 col: width 0.5, dodge2 with category; `stacked = TRUE` stack; `stacked_fill = TRUE` fill; `reverse = TRUE` reverses the stack order.
- [ ] 08.03 area with category, not stacked: identity position (F045).
- [ ] 08.04 line: round line ends; `linetype` and `linewidth` honoured.
- [ ] 08.05 point and jitter: size 2; `jitter_seed` gives reproducible positions.
- [ ] 08.06 boxplot: whisker error bars present, outliers at 75% opacity, white fill with category and original colours.
- [ ] 08.07 violin: quartile lines, `violin_scale` honoured.
- [ ] 08.08 histogram: automatic binwidth message and value; constant data does not fail (F046); `binwidth` honoured.
- [ ] 08.09 density: no y used, "Ignoring y" message.
- [ ] 08.10 tile and raster.
- [ ] 08.11 Extra argument in `...` (e.g. `alpha = 0.3`) reaches the layer, message lists it.
- [ ] 08.12 Unsupported geom (e.g. "step"): warning, plot still builds without "unknown parameter" warnings (F060).
- [ ] 08.13 beeswarm (skip without ggbeeswarm).

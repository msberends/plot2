---
id: F143
title: Long rotated datalabels can run beyond the panel, because the value axis expansion assumes unrotated labels
severity: low
type: bug
area: datalabels
status: verified
verification: verified
effort: M
api_change: no
locations: [R/validate.R:1005]
related: [F140]
origin: found while extending F140
commit: 65dcae8
created: 2026-11-09
updated: 2026-11-09
---

# F143 Rotated datalabels beyond the panel

## Summary

The default `y.expand = 0.25` leaves room above the bars for a label one line high. A rotated label (`datalabels.angle = 90` or `270`) needs room for its full length instead. With a long label such as "1 234 567" on the highest bar, it runs past the top of the panel, and with a reversed axis it crosses the x-axis labels.

## Reproduction

```r
d <- data.frame(g = c("a", "b"), n = c(25, 1234567))
plot2(d, g, n, datalabels.angle = 90)
plot2(d, g, n, datalabels.angle = 270, y.transform = "reverse")
```

## Proposed fix

The room needed depends on the label length in physical units relative to the panel height, which is only known when drawing. Options:

1. A heuristic at build time: enlarge the default expansion with the length of the longest label (assumes a typical device size).
2. Compute it at draw time, e.g. in a custom `ggplot_build` or `print` method, so the expansion fits the actual device.
3. Leave it to the user (`y.expand`), and document that rotated labels may need more room.

## History

- 2026-11-09: found while extending F140; needs the maintainer's choice of approach.
- 2026-11-09: maintainer decided (AskUserQuestion): a heuristic at build time. Fixed: `fit_rotated_datalabels()` runs after the datalabels are formatted, when `y.expand` and `y.limits` are not set, for column plots with rotated labels (identity or reversed axis, not stacked, not horizontal). It estimates the share of the panel each label takes (characters plus the two-space padding, times 0.6 of the label size, times |sin(angle)|, over an assumed panel height of 65 mm, about a 6 by 4 inch figure) and enlarges the expansion on the side of the labels so that every bar's label fits, per facet; the room is only ever increased. Checked visually at 90, 270, 45 and 270 reversed, in small panels (about 63 mm) and at 7 by 5 inches (some extra room, as intended). Test 17.12; documented under the datalabel settings.

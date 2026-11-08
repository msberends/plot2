---
id: F143
title: Long rotated datalabels can run beyond the panel, because the value axis expansion assumes unrotated labels
severity: low
type: bug
area: datalabels
status: needs-decision
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

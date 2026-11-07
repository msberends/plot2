# E03 Type shortcuts and early normalisation

`R/plot2.R:826-908`

## Role

Translates plot2's own types into a geom plus settings: barpercent, back-to-back, linedot, upset, dumbbell, sankey, spider, "bar" (horizontal columns). Also normalises `decimal.mark`/`big.mark` clashes and `category.type`.

## Branches

barpercent (col, horizontal, freq-desc, "%n (%p)", 10 items); back-to-back (col, horizontal, 10 items); linedot (line, points added later); upset; dumbbell (point, horizontal); sankey (line placeholder, datalabels off, expansions, empty x title); spider (polygon); any type matching "bar" (col, horizontal).

## Findings

F009 ("bar" also matches errorbar and crossbar).

## End-to-end test cases

- [x] E03.01 barpercent: horizontal, sorted by frequency, labels "n (p%)".
- [x] E03.02 barpercent: at most 10 items by default.
- [x] E03.03 "bar": horizontal columns.
- [x] E03.04 linedot ("ld", "line-dot", "pointline"): one line and two point layers.
- [x] E03.05 dumbbell ("d"): points and segments, horizontal.
- [x] E03.06 sankey ("s"): parallel-sets layers (skip without ggforce).
- [x] E03.07 spider ("radar"): polygon with `CoordSpider`.
- [x] E03.08 upset ("u"): patchwork of four plots.
- [x] E03.09 (skipped: F009) "errorbar" is not converted to horizontal columns.
- [x] E03.10 `decimal.mark` equal to `big.mark`: big mark becomes a space.
- [x] E03.11 `category.type = "color"` is accepted as "colour"; an invalid value errors.
- [x] E03.12 A non-character `type` errors.

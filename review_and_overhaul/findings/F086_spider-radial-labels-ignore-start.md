---
id: F086
title: coord_spider() radial labels ignore start and pile up at the centre
severity: low
type: bug
area: scales
status: verified
verification: verified
effort: S
api_change: no
locations: [R/coord_spider.R:387, R/coord_spider.R:391]
related: []
origin: R04 sheet M07
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F086 coord_spider() radial labels ignore start and pile up at the centre

## Summary

The radial (value) labels are placed at x = 0.5 npc and y = `(r * cos(start) + 1) / 2` (`R/coord_spider.R:387-391`), i.e. on the vertical axis only. With `start = 0` this is the first spoke; for any other `start` the labels leave the spoke, and with `start = pi / 2` they all sit at the centre on top of each other. Text sizes are fixed in points and do not follow the theme's font family or colour.

## Reproduction

```r
library(ggplot2)
sp <- data.frame(dim = c("a", "b", "c", "d"), score = c(3, 5, 2, 4))
b <- ggplot_build(ggplot(sp, aes(dim, score, group = 1)) + geom_polygon(fill = NA, colour = "black") + coord_spider(start = pi / 2))
pp <- b$layout$panel_params[[1]]
bg <- b$layout$coord$render_bg(pp, theme_grey())
txt <- Filter(function(k) inherits(k, "text"), bg$children)
print(unique(vapply(txt[-(1:4)], function(k) paste(as.character(k$x), as.character(k$y)), "")))
# Expected after fix: different positions along the first spoke, not all "0.5npc 0.5npc".
```

## Proposed fix

Place label i at `x = (r_i * sin(start) + 1) / 2`, `y = (r_i * cos(start) + 1) / 2`; take font family, colour and size from the theme (`axis.text`).

## History

- 2026-11-08: filed in R04, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; radial labels are placed along the first spoke at angle start, with font family and colour from the theme's axis.text (size still from r_label_size). Checked visually with start = pi / 2.

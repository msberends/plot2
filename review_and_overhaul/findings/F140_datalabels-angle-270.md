---
id: F140
title: With datalabels.angle = 270, labels are centred on the end of the bar instead of placed beyond it
severity: low
type: bug
area: datalabels
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2533]
related: []
origin: I002 sheet 17 (case 17.06)
commit: a381436
created: 2026-11-08
updated: 2026-11-08
---

# F140 datalabels.angle = 270

## Summary

`generate_datalabels()` sets the label offsets for vertical text only when `datalabels.angle == 90` (or `horizontal = TRUE`). For 270 degrees the offsets of unrotated text are kept (`hjust = 0.5`, `vjust = -0.75`), so the vertical labels are centred on the end of the bar and overlap it.

## Reproduction

```r
p <- plot2(data.frame(g = c("a", "b"), n = c(12, 30)), g, n, datalabels.angle = 270)
ggplot2::layer_data(p, 3)[, c("angle", "hjust", "vjust")] # hjust 0.5, vjust -0.75
```

## Proposed fix

Treat 270 like 90 with mirrored horizontal justification (the text reads downwards, so its end is at the bar).

## History

- 2026-11-08: found while coding I002 sheet 17 (case 17.06); fixed in the same session (`hjust = 1.25` for the text, `1.1` for the background label, mirrored for a reversed y axis); checked visually. Test 17.06.

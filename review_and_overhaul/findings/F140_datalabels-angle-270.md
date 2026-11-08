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
updated: 2026-11-09
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
- 2026-11-09: extended at the maintainer's request ("no large gaps"): the gap between bar and label was relative to the label's own length (`hjust = -0.25` and similar), so long labels such as "1 234 567" stood far from their bar, in horizontal plots too. Every rotated label (any angle, not only 90 and 270) and every label in a horizontal plot is now anchored exactly at the bar end (`hjust` 0 or 1, mirrored for a reversed axis) with a fixed padding of two spaces inside the label on the side of the bar (`datalabels_pad_side()`, `datalabels_pad()`). Unrotated labels above vertical bars are unchanged. Checked visually at 0, 45, 90 and 270 degrees, reversed, horizontal and back-to-back; snapshots bar-horizontal and back-to-back updated. Test 17.06 rewritten, 17.05 extended; `datalabels_of()` trims the padding.

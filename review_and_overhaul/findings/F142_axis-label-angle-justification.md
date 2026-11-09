---
id: F142
title: Rotated x labels are justified by fixed rules that misalign them at 90 and 270 degrees and in horizontal plots
severity: low
type: bug
area: theme
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2271]
related: [F051, F140]
origin: maintainer (2026-11-09), while reviewing F140
commit: 65dcae8
created: 2026-11-09
updated: 2026-11-09
---

# F142 Rotated axis label justification

## Summary

`validate_theme()` set `angle` and only `hjust` for the x labels, from fixed angle ranges (right aligned for 11 to 170 degrees, and so on). `vjust` stayed at the theme default, so labels at 90 and 270 degrees were not centred on their ticks. In horizontal plots the same rules were applied to the labels on the left axis, where they do not fit: at 90 degrees the labels overlapped, at 45 degrees they shifted away from their bars.

## Reproduction

```r
d <- data.frame(g = c("a", "Somewhat longer", "A very long category name"), n = c(5, 8, 3))
plot2(d, g, n, x.label_angle = 90)                     # not centred on the ticks
plot2(d, g, n, x.label_angle = 90, horizontal = TRUE)  # labels overlap
```

## Proposed fix

Use ggplot2's `guide_axis(angle = )`, whose heuristic justifies labels for every angle and axis position; keep the theme route when the user sets `x.label_align`.

## History

- 2026-11-09: raised by the maintainer while reviewing F140 ("that goes for x.label_angle too"); fixed: `plot2_exec()` adds `guides(x = guide_axis(angle = x.label_angle))` (position `y` after `coord_flip()`) unless `x.label_align` is set. Checked visually at 30, 45, 90, 270 and -45 degrees and horizontally at 45 and 90. Tests 15.07 and F051 (test-regression-phase1.R) updated; new helper `axis_guide_angle()`.

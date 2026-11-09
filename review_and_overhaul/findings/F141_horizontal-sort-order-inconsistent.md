---
id: F141
title: Horizontal plots reverse the reading order for factors, "alpha" and FALSE, unlike the other sort methods
severity: medium
type: bug
area: sorting
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2822, R/validate.R:2833]
related: [F051]
origin: I002 sheet 20 (case 20.02)
commit: 735f291
created: 2026-11-08
updated: 2026-11-08
---

# F141 Horizontal sort order inconsistent

## Summary

For horizontal plots, `validate_sorting()` swaps "asc" and "desc" so that the values read from top to bottom in the same order as a vertical plot reads from left to right. This only worked for character values sorted with `TRUE`, "asc", "desc" and the frequency methods, and for manual vectors. It failed for:

- `x.sort = "alpha"`: not swapped (no "asc" in the name), so it read descending, unlike its documented equivalent "asc";
- `x.sort = FALSE`, "order", "inorder": not reversed, so the data order read from bottom to top;
- factors: the factor branch of `sort_data()` reversed the levels for horizontal plots itself, on top of the swap, so both cancelled out and the levels read from bottom to top (with the default `x.sort`, e.g. age groups in a dumbbell plot read 75+ to 0-11 from the top).

## Reproduction

```r
d <- data.frame(x = c("b", "A10", "c", "A2"), y = c(5, 1, 9, 3))
rev(ggplot2::layer_scales(plot2(d, x, y, x.sort = "asc", horizontal = TRUE))$x$get_labels())   # A2 A10 b c
rev(ggplot2::layer_scales(plot2(d, x, y, x.sort = "alpha", horizontal = TRUE))$x$get_labels()) # c b A10 A2
```

## Proposed fix

Sort as for a vertical plot and reverse the levels once at the end when horizontal.

## History

- 2026-11-08: found while coding I002 sheet 20 (case 20.02); fixed in the same session: `sort_data()` now sorts with `sort_data_vertical()` and reverses the levels for horizontal plots. Back-to-back plots keep their previous facet order (they used `horizontal = TRUE` to put the second facet on the left), except that the data order is kept as before. Visible change: factors in horizontal plots (such as age groups in a dumbbell plot) now read in level order from the top; the dumbbell snapshot was updated. Tests 20.01, 20.02.

---
id: F062
title: set_max_items(): inverted-looking summarise condition, %p counts rows, n includes the rest item
severity: low
type: bug
area: data-validation
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2809, R/validate.R:2817, R/validate.R:2858]
related: []
origin: R02 sheet 22
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F062 set_max_items() details

## Summary

- After merging levels, the function summarises only `if (all(group_sizes(df) == 1))`, i.e. when there is nothing to summarise; the real summarising happens later in `plot2_exec()`. The condition reads inverted and only recomputes datalabels.
- `%p` in `*.max_txt` is the share of rows, not of y.
- `x.max_items = n` shows n - 1 values plus the rest item; undocumented.

## Reproduction

```r
p <- plot2(data.frame(g = letters[1:5], v = c(50, 20, 10, 10, 10)), g, v, x.max_items = 3, x.max_txt = "rest (%p)")
print(levels(p$data$g))
# Currently: "rest (60%)" (3 of 5 rows), although the merged values hold 30% of y.
# Expected after fix: percentage of y (or documented as share of rows), and a clear rule for n.
```

## History

- 2026-11-07: filed in R02 (by reading).
- 2026-11-07: reproduced.
- 2026-11-08: fixed: %p is the share of the total of y (maintainer's decision: 'Share of y'), or of the rows when y is not numeric; *.max_items counts the rest item (decision: 'Total items'), now documented; %n documented as the number of merged values, which the code already did (the documentation said 'outcome of summarise_function'); the summarising block that only ran when there was nothing to summarise is removed, since plot2_exec() summarises the merged groups. Test in test-regression-phase6.R.

---
id: F069
title: Internal columns (_var_datalabels, _sankey_*) remain in the returned plot data
severity: low
type: bug
area: architecture
status: verified
verification: verified
effort: S
api_change: no
locations: [R/utils.R:631, R/plot2.R:2255]
related: [F001]
origin: R03 sheet E14
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F069 Internal columns left in the plot data

## Summary

`restore_mapping()` removes only `_var_x`, `_var_y`, `_var_category` and `_var_facet` from `p$data`. `_var_datalabels`, `_var_y_secondary` and the Sankey helper columns `_sankey_split`, `_sankey_x`, `_sankey_id` remain visible to users who inspect or reuse `p$data`, and the datalabel layers still map `_var_datalabels`.

## Reproduction

```r
p <- plot2(data.frame(x = c("a", "b"), y = 1:2), x, y)
print(colnames(p$data))
# Currently: "x" "y" "_var_datalabels"
# Expected after fix: only user-facing columns (datalabels under a readable name, e.g. "datalabels").
```

## Proposed fix

Rename remaining internal columns to readable names and update the layer mappings accordingly in `restore_mapping()`.

## History

- 2026-11-07: filed in R03, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; restore_mapping() removes internal columns that no inherited layer uses and gives the others readable names (datalabels, sankey_x, sankey_split, sankey_id, made unique against user columns), rewriting the layer mappings accordingly. Tests M03 and 03.22 updated to the new names.

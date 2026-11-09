---
id: F055
title: Datalabel background layer maps a vector captured from the environment instead of a data column
severity: medium
type: design
area: data-validation
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2447]
related: [F026]
origin: R02 sheet 17
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F055 Datalabel background captures an external vector

## Summary

The background `geom_label()` uses `aes(label = ifelse(is.na(original_values), NA, paste0(original_values, strrep("-", ...))))`, where `original_values` is a vector from the function environment. It only works while the layer data have exactly the same rows in the same order; replacing the data (`%+%`), or any stat that reorders, breaks it. The dash padding to widen the box is also font-dependent.

## Reproduction

```r
p <- plot2(data.frame(g = c("a", "b", "c"), v = 1:3), g, v)
p2 <- p
p2$data <- data.frame(g = c("a", "b"), v = 4:5)
print(tryCatch({ ggplot2::ggplot_build(p2); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: "OK"
```

## Proposed fix

Store the padded text as a data column (for example `_var_datalabels_bg`, renamed in `restore_mapping()`), or use `label.padding` / `label.size` to widen the box instead of padding text.

## History

- 2026-11-07: filed in R02 (by reading).
- 2026-11-07: reproduced via `tools/run_repro.R` ("Aesthetics must be either length 1 or the same as the data").
- 2026-11-08: fixed: the background geom_label() computes its padded label from the _var_datalabels column (renamed with the layer mappings by restore_mapping()), not from a vector of the function, so it follows the layer data. The reproduction above replaces the data without a label column, which no label layer can draw; the test uses new data with the datalabels column (failed before with 'Aesthetics must be either length 1 or the same as the data', now builds). The font-dependent dash padding is unchanged. Tests: test-regression-phase4.R (F055).

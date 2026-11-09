---
id: F044
title: Reverse y transform permutes the expansion vector incorrectly
severity: medium
type: bug
area: scales
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1268]
related: [F043]
origin: R02 sheet 06
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F044 Reverse transform permutes expansion incorrectly

## Summary

An `expansion()` vector is `c(mult_lower, add_lower, mult_upper, add_upper)`. For `y.transform = "reverse"` the code uses `c(e[3], e[c(1, 2, 4)])`, which puts the lower multiplicative expansion in the additive slot (data units) and drops the upper multiplicative expansion. The correct swap is `e[c(3, 4, 1, 2)]`.

## Reproduction

```r
p <- plot2(data.frame(g = c("a", "b"), v = c(1, 10)), g, v, y.transform = "reverse", y.expand = c(0.1, 0.5))
print(unclass(ggplot2::layer_scales(p)$y$expand))
# Currently: 0.5 0.1 0 0
# Expected after fix: 0.5 0 0.1 0
```

## History

- 2026-11-07: filed in R02, reproduced.
- 2026-11-08: fixed and verified (Phase 1). Expansion swapped as `e[c(3, 4, 1, 2)]`; regression test 06.13.

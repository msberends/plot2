---
id: F083
title: move_layer() with a positive move duplicates a layer and drops another
severity: medium
type: bug
area: methods
status: open
verification: verified
effort: S
api_change: no
locations: [R/move_layer.R:44, R/move_layer.R:45]
related: []
origin: R04 sheet M06
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F083 move_layer() with a positive move duplicates a layer and drops another

## Summary

`move_layer()` shifts the remaining layers with `for (i in (layer_new + 1):length(layers))` and `layers_backup[[i - 1]]`, which only works when moving a layer backwards. Moving forwards (positive `move`) copies the wrong layers; when `layer_new` is the last position, `(n + 1):n` counts down and appends an extra layer. For two layers [point, line], `move_layer(p, layer = 1, move = 1)` gives [point, point, line].

## Reproduction

```r
p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point() + ggplot2::geom_line()
q <- move_layer(p, layer = 1, move = 1)
print(vapply(q$layers, function(l) class(l$geom)[1], ""))
# Expected after fix: "GeomLine" "GeomPoint"
```

## Proposed fix

Reorder by index: `ord <- append(setdiff(seq_along(layers), layer_old), layer_old, after = layer_new - 1); plot$layers <- layers[ord]`. Also clamp `layer_new` to the number of layers.

## History

- 2026-11-08: filed in R04, reproduced.

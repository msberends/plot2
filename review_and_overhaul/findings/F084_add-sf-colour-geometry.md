---
id: F084
title: add_sf() ignores datalabels.colour and requires a geometry column named geometry
severity: medium
type: bug
area: methods
status: open
verification: verified
effort: S
api_change: no
locations: [R/add_type.R:649, R/add_type.R:694]
related: [F003]
origin: R04 sheet M05
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F084 add_sf() ignores datalabels.colour and requires a geometry column named geometry

## Summary

Three issues in `add_sf()`: (1) `datalabels.colour` is accepted but never used; the labels get the border `colour` (default grey50) (`R/add_type.R:694`). (2) It requires a column literally named `geometry` in the plot data (`R/add_type.R:649`), while sf allows any name (`plot2()` itself handles other names via `_sf.column`), so `add_sf()` fails with "must be a ggplot2 object based on geographic data" on such plots. (3) The default `datalabels.nudge_y = 2500` assumes the Dutch RD New projection (EPSG:28992); for other CRSs a message is shown, but the default is unsuitable elsewhere. Notably, the bundled `netherlands` data set is in EPSG:4326 (degrees), so with the package's own example data the default shifts labels by 2,500 degrees.

## Reproduction

```r
m <- plot2(netherlands)
q <- add_sf(m, netherlands[1:2, ], datalabels = province, datalabels.colour = "red")
print(unique(as.character(ggplot2::layer_data(q, length(q$layers))$colour)))
nl2 <- sf::st_set_geometry(netherlands, "geom")
print(tryCatch({ add_sf(plot2(nl2), netherlands[1:2, ]); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: "#FF0000"; "OK".
```

## Proposed fix

Use `datalabels.colour` for the text layer; detect the geometry column with `attr(plot$data, "sf_column")` or `inherits(col, "sfc")`; default `datalabels.nudge_y` to 0 (or a fraction of the bounding box).

## History

- 2026-11-08: filed in R04, reproduced.

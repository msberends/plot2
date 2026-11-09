# M07 coord_spider()

`R/coord_spider.R`

## Role

A ggproto coordinate system for spider (radar) charts with straight segments: theta from a discrete axis, radius anchored at 0 for non-negative data, a web of spokes and rings drawn as background, theta and radial labels drawn by the coordinate itself; axis titles removed via `ggplot_add()`.

## Findings

F086 (new: radial labels ignore `start`), F037 (ggplot2 internals: ggproto `Coord` subclass). Idea I003 (facets work in plain ggplot2, so plot2's restriction may be lifted).

## End-to-end test cases

- [x] M07.01 Works on a plain ggplot2 polygon plot; axis titles removed.
- [x] M07.02 All points lie within the unit circle defined by `outer.radius`.
- [x] M07.03 The first category lies on the vertical spoke for `start = 0`.
- [x] M07.04 Radius anchored at 0: the value 0 maps to the centre for non-negative data.
- [x] M07.05 `outer.radius` outside (0, 1] errors.
- [x] M07.06 `theta = "y"` works.
- [x] M07.07 Facets work in plain ggplot2.
- [x] M07.08 (skipped: F086) Radial labels follow `start`.

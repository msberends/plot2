# E15 Sankey plots

Spread over `R/plot2.R:866-881`, `R/plot2.R:1394-1405`, `R/plot2.R:1693-1717`, `R/plot2.R:2255-2257`, and `R/validate.R:593-620`

## Role

Reshapes the data to one row per observation and x variable (`_sankey_x`, `_sankey_split`, `_sankey_id`) and draws ggforce parallel sets: flows coloured by category, node rectangles, node labels.

## Findings

F071 (requirement check too late), F069 (helper columns remain).

## End-to-end test cases

All skip without ggforce.

- [x] E15.01 Layers: shape (flows), parallel-sets axes (nodes), text (labels).
- [x] E15.02 The x-axis shows the variable names in the order given.
- [x] E15.03 Missing category or y errors with the friendly message.
- [x] E15.04 Axes removed by default, with a message.
- [x] E15.05 Facets supported.
- [x] E15.06 `horizontal = TRUE` flips the coordinates.
- [x] E15.07 `sankey.alpha` reaches the flow layer.
- [x] E15.08 (skipped: F071) A single x variable gives the friendly message.
- [x] E15.09 (skipped: F069) No helper columns in `p$data`.

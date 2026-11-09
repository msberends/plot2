# M06 move_layer() and add_mapping()

`R/move_layer.R`, `R/add_mapping.R`

## Role

`move_layer()` moves a layer in the drawing order; `add_mapping()` merges aesthetics into the plot mapping.

## Findings

F083 (new: moving forwards duplicates a layer).

## End-to-end test cases

- [x] M06.01 Default: the last layer moves one position back.
- [x] M06.02 A large negative move is clamped to the first position.
- [x] M06.03 A non-existing layer errors.
- [x] M06.04 (skipped: F083) A positive move: layer 1 to position 2, layer count unchanged.
- [x] M06.05 `add_mapping(colour = y)` adds the colour aesthetic and keeps the others.
- [x] M06.06 Both reject non-ggplot input.

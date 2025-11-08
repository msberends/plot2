# Move a `ggplot` Layer

Use this function to move a certain plot layer up or down. This function
returns a `ggplot` object.

## Usage

``` r
move_layer(plot, move = -1, layer = length(plot$layers))
```

## Arguments

- plot:

  A `ggplot` object.

- move:

  Number of layers to move `layer` up or down.

- layer:

  The layer to affect, defaults to top layer.

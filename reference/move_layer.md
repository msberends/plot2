# Move a `ggplot` Layer

Use this function to move a certain plot layer up or down.

## Usage

``` r
move_layer(plot, move = -1, layer = length(plot$layers))
```

## Arguments

- plot:

  A `ggplot` object.

- move:

  Number of layers to move `layer`: a negative value moves it down
  (drawn earlier, so below other layers), a positive value moves it up.
  The new position is limited to the existing layers. Defaults to `-1`,
  one layer down.

- layer:

  The number of the layer to move, defaults to the top layer.

## Value

A `ggplot` object with the layers in the new order.

## Examples

``` r
p <- plot2(iris, Sepal.Length, Sepal.Width, colour = "grey75") |>
  add_line(y = mean(Sepal.Width), colour = "red", linewidth = 2)
#> Using type = "point" since both axes are numeric

# the line is drawn on top of the points
p


# move the line one layer down, below the points
move_layer(p)
```

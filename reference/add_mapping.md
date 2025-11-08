# Add Additional Mapping

This function can be used to adjust the mapping of a plot.

## Usage

``` r
add_mapping(plot, ...)
```

## Arguments

- plot:

  A `ggplot2` plot.

- ...:

  Arguments passed on to
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

## Examples

``` r
p <- iris |> plot2(Sepal.Length, Sepal.Width, Species, zoom = TRUE)
#> ℹ Using type = "point" since both axes are numeric
p


p |> add_mapping(shape = Species)
```

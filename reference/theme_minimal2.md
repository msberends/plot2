# An Even More Minimal Theme

This `ggplot2` theme provides even more white area and less clutter than
[`theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Usage

``` r
theme_minimal2(
  ...,
  colour_font_primary = getOption("plot2.colour_font_primary", "black"),
  colour_font_secondary = getOption("plot2.colour_font_secondary", "grey35"),
  colour_font_axis = getOption("plot2.colour_font_axis", "grey25"),
  colour_background = getOption("plot2.colour_background", "white")
)
```

## Arguments

- ...:

  Arguments passed on to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- colour_font_primary:

  Colour to set for the plot title and tag.

- colour_font_secondary:

  Colour to set for the plot subtitle and caption.

- colour_font_axis:

  Colour to set for the axis titles on both x and y.

- colour_background:

  Colour to set for the background.

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(hp, mpg)) +
  geom_point()

  
ggplot(mtcars, aes(hp, mpg)) +
  geom_point() +
  theme_minimal2()

  
# in plot2(), the 'theme' argument defaults to theme_minimal2():
mtcars |>
  plot2(hp, mpg)
#> ℹ Using type = "point" since both axes are numeric

  
# set to NULL to use the ggplot2 default:
mtcars |>
  plot2(hp, mpg, theme = NULL)
#> ℹ Using type = "point" since both axes are numeric
```

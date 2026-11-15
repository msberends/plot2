# Get Plot Title

Get the title of the plot, or a default value. If the title is not set
in a plot, this function tries to generate one from the plot mapping.

## Usage

``` r
get_plot_title(plot, valid_filename = TRUE, default = NULL)
```

## Arguments

- plot:

  A `ggplot2` plot.

- valid_filename:

  A [logical](https://rdrr.io/r/base/logical.html) to indicate whether
  the returned value should be a valid filename, defaults to `TRUE`.

- default:

  The default value, if a plot title is absent.

## Examples

``` r
without_title <- plot2(mtcars)
#> Using x = mpg
#> Using y = cyl
#> Using type = "point" since both axes are numeric
with_title <- plot2(mtcars, title = "Plotting **mpg** vs. **cyl**!")
#> Using x = mpg
#> Using y = cyl
#> Using type = "point" since both axes are numeric

# default is a guess:
get_plot_title(without_title)
#> [1] "cyl_per_mpg"
get_plot_title(without_title, valid_filename = FALSE)
#> [1] "Cyl per mpg"
get_plot_title(with_title)
#> [1] "plotting_mpg_vs_cyl"
get_plot_title(with_title, valid_filename = FALSE)
#> [1] "Plotting mpg vs. cyl!"

# unless 'default' is set (only affects plots without title):
get_plot_title(without_title, default = "title")
#> [1] "title"
get_plot_title(with_title, default = "title")
#> [1] "plotting_mpg_vs_cyl"
```

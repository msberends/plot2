# Introduction to plot2

[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
creates a complete `ggplot2` plot from a single function call. You
describe what you want to see, and
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
determines how to draw it: it chooses the plot type, summarises the
data, sorts, sets colours, axes and data labels, and applies a clean
theme. The result is an ordinary `ggplot` object, so everything that
`ggplot2` offers remains available afterwards.

This vignette introduces the main ideas. It uses well-known data sets:
`mpg` from `ggplot2`, with the fuel economy of 234 cars from 1999 and
2008, `gapminder` from the `gapminder` package, with the life
expectancy, population and income of 142 countries from 1952 to 2007,
and some data sets from base R.

``` r

library(plot2)
data(mpg, package = "ggplot2")

mpg
#> # A tibble: 234 × 11
#>    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
#>    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
#>  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
#>  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
#>  3 audi         a4           2    2008     4 manu… f        20    31 p     comp…
#>  4 audi         a4           2    2008     4 auto… f        21    30 p     comp…
#>  5 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
#>  6 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
#>  7 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
#>  8 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
#>  9 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
#> 10 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
#> # ℹ 224 more rows
```

``` r

library(gapminder)

gapminder
#> # A tibble: 1,704 × 6
#>    country     continent  year lifeExp      pop gdpPercap
#>    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
#>  1 Afghanistan Asia       1952    28.8  8425333      779.
#>  2 Afghanistan Asia       1957    30.3  9240934      821.
#>  3 Afghanistan Asia       1962    32.0 10267083      853.
#>  4 Afghanistan Asia       1967    34.0 11537966      836.
#>  5 Afghanistan Asia       1972    36.1 13079460      740.
#>  6 Afghanistan Asia       1977    38.4 14880372      786.
#>  7 Afghanistan Asia       1982    39.9 12881816      978.
#>  8 Afghanistan Asia       1987    40.8 13867957      852.
#>  9 Afghanistan Asia       1992    41.7 16317921      649.
#> 10 Afghanistan Asia       1997    41.8 22227415      635.
#> # ℹ 1,694 more rows
```

## Four directions

Every plot is described by up to four plotting directions, which are
also the first four arguments of
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md):

- `x`: the values along the x axis;
- `y`: the values along the y axis;
- `category`: the groups within the plot, shown as colours (in `ggplot2`
  this is the `colour` or `fill` aesthetic, depending on the geom);
- `facet`: the groups shown in separate panels.

Columns are given without quotes, as in `dplyr`:

``` r

mpg |>
  plot2(x = displ, y = hwy, category = drv, facet = year)
#> Using type = "point" since both axes are numeric
#> Assuming facet.fixed_x = TRUE since the two x scales differ by less than 25%
#> Assuming facet.fixed_y = TRUE since the two y scales differ by less than 25%
#> Assuming facet.repeat_labels_y = FALSE since y has fixed scales
```

![](plot2_files/figure-html/unnamed-chunk-4-1.png)

Since these are the first four arguments, the names can be left out. The
following call gives the same plot:

``` r

mpg |>
  plot2(displ, hwy, drv, year)
```

[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) fills
in what is not given. The `iris` data have four numeric columns and a
factor, so a call without any directions plots the first two numeric
columns as points, coloured by the factor:

``` r

iris |>
  plot2()
#> Using x = Sepal.Length
#> Using y = Sepal.Width
#> Using category = Species
#> Using type = "point" since both axes are numeric
```

![](plot2_files/figure-html/unnamed-chunk-6-1.png)

Each automatic decision follows a fixed rule. In an interactive session,
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
reports these decisions in a message, such as
`Using category = Species`; set `options(plot2.silent = TRUE)` to hide
them. Any decision is overridden by setting the argument concerned. The
section *Automatic choices* in
[`?plot2`](https://msberends.github.io/plot2/reference/plot2.md) lists
all rules.

## Calculations in arguments

The directions accept calculations on the columns of the data. For `y`,
this means that the data are summarised per group, without a separate
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) and
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
step:

``` r

mpg |>
  plot2(x = class, y = n_distinct(model))
```

![](plot2_files/figure-html/unnamed-chunk-7-1.png)

Every value of `x` is now one bar, so
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) chose
a column plot and added data labels. Use `y = n()` to count rows, or any
other function that returns a single value per group, such as
[`mean()`](https://rdrr.io/r/base/mean.html),
[`median()`](https://rdrr.io/r/stats/median.html) or
[`max()`](https://rdrr.io/r/base/Extremes.html).

The other directions accept calculations too:

``` r

gapminder |>
  plot2(x = year,
        y = median(lifeExp),
        category = ifelse(continent == "Africa", "Africa", "Rest of the world"),
        type = "line")
```

![](plot2_files/figure-html/unnamed-chunk-8-1.png)

[`n()`](https://dplyr.tidyverse.org/reference/context.html) and
[`n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
are re-exported from `dplyr`, together with the `tidyselect` helpers
such as [`where()`](https://tidyselect.r-lib.org/reference/where.html)
and
[`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
so they work without loading other packages. A helper can select several
columns for `y`, which are then shown as a category:

``` r

iris |>
  plot2(x = Species, y = where(is.double))
#> Using y = c(Petal.Length, Petal.Width, Sepal.Length, Sepal.Width)
#> Using type = "boxplot" since all groups in Species and category contain at least three values
```

![](plot2_files/figure-html/unnamed-chunk-9-1.png)

Titles accept calculations as well, which keeps them correct when the
data change.
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) also
works here:

``` r

mpg |>
  plot2(x = hwy,
        type = "histogram",
        title = paste("Highway mileage of", n_distinct(model), "car models"),
        subtitle = paste("Based on", n(), "cars"),
        caption = paste("Model years", min(year), "and", max(year)))
#> Using binwidth = 2.4 based on data
#> Assuming smooth = TRUE for type = "histogram"
```

![](plot2_files/figure-html/unnamed-chunk-10-1.png)

## Plot types

Without a `type`,
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
chooses one from the data: a boxplot if every group contains at least
three values, points if both axes are numeric, and columns otherwise.
The `type` argument sets it explicitly. It accepts the name of any
`ggplot2` geom, with or without the `geom_` prefix, and abbreviations
such as `"l"` for lines and `"b"` for boxplots:

``` r

mpg |>
  plot2(x = class, y = hwy, type = "violin")
```

![](plot2_files/figure-html/unnamed-chunk-11-1.png)

[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) also
adds types that `ggplot2` does not provide directly, such as dumbbell,
back-to-back, Sankey, spider and UpSet plots. A dumbbell plot compares
two groups:

``` r

mpg |>
  plot2(x = class, y = median(hwy), category = year, type = "dumbbell")
#> Assuming category.character = TRUE since category seems to be years (use category.character = FALSE to prevent this)
```

![](plot2_files/figure-html/unnamed-chunk-12-1.png)

[`vignette("supported_types", package = "plot2")`](https://msberends.github.io/plot2/articles/supported_types.md)
shows every type with its data requirements.

## Sorting and limiting

Each direction can be sorted with one argument: `x.sort`,
`category.sort` and `facet.sort`. They accept `TRUE` (alphabetical, with
numbers in their numeric order), `"freq-desc"` and `"freq-asc"` (by the
values of `y`), `FALSE` (in the order of the data), or a vector with the
desired order:

``` r

mtcars |>
  plot2(x = carb, y = n(), x.sort = "freq-desc")
#> Using x.character = TRUE since x.sort is set
```

![](plot2_files/figure-html/unnamed-chunk-13-1.png)

A vector does not need to contain all values; the remaining values
follow in their usual order:

``` r

mpg |>
  plot2(x = class,
        y = n(),
        category = drv,
        x.sort = c("suv", "pickup"),
        category.sort = "alpha-desc")
```

![](plot2_files/figure-html/unnamed-chunk-14-1.png)

`x.max_items`, `category.max_items` and `facet.max_items` limit the
number of values shown. The values beyond the limit are combined into
one item, which is placed last:

``` r

mtcars |>
  plot2(x = carb, y = n(), x.sort = "freq-desc", x.max_items = 4)
#> Using x.character = TRUE since x.sort is set
```

![](plot2_files/figure-html/unnamed-chunk-15-1.png)

## Stacking

Columns of different categories are placed next to each other.
`stacked = TRUE` stacks them, and `stacked_fill = TRUE` stacks them to
100%:

``` r

mpg |>
  plot2(x = class, y = n(), category = drv, stacked = TRUE)
```

![](plot2_files/figure-html/unnamed-chunk-16-1.png)

``` r

mpg |>
  plot2(x = class, y = n(), category = drv, stacked_fill = TRUE)
```

![](plot2_files/figure-html/unnamed-chunk-17-1.png)

## Axes and data labels

All settings of a direction share its name as a prefix, such as
`x.title`, `y.limits` or `category.title`. The y axis starts at zero
where possible; `y.zoom = TRUE` (or `zoom = TRUE` for both axes) limits
it to the range of the data. `horizontal = TRUE` turns the plot, and a
`type = "bar"` gives horizontal columns:

``` r

mpg |>
  plot2(x = class,
        y = n(),
        category = year,
        type = "bar",
        x.title = "Class",
        y.title = "Number of cars",
        category.title = "Model year")
#> Assuming category.character = TRUE since category seems to be years (use category.character = FALSE to prevent this)
```

![](plot2_files/figure-html/unnamed-chunk-18-1.png)

Axis values can be formatted with functions, such as `y.percent = TRUE`
for percentages or `y.labels` for any labelling function. A secondary y
axis is set with `y_secondary`. This example uses the `economics` data
set from `ggplot2`:

``` r

ggplot2::economics |>
  plot2(x = date,
        y = unemploy / 1000,
        y_secondary = psavert,
        type = "line",
        colour = "#4E79A7",
        y_secondary.colour = "#F28E2B",
        x.title = NULL,
        y.title = "Unemployed (millions)",
        y_secondary.title = "Personal savings rate (%)")
#> Using x.date_breaks = "5 years" based on data
#> Using x.date_labels = "yyyy" based on data
```

![](plot2_files/figure-html/unnamed-chunk-19-1.png)

Data labels are added automatically to column plots with at most 25
values. `datalabels = TRUE` always shows them, `datalabels = FALSE`
hides them, and `datalabels.format` sets their text. In this format,
`%n` is the value and `%p` its percentage:

``` r

mpg |>
  plot2(x = class, y = n(), datalabels.format = "%n (%p)")
```

![](plot2_files/figure-html/unnamed-chunk-20-1.png)

## Colours

`colour` sets the colours for every plot type. In `ggplot2`, this
requires either the `colour` or the `fill` aesthetic with its matching
scale function, depending on the geom. `colour` accepts colour names,
hexadecimal codes, palette names such as `"viridis"`, and named vectors
that match the values of `category`:

``` r

mpg |>
  plot2(x = class,
        y = n(),
        category = year,
        colour = c("1999" = "#4E79A7", "2008" = "#F28E2B"))
#> Assuming category.character = TRUE since category seems to be years (use category.character = FALSE to prevent this)
```

![](plot2_files/figure-html/unnamed-chunk-21-1.png)

`category.focus` highlights one value and shows the others in grey:

``` r

gapminder |>
  plot2(x = year,
        y = median(lifeExp),
        category = continent,
        category.focus = "Africa",
        type = "line")
```

![](plot2_files/figure-html/unnamed-chunk-22-1.png)

The article *Colours and themes* on the package website describes
palettes, custom organisation colours and themes in more detail.

## Adding layers

The `add_*()` functions add a layer to an existing plot. They use the
data of the plot unless other data are given, and they take the
`category` of the plot into account:

``` r

p <- mtcars |>
  plot2(x = mpg, y = hp, category = factor(cyl), category.title = "Cylinders")
#> Using type = "point" since both axes are numeric

p |>
  add_line(y = mean(hp), legend.value = "Mean per group")
```

![](plot2_files/figure-html/unnamed-chunk-23-1.png)

[`add_line()`](https://msberends.github.io/plot2/reference/add_type.md),
[`add_point()`](https://msberends.github.io/plot2/reference/add_type.md),
[`add_col()`](https://msberends.github.io/plot2/reference/add_type.md),
[`add_errorbar()`](https://msberends.github.io/plot2/reference/add_type.md)
and
[`add_smooth()`](https://msberends.github.io/plot2/reference/add_type.md)
cover the most common layers, and
[`add_type()`](https://msberends.github.io/plot2/reference/add_type.md)
adds any other geom. A smooth can also be set directly with the `smooth`
argument of
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md):

``` r

mtcars |>
  plot2(x = mpg, y = hp, smooth = TRUE, smooth.method = "lm")
#> Using type = "point" since both axes are numeric
```

![](plot2_files/figure-html/unnamed-chunk-24-1.png)

## Other kinds of input

[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) is an
S3 generic. Besides data frames, it plots vectors, tables, formulas,
matrices, and models and other objects that
[`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
or
[`broom::augment()`](https://generics.r-lib.org/reference/augment.html)
convert to a data frame. A formula has the form
`y ~ x + category | facet`:

``` r

plot2(hp ~ mpg + factor(cyl), data = mtcars)
#> Using type = "point" since both axes are numeric
```

![](plot2_files/figure-html/unnamed-chunk-25-1.png)

A correlation matrix becomes a heatmap:

``` r

mtcars |>
  cor() |>
  plot2(colour = c("#2166AC", "white", "#B2182B"),
        datalabels = TRUE,
        category.title = "Correlation")
#> Assuming type = "tile" since the matrix contains identical row and column names
#> Using category.midpoint = 0 (the current category scale centre)
```

![](plot2_files/figure-html/unnamed-chunk-26-1.png)

A model is converted with
[`broom::augment()`](https://generics.r-lib.org/reference/augment.html),
which adds columns such as the fitted values (`.fitted`) and the
residuals (`.resid`) to the data of the model. This gives a residual
plot:

``` r

lm(hp ~ mpg, data = mtcars) |>
  plot2(x = .fitted, y = .resid, x.title = "Fitted values", y.title = "Residuals")
#> Using type = "point" since both axes are numeric
```

![](plot2_files/figure-html/unnamed-chunk-27-1.png)

Geographic data of class `sf` become maps; the article *Maps* on the
package website describes them. `?plot2-methods` lists all methods.

## Continuing with ggplot2

The result of
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) is a
`ggplot` object. Any `ggplot2` layer, scale, coordinate system or theme
element can be added with `+`:

``` r

mpg |>
  plot2(x = class, y = mean(hwy), y.title = "Mean highway mileage", datalabels = FALSE) +
  ggplot2::geom_hline(yintercept = mean(mpg$hwy), linetype = "dashed")
```

![](plot2_files/figure-html/unnamed-chunk-28-1.png)

By default,
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) uses
[`theme_minimal2()`](https://msberends.github.io/plot2/reference/theme_minimal2.md),
a variant of
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with fewer lines and more white space. The `theme` argument sets another
theme, and `theme = NULL` uses the default `ggplot2` theme.

## Settings for all plots

Package options set defaults for all plots in a session, such as the
colours (`plot2.colour`), the font (`plot2.font`), the theme
(`plot2.theme`) and the decimal mark (`plot2.decimal.mark`). Set them
with [`options()`](https://rdrr.io/r/base/options.html), for example in
an `.Rprofile` file:

``` r

options(plot2.colour = "viridis",
        plot2.decimal.mark = ",")
```

`?plot2-options` lists all options.

## Learning more

- [`vignette("supported_types", package = "plot2")`](https://msberends.github.io/plot2/articles/supported_types.md)
  shows all plot types.
- [`?plot2`](https://msberends.github.io/plot2/reference/plot2.md)
  documents every argument. Its section *Automatic choices* describes
  how missing arguments are determined.
- The package website, <https://msberends.github.io/plot2/>, has
  articles on colours and themes, fonts and maps, and on
  [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) for
  users of `ggplot2`.
- [`create_interactively()`](https://msberends.github.io/plot2/reference/create_interactively.md)
  opens a Shiny app that builds a
  [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md) call
  from a point-and-click interface and shows the resulting code.

# Interactively Create a `plot2`

This Shiny app allows for interactive creation of a `plot2`.

## Usage

``` r
create_interactively(data = NULL)
```

## Arguments

- data:

  A data set to load. Not strictly required, since all data sets in the
  global environment will be shown.

## Details

![](figures/create_interactively.jpg)

## Examples

``` r
if (FALSE) { # \dontrun{

create_interactively()

iris |> create_interactively()
} # }
```

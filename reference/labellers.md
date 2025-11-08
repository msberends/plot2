# Label Euro currencies

Format numbers as currency, rounding values to dollars or cents using a
convenient heuristic.

## Usage

``` r
euros(x, big.mark = big_mark(), decimal.mark = dec_mark(), ...)

dollars(x, big.mark = big_mark(), decimal.mark = dec_mark(), ...)
```

## Arguments

- x:

  Values.

- big.mark:

  Thousands separator, defaults to
  [`big_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md).

- decimal.mark:

  Decimal mark, defaults to
  [`dec_mark()`](https://msberends.github.io/plot2/reference/dec_mark.md).

- ...:

  Any argument to give to the geom. This will override automatically-set
  settings for the geom.

## Examples

``` r
if (FALSE) { # \dontrun{
profit <- data.frame(group = LETTERS[1:4],
                     profit = runif(4, 10000, 25000))

profit |>
  plot2(y.labels = euros,
        datalabels = FALSE)
        
profit |>
  plot2(y.labels = euros,
        datalabels.format = euros)
} # }
```

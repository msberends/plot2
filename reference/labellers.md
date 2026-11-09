# Label Currencies

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

## Details

The euro sign of `euros()` cannot be encoded by the default
[pdf()](https://rdrr.io/r/grDevices/pdf.html) device. To save such plots
as PDF, use the Cairo device, e.g.
`ggplot2::ggsave("plot.pdf", device = grDevices::cairo_pdf)`, or in R
Markdown the chunk option `dev = "cairo_pdf"`.

## Examples

``` r
profit <- data.frame(group = LETTERS[1:4],
                     profit = runif(4, 10000, 25000))

profit |>
  plot2(y.labels = euros,
        datalabels = FALSE)
#> Using y = profit
#> Using x = group

        
profit |>
  plot2(y.labels = euros,
        datalabels.format = euros)
#> Using y = profit
#> Using x = group


dollars(c(1500, 2499.5))
#> [1] "$1 500.00" "$2 499.50"
```

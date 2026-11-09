# Use Decimal Comma?

These functions determine which characters the decimal mark and big mark
should be that are used in plotting. They base the determination on
`getOption("OutDec")`, which is also what
[`base::format()`](https://rdrr.io/r/base/format.html) uses.

## Usage

``` r
dec_mark()

big_mark()
```

## Details

If the [option](https://rdrr.io/r/base/options.html)
`"plot2.decimal.mark"` is set, that value will be used for `dec_mark()`
if it is either a comma or a full stop.

At default, `big_mark()` returns a full stop if `dec_mark()` returns a
comma, and a space otherwise. If the
[option](https://rdrr.io/r/base/options.html) `"plot2.big.mark"` is set,
that value will be used if it is either a comma (`","`) or a full stop
(`"."`) or a space (`" "`) or an empty character (`""`).

## Examples

``` r
# at default, this follows `getOption("OutDec")`:
dec_mark()
#> [1] "."
# and big_mark() returns a space if dec_mark() returns ".":
big_mark()
#> [1] " "

# you you can set options to alter behaviour:
options(plot2.decimal.mark = ",", plot2.big.mark = ".")
dec_mark()
#> [1] ","
big_mark()
#> [1] "."

options(plot2.decimal.mark = ",", plot2.big.mark = NULL)
dec_mark()
#> [1] ","
big_mark()
#> [1] "."

options(plot2.big.mark = ",")
dec_mark()
#> [1] ","
big_mark()
#> [1] ","

# clean up
options(plot2.decimal.mark = NULL, plot2.big.mark = NULL)
```

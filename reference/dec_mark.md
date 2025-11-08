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

If the [option](https://rdrr.io/r/base/options.html) `"dec_mark"` is
set, that value will be used for `dec_mark()` if it is either a comma or
a full stop.

At default, `big_mark()` returns a full stop if `dec_mark()` returns a
comma, and a space otherwise. If the
[option](https://rdrr.io/r/base/options.html) `"big_mark"` is set, that
value will be used if it is either a comma (`","`) or a full stop
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
options(dec_mark = ",", big_mark = ".")
dec_mark()
#> [1] ","
big_mark()
#> [1] "."

options(dec_mark = ",", big_mark = NULL)
dec_mark()
#> [1] ","
big_mark()
#> [1] "."

options(big_mark = ",")
dec_mark()
#> [1] ","
big_mark()
#> [1] ","

# clean up
options(dec_mark = NULL, big_mark = NULL)
```

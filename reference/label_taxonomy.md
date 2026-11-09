# Label Taxonomic Names in Italics

Format the names of microorganisms in axis or legend labels in italics,
as is the convention in microbiology. Family and genus names in the
[microorganisms](https://amr-for-r.org/reference/microorganisms.html)
data set of the `AMR` package are italicised, and so are species and
subspecies names that follow a genus or an abbreviated genus (such as
*S. aureus*); other words are kept as they are.

## Usage

``` r
label_taxonomy(x, ...)
```

## Arguments

- x:

  Values, such as the breaks of an axis.

- ...:

  Not used, allows this function to be used as a labeller.

## Value

An [expression](https://rdrr.io/r/base/expression.html) vector if any
taxonomic name was found, otherwise `x` as character.

## Details

Use this function for the labels of any plotting direction, e.g.
`x.labels = label_taxonomy` or `category.labels = label_taxonomy`. It
requires the `AMR` package.

## Examples

``` r
if (requireNamespace("AMR", quietly = TRUE)) {
  label_taxonomy(c("Escherichia coli", "Other", "S. aureus"))
  
  bacteria <- data.frame(name = c("Escherichia coli", "Staphylococcus aureus", "Other"),
                         n = c(25, 18, 6))
  bacteria |>
    plot2(name, n, x.labels = label_taxonomy)
}
```

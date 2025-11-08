# Example Geography Data Set: the Netherlands

A data set containing the geometies of the twelve provinces of the
Netherlands, according to Statistics Netherlands (2021).

## Usage

``` r
netherlands
```

## Format

A [data.frame](https://rdrr.io/r/base/data.frame.html) with 12
observations and 3 variables:

- `province`  
  name of the Dutch province

- `area_km2`  
  area in square kilometres

- `geometry`  
  geometry of the province, of class sfc_MULTIPOLYGON/sfc

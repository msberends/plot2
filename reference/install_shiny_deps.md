# Install All Shiny App Dependencies

A convenience function for administrators to check and install all
packages required by
[`create_interactively()`](https://msberends.github.io/plot2/reference/create_interactively.md),
including those needed for the upload tab (`upload_tab = TRUE`). This
function is not intended for end users; run it once in an admin session
to prepare the environment:

## Usage

``` r
install_shiny_deps()
```

## Details

    plot2:::install_shiny_deps()

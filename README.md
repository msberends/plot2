# `plot2`: Simplified and Enhanced Data Visualisation in R

`plot2` is a simple yet powerful extension of `ggplot2`, designed to streamline the process of creating high-quality data visualisations in R by taking away most of the work. Built with the philosophy of **Less Typing, More Plotting**, `plot2` automates many of the routine tasks that typically require manual intervention in `ggplot2`. For plotting on-the-fly, it even renders pre-processing steps in, e.g., `dplyr`, `tidyr` and `forcats` largely superfluous. This package allows you to focus on the insights and stories your data can tell, rather than on the intricate details of plot construction.

> For a comprehensive guide to using `plot2`, including advanced features and customisation options, please [see the full vignette here](https://msberends.github.io/plot2/articles/plot2.html).

## Philosophy

`ggplot2` is a versatile tool that has become a cornerstone of data visualisation in R, giving users unparalleled control over their plots. However, with this flexibility often comes the need for repetitive and verbose code, especially for routine tasks such as setting axis labels, choosing plot types, or transforming data.

`plot2` is designed to complement `ggplot2` by offering a more streamlined and intuitive interface. It simplifies the process of creating plots by automatically handling many of the details, without sacrificing the flexibility and power that `ggplot2` provides. Whether you're quickly exploring data or preparing a polished visualisation for publication, `plot2` helps you get there faster with less code.

## Key Features

- **Plotting With As Few Lines As Possible:** no need to type `ggplot()`, `geom_col()`, `aes()`, `scale_colour_manual()`, `facet_wrap()` or `theme()` anymore, just one `plot2()` call will suffice.
- **Automatic Plot Selection:** `plot2` automatically chooses the best plot type based on your data, saving you time and effort.
- **Inline Data Transformations:** Eliminate the need for manual pre-processing steps e.g. in `dplyr` and `tidyr`, by performing data transformations directly within the plotting function, even for axis and plot titles.
- **Enhanced Sorting and Faceting:** Easily sort and facet your data with simple arguments, streamlining the creation of complex multi-panel plots.
- **New Clean Theme:** Includes `theme_minimal2()`, a new minimalist theme based on `theme_minimal()` that is further optimised for clear and professional outputs, therefore perfect for PDF publications, scientific manuscripts, and presentations.
- **Seamless Integration with ggplot2:** Retain all the power and flexibility of `ggplot2` while benefiting from `plot2`’s streamlined interface.

## Installation

You can install the latest version of `plot2` here:

```r
# Directly from R Universe
install.packages("plot2",
                 repos = c(options()$repos, "https://msberends.r-universe.dev"))

# from GitHub using the 'remotes' package
remotes::install_github("msberends/plot2")
```

## Basic Usage

Here’s how easy it is to get started with `plot2`:

```r
library(plot2)

# Like plot(), just pass x, y in the way you prefer
plot2(mtcars, mpg, hp)
mtcars %>% plot2(mpg, hp)
plot2(hp ~ mpg, data = mtcars)
mtcars |> plot2(mpg, hp) # preferred method since R 4.1

# Use inline transformations
mtcars |> 
  plot2(x = log(mpg), y = sqrt(hp))

# Select using tidyselect language
iris |>
  plot2(x = Species, y = where(is.double))

# Count with n() or n_distinct(), sort by frequency
mtcars |> 
  plot2(carb, y = n(), x.sort = "freq-desc")
  
# Easily add plot types using the pipe
iris |>
  plot2(x = Sepal.Length,
        y = Sepal.Width,
        category = Species) |>
  add_line(y = mean(Sepal.Width)) # adds 3 lines, for each of `Species`

# Lose the need for seperate grouping and summarising
admitted_patients |> 
  plot2(x = hospital,
        y = n_distinct(patient_id),
        category = ifelse(date < "2010-01-01", "Prior to 2010", "Since 2010"),
        facet = ward)
```

## Getting Involved

We welcome contributions to `plot2`, whether it's through reporting issues, suggesting features, or submitting pull requests. If you're familiar with `ggplot2` and the tidyverse, your insights will be especially valuable as we continue to develop and refine the package.

## Previous Iteration

Though only released here in August 2024, this package has had years of development with hundreds of Git commits in an earlier iteration [here](https://github.com/certe-medical-epidemiology/certeplot2).

## License

This project is licensed under the GNU GPL v2.0 License - see the [LICENSE](LICENSE.md) file for details.


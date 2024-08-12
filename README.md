# `plot2`: Simplified and Enhanced Data Visualisation in R

`plot2` is a powerful extension of `ggplot2`, designed to streamline the process of creating high-quality data visualisations in R. Built with the philosophy of "less typing, more plotting," `plot2` automates many of the routine tasks that typically require manual intervention in `ggplot2`. This allows you to focus on the insights and stories your data can tell, rather than on the intricate details of plot construction.

## Philosophy

`ggplot2` is a versatile tool that has become a cornerstone of data visualisation in R, giving users unparalleled control over their plots. However, with this flexibility often comes the need for repetitive and verbose code, especially for routine tasks such as setting axis labels, choosing plot types, or transforming data.

`plot2` is designed to complement `ggplot2` by offering a more streamlined and intuitive interface. It simplifies the process of creating complex plots by automatically handling many of the details, without sacrificing the flexibility and power that `ggplot2` provides. Whether you're quickly exploring data or preparing a polished visualisation for publication, `plot2` helps you get there faster with less code.

## Key Features

- **Automatic Plot Selection:** `plot2` automatically chooses the best plot type based on your data, saving you time and effort.
- **Inline Data Transformations:** Perform data transformations directly within the plotting function, eliminating the need for pre-processing steps.
- **Enhanced Sorting and Faceting:** Easily sort and facet your data with simple arguments, streamlining the creation of complex multi-panel plots.
- **Customisable Themes:** Includes `theme_minimal2()`, a minimalist theme optimised for clear and professional outputs, perfect for publications and presentations.
- **Seamless Integration with ggplot2:** Retain all the power and flexibility of `ggplot2` while benefiting from `plot2`’s streamlined interface.

## Installation

You can install the latest version of `plot2` from GitHub:

```r
# Install the development version from GitHub
devtools::install_github("msberends/plot2")
```

## Basic Usage

Here’s how easy it is to get started with `plot2`:

```r
library(plot2)

# Create a simple plot
data.frame(x = LETTERS[1:10], y = 11:20) |> 
  plot2()

# Add a custom plot type
data.frame(x = 1:10, y = rnorm(10)) |> 
  plot2(type = "point")

# Use inline transformations
mtcars |> 
  plot2(x = log(mpg), y = sqrt(hp))

# Select using tidyselect language
iris |>
  plot2(x = Species, y = where(is.double))

# Sort by frequency
mtcars |> 
  plot2(carb, y = n(), x.sort = "freq-desc")
```

For a comprehensive guide to using `plot2`, including advanced features and customisation options, please see the full vignette [here]({{url-placeholder}}).

## Getting Involved

We welcome contributions to `plot2`, whether it's through reporting issues, suggesting features, or submitting pull requests. If you're familiar with `ggplot2` and the tidyverse, your insights will be especially valuable as we continue to develop and refine the package.

## License

This project is licensed under the GPL-v2 License - see the [LICENSE](LICENSE) file for details.


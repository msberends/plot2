# A00 Argument inventory of plot2()

**Generated** by `review_and_overhaul/tools/argument_inventory.R` on 2026-11-08 (commit `7a60828`). Do not edit by hand; edit the sheets instead. A CSV copy is next to this file.

Columns: *default* is the default in the generic `plot2()`; *method differences* lists S3 methods (and `plot2_exec()`) whose default differs from the generic or that lack the argument; *roxygen block* is the `@param` line in `R/plot2.R` that documents it; *documented default* quotes the sentences of that block that mention a default; *validated by* lists the `validate_*()` calls in `plot2_exec()` that receive the argument; *exec refs* and *method refs* count the symbol in the bodies of `plot2_exec()` and of the generic plus methods; *tests* counts `arg =` uses in `tests/testthat/` (uses / files); *vign.* counts `arg =` uses in the vignettes and README.Rmd.

## Summary

- Arguments in the generic: 153; groups: (core) (42), category (17), datalabels (7), facet (17), legend (8), sankey (4), smooth (8), subtitle (2), title (2), x (24), y (15), y_secondary (7).
- Arguments without a roxygen `@param`: none.
- Arguments never referenced in `plot2_exec()` nor in the generic or methods: none.
- Arguments never used in any test: 76 of 153.
- Arguments never shown in vignettes or README: 123 of 153.
- Extra arguments of methods, not in the generic: `plot2.sf(crs)`, `plot2.sf(datalabels.centroid)`.
- Naming styles of the part after the group prefix: dot (5), single word (112), snake (35).

## Inventory

| # | argument | group | default | method differences | roxygen block | documented default | validated by | exec refs | method refs | tests | vign. |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `.data` | (core) | `(none)` | formula: `NULL` | `.data` |  | validate_data, validate_title | 34 | 44 | 0 / 0 | 0 |
| 2 | `x` | (core) | `NULL` | freq: `.data$item` | `x` |  | validate_data | 25 | 24 | 98 / 19 | 53 |
| 3 | `y` | (core) | `NULL` | freq: `.data$count` | `y` |  | validate_data, validate_y_scale | 18 | 28 | 99 / 18 | 54 |
| 4 | `category` | (core) | `NULL` |  | `category,facet` |  | validate_data | 3 | 24 | 23 / 7 | 27 |
| 5 | `facet` | (core) | `NULL` |  | `category,facet` |  | validate_data | 2 | 23 | 29 / 10 | 9 |
| 6 | `type` | (core) | `NULL` |  | `type` | At default, it also sets `x.expand = c(0.05, 0.05)` and `y.limits = c(NA, NA)` and `y.expand = c(0.01, 0.01)`. In this case, the type wil... | validate_category_scale, validate_colour, validate_data, validate_facet, validate_linewidth, validate_size, validate_theme, validate_type, validate_width, validate_y_scale | 67 | 9 | 116 / 20 | 31 |
| 7 | `x.title` | x | `TRUE` | matrix: `FALSE`; sf: `FALSE`; freq: `"Item"` | `title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title` | This can be: - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default... | validate_markdown, validate_title | 12 | 15 | 5 / 3 | 10 |
| 8 | `y.title` | y | `TRUE` | matrix: `FALSE`; sf: `FALSE`; freq: `"Count"` | `title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title` | This can be: - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default... | validate_data, validate_markdown, validate_title | 17 | 15 | 6 / 4 | 11 |
| 9 | `category.title` | category | `NULL` | freq: `TRUE` | `title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title` | This can be: - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default... | validate_title | 5 | 7 | 3 / 3 | 3 |
| 10 | `title` | (core) | `NULL` |  | `title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title` | This can be: - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default... | validate_markdown, validate_title | 9 | 15 | 19 / 7 | 17 |
| 11 | `subtitle` | (core) | `NULL` |  | `title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title` | This can be: - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default... | validate_markdown, validate_title | 9 | 15 | 7 / 4 | 0 |
| 12 | `caption` | (core) | `NULL` |  | `title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title` | This can be: - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default... | validate_markdown, validate_title | 8 | 15 | 5 / 2 | 2 |
| 13 | `tag` | (core) | `NULL` |  | `title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title` | This can be: - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default... | validate_markdown, validate_title | 6 | 15 | 5 / 2 | 0 |
| 14 | `title.linelength` | title | `60` |  | `title.linelength` |  | validate_title | 3 | 7 | 0 / 0 | 0 |
| 15 | `title.colour` | title | `getOption("plot2.colour_font_primary", "black")` |  | `title.colour` |  | validate_theme | 3 | 7 | 0 / 0 | 0 |
| 16 | `subtitle.linelength` | subtitle | `60` |  | `subtitle.linelength` |  | validate_title | 3 | 7 | 0 / 0 | 0 |
| 17 | `subtitle.colour` | subtitle | `getOption("plot2.colour_font_secondary", "grey35")` |  | `subtitle.colour` |  | validate_theme | 3 | 7 | 0 / 0 | 0 |
| 18 | `na.replace` | (core) | `""` |  | `na.replace` |  | validate_data | 2 | 6 | 0 / 0 | 0 |
| 19 | `na.rm` | (core) | `FALSE` |  | `na.rm` |  | validate_data | 5 | 6 | 4 / 1 | 0 |
| 20 | `facet.position` | facet | `"top"` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_facet | 2 | 6 | 0 / 0 | 0 |
| 21 | `facet.fill` | facet | `NULL` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 22 | `facet.bold` | facet | `TRUE` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 23 | `facet.italic` | facet | `FALSE` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 24 | `facet.size` | facet | `10` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 25 | `facet.margin` | facet | `8` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 26 | `facet.repeat_lbls_x` | facet | `TRUE` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_facet | 2 | 6 | 2 / 1 | 0 |
| 27 | `facet.repeat_lbls_y` | facet | `NULL` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_facet | 2 | 6 | 2 / 1 | 0 |
| 28 | `facet.fixed_y` | facet | `NULL` |  | `facet.fixed_y` | Defaults to `TRUE` only if the coefficient of variation (standard deviation divided by mean) of the maximum values of y is less than 25%. | validate_facet, validate_y_scale | 6 | 6 | 1 / 1 | 2 |
| 29 | `facet.fixed_x` | facet | `NULL` |  | `facet.fixed_x` |  | validate_x_scale | 3 | 12 | 2 / 1 | 0 |
| 30 | `facet.drop` | facet | `FALSE` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_facet | 2 | 6 | 0 / 0 | 0 |
| 31 | `facet.nrow` | facet | `NULL` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_facet | 2 | 6 | 1 / 1 | 1 |
| 32 | `facet.relative` | facet | `FALSE` |  | `facet.position,facet.fill,facet.bold,facet.italic,facet.size,facet.margin,facet.repeat_lbls_x,facet.repeat_lbls_y,facet.drop,facet.nrow,facet.relative` |  | validate_facet | 2 | 6 | 2 / 1 | 0 |
| 33 | `x.date_breaks` | x | `NULL` |  | `x.date_breaks` | Breaks to use when the X-axis contains dates, will be determined automatically if left blank. | validate_x_scale | 2 | 6 | 0 / 0 | 0 |
| 34 | `x.date_labels` | x | `NULL` |  | `x.date_labels` | Labels to use when the X-axis contains dates, will be determined automatically if left blank. | validate_x_scale | 2 | 6 | 0 / 0 | 0 |
| 35 | `x.date_remove_years` | x | `NULL` |  | `x.date_remove_years` | The default is `FALSE`, unless `category` contains all years present in `x`. | validate_data | 2 | 6 | 1 / 1 | 0 |
| 36 | `category.focus` | category | `NULL` |  | `category.focus` |  |  | 11 | 6 | 7 / 2 | 0 |
| 37 | `colour` | (core) | `getOption("plot2.colour", "ggplot2")` | sf: `getOption("plot2.colour_sf", "grey50")` | `colour` |  | validate_colour, validate_theme | 19 | 9 | 27 / 11 | 15 |
| 38 | `colour_fill` | (core) | `NULL` | sf: `getOption("plot2.colour_sf_fill", getOption("plot2.colour...` | `colour_fill` | Get_colour(s) to be used for filling, will be determined automatically if left blank and will be evaluated with [get_colour()]. | validate_category_scale, validate_colour | 10 | 13 | 3 / 1 | 3 |
| 39 | `colour_opacity` | (core) | `0` |  | `colour_opacity` |  | validate_colour | 3 | 6 | 0 / 0 | 0 |
| 40 | `x.lbl_angle` | x | `0` |  | `x.lbl_angle` |  | validate_theme | 2 | 7 | 2 / 1 | 0 |
| 41 | `x.lbl_align` | x | `NULL` |  | `x.lbl_align` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 42 | `x.lbl_italic` | x | `FALSE` |  | `x.lbl_italic` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 43 | `x.lbl_taxonomy` | x | `FALSE` |  | `x.lbl_taxonomy` |  |  | 2 | 6 | 2 / 1 | 0 |
| 44 | `x.remove` | x | `FALSE` |  | `x.remove,y.remove` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 45 | `x.position` | x | `"bottom"` |  | `x.position,y.position` |  | validate_x_scale | 2 | 6 | 0 / 0 | 0 |
| 46 | `x.max_items` | x | `Inf` |  | `x.max_items,category.max_items,facet.max_items` | Number of maximum items to use, defaults to infinite. | validate_data | 4 | 12 | 2 / 1 | 2 |
| 47 | `x.max_txt` | x | `"(rest, x%n)"` |  | `x.max_txt,category.max_txt,facet.max_txt` |  | validate_data | 2 | 6 | 0 / 0 | 0 |
| 48 | `category.max_items` | category | `Inf` |  | `x.max_items,category.max_items,facet.max_items` | Number of maximum items to use, defaults to infinite. | validate_data | 2 | 6 | 1 / 1 | 0 |
| 49 | `category.max_txt` | category | `"(rest, x%n)"` |  | `x.max_txt,category.max_txt,facet.max_txt` |  | validate_data | 2 | 6 | 0 / 0 | 0 |
| 50 | `facet.max_items` | facet | `Inf` |  | `x.max_items,category.max_items,facet.max_items` | Number of maximum items to use, defaults to infinite. | validate_data | 2 | 6 | 1 / 1 | 0 |
| 51 | `facet.max_txt` | facet | `"(rest, x%n)"` |  | `x.max_txt,category.max_txt,facet.max_txt` |  | validate_data | 2 | 6 | 0 / 0 | 0 |
| 52 | `x.breaks` | x | `NULL` |  | `x.breaks,y.breaks` |  | validate_x_scale | 2 | 6 | 0 / 0 | 0 |
| 53 | `x.n_breaks` | x | `NULL` |  | `x.n_breaks,y.n_breaks` |  | validate_x_scale | 2 | 6 | 0 / 0 | 0 |
| 54 | `x.transform` | x | `"identity"` |  | `x.transform,y.transform,category.transform` |  | validate_x_scale | 2 | 6 | 1 / 1 | 0 |
| 55 | `x.expand` | x | `NULL` | sf: `0` | `x.expand,y.expand` | `x.expand` defaults to 0.5 and `y.expand` defaults to `0.25`, except for sf objects (then both default to 0). | validate_x_scale | 7 | 6 | 0 / 0 | 0 |
| 56 | `x.limits` | x | `NULL` |  | `x.limits,y.limits` |  | validate_x_scale | 7 | 6 | 4 / 1 | 0 |
| 57 | `x.labels` | x | `NULL` |  | `x.labels,y.labels,y_secondary.labels` |  | validate_x_scale | 4 | 6 | 0 / 0 | 0 |
| 58 | `x.character` | x | `NULL` |  | `x.character` | The default is `FALSE`, except for years (values between 2000 and 2050) and months (values from 1 to 12). | validate_data | 2 | 6 | 0 / 0 | 0 |
| 59 | `x.drop` | x | `FALSE` |  | `x.drop` |  | validate_data, validate_x_scale | 6 | 6 | 2 / 1 | 0 |
| 60 | `x.mic` | x | `FALSE` |  | `x.mic` |  | validate_data | 3 | 6 | 2 / 1 | 0 |
| 61 | `x.zoom` | x | `FALSE` |  | `x.zoom,y.zoom` |  | validate_x_scale | 4 | 12 | 0 / 0 | 0 |
| 62 | `y.remove` | y | `FALSE` |  | `x.remove,y.remove` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 63 | `y.24h` | y | `FALSE` |  | `y.24h` |  | validate_y_scale | 3 | 6 | 1 / 1 | 0 |
| 64 | `y.age` | y | `FALSE` |  | `y.age` |  | validate_y_scale | 3 | 6 | 1 / 1 | 0 |
| 65 | `y.scientific` | y | `NULL` |  | `y.scientific,y_secondary.scientific` | Defaults to `TRUE` only if the range of the y values spans more than `10e5`. | validate_y_scale | 3 | 6 | 2 / 1 | 0 |
| 66 | `y.percent` | y | `FALSE` |  | `y.percent,y_secondary.percent` |  | validate_data, validate_y_scale | 6 | 12 | 3 / 1 | 0 |
| 67 | `y.percent_break` | y | `0.1` |  | `y.percent_break` |  | validate_y_scale | 3 | 12 | 1 / 1 | 0 |
| 68 | `y.breaks` | y | `NULL` |  | `x.breaks,y.breaks` |  | validate_y_scale | 3 | 6 | 0 / 0 | 0 |
| 69 | `y.n_breaks` | y | `NULL` |  | `x.n_breaks,y.n_breaks` |  | validate_y_scale | 3 | 6 | 2 / 1 | 0 |
| 70 | `y.limits` | y | `NULL` |  | `x.limits,y.limits` |  | validate_y_scale | 11 | 6 | 1 / 1 | 1 |
| 71 | `y.labels` | y | `NULL` |  | `x.labels,y.labels,y_secondary.labels` |  | validate_y_scale | 3 | 6 | 0 / 0 | 3 |
| 72 | `y.expand` | y | `NULL` | sf: `0` | `x.expand,y.expand` | `x.expand` defaults to 0.5 and `y.expand` defaults to `0.25`, except for sf objects (then both default to 0). | validate_y_scale | 8 | 6 | 0 / 0 | 0 |
| 73 | `y.transform` | y | `"identity"` |  | `x.transform,y.transform,category.transform` |  | validate_y_scale | 4 | 6 | 3 / 1 | 1 |
| 74 | `y.position` | y | `"left"` |  | `x.position,y.position` |  | validate_y_scale | 3 | 6 | 0 / 0 | 0 |
| 75 | `y.zoom` | y | `FALSE` |  | `x.zoom,y.zoom` |  | validate_y_scale | 4 | 6 | 0 / 0 | 0 |
| 76 | `y_secondary` | (core) | `NULL` |  | `y_secondary` |  | validate_data | 3 | 13 | 7 / 4 | 0 |
| 77 | `y_secondary.type` | y_secondary | `type` |  | `y_secondary.type` |  | validate_type | 5 | 5 | 2 / 1 | 0 |
| 78 | `y_secondary.title` | y_secondary | `TRUE` |  | `title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title` | This can be: - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default... | validate_title, validate_y_scale | 6 | 5 | 0 / 0 | 0 |
| 79 | `y_secondary.colour` | y_secondary | `colour` | default: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; data.frame: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; formula: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; matrix: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; sf: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; freq: `get_colour(getOption("plot2.colour", "ggplot2"), 2)` | `y_secondary.colour,y_secondary.colour_fill` |  | validate_theme | 6 | 5 | 2 / 1 | 0 |
| 80 | `y_secondary.colour_fill` | y_secondary | `colour_fill` | default: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; data.frame: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; formula: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; matrix: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; sf: `get_colour(getOption("plot2.colour", "ggplot2"), 2)`; freq: `get_colour(getOption("plot2.colour", "ggplot2"), 2)` | `y_secondary.colour,y_secondary.colour_fill` |  |  | 4 | 5 | 0 / 0 | 0 |
| 81 | `y_secondary.scientific` | y_secondary | `NULL` |  | `y.scientific,y_secondary.scientific` | Defaults to `TRUE` only if the range of the y values spans more than `10e5`. | validate_y_scale | 2 | 5 | 0 / 0 | 0 |
| 82 | `y_secondary.percent` | y_secondary | `FALSE` |  | `y.percent,y_secondary.percent` |  | validate_y_scale | 2 | 5 | 0 / 0 | 0 |
| 83 | `y_secondary.labels` | y_secondary | `NULL` |  | `x.labels,y.labels,y_secondary.labels` |  | validate_y_scale | 2 | 5 | 0 / 0 | 0 |
| 84 | `category.type` | category | `"colour"` |  | `category.type` | `"colour"` (default), `"shape"`, `"size"`, `"linetype"`, `"linewidth"`, `"alpha"`. | validate_category_scale | 24 | 6 | 7 / 3 | 0 |
| 85 | `category.labels` | category | `NULL` |  | `category.labels,category.percent,category.breaks,category.expand,category.midpoint` |  | validate_category_scale, validate_data | 10 | 6 | 1 / 1 | 0 |
| 86 | `category.percent` | category | `FALSE` |  | `category.labels,category.percent,category.breaks,category.expand,category.midpoint` |  | validate_category_scale | 2 | 6 | 0 / 0 | 0 |
| 87 | `category.breaks` | category | `NULL` |  | `category.labels,category.percent,category.breaks,category.expand,category.midpoint` |  | validate_category_scale | 2 | 6 | 0 / 0 | 0 |
| 88 | `category.limits` | category | `NULL` |  | `category.limits` |  | validate_category_scale | 2 | 6 | 2 / 1 | 0 |
| 89 | `category.expand` | category | `0` |  | `category.labels,category.percent,category.breaks,category.expand,category.midpoint` |  | validate_category_scale | 2 | 6 | 0 / 0 | 0 |
| 90 | `category.midpoint` | category | `NULL` |  | `category.labels,category.percent,category.breaks,category.expand,category.midpoint` |  | validate_category_scale | 2 | 6 | 0 / 0 | 0 |
| 91 | `category.transform` | category | `"identity"` |  | `x.transform,y.transform,category.transform` |  | validate_category_scale | 2 | 6 | 1 / 1 | 0 |
| 92 | `category.date_breaks` | category | `NULL` |  | `category.date_breaks` | Breaks to use when the category contains dates, will be determined automatically if left blank. | validate_category_scale | 2 | 6 | 1 / 1 | 0 |
| 93 | `category.date_labels` | category | `NULL` |  | `category.date_labels` | Labels to use when the category contains dates, will be determined automatically if left blank. | validate_category_scale | 2 | 6 | 1 / 1 | 0 |
| 94 | `category.character` | category | `NULL` |  | `category.character` | The default is `FALSE`, except for years (values between 2000 and 2050) and months (values from 1 to 12). | validate_data | 2 | 6 | 0 / 0 | 2 |
| 95 | `x.sort` | x | `NULL` | freq: `"freq-desc"` | `x.sort,category.sort,facet.sort` | Sorting of the plotting direction, defaults to `TRUE`, except for continuous values on the X-axis (such as dates and numbers). | validate_data, validate_sorting | 13 | 6 | 11 / 2 | 11 |
| 96 | `category.sort` | category | `TRUE` |  | `x.sort,category.sort,facet.sort` | Sorting of the plotting direction, defaults to `TRUE`, except for continuous values on the X-axis (such as dates and numbers). | validate_data, validate_sorting | 3 | 6 | 2 / 1 | 0 |
| 97 | `facet.sort` | facet | `TRUE` |  | `x.sort,category.sort,facet.sort` | Sorting of the plotting direction, defaults to `TRUE`, except for continuous values on the X-axis (such as dates and numbers). | validate_data | 2 | 6 | 0 / 0 | 0 |
| 98 | `x.complete` | x | `NULL` |  | `x.complete,category.complete,facet.complete` |  | validate_data | 2 | 6 | 1 / 1 | 0 |
| 99 | `category.complete` | category | `NULL` |  | `x.complete,category.complete,facet.complete` |  | validate_data | 2 | 6 | 0 / 0 | 0 |
| 100 | `facet.complete` | facet | `NULL` |  | `x.complete,category.complete,facet.complete` |  | validate_data | 2 | 6 | 0 / 0 | 0 |
| 101 | `datalabels` | (core) | `TRUE` |  | `datalabels` | This will default to the values of `y` in column-type plots, or when plotting spatial 'sf' data, the values of the first column. | validate_data | 5 | 12 | 8 / 3 | 2 |
| 102 | `datalabels.round` | datalabels | `ifelse(y.percent, 2, 1)` |  | `datalabels.round` |  | validate_data | 3 | 6 | 0 / 0 | 0 |
| 103 | `datalabels.format` | datalabels | `"%n"` | sf: `NULL` | `datalabels.format` |  | validate_data | 4 | 6 | 0 / 0 | 0 |
| 104 | `datalabels.colour` | datalabels | `"grey25"` | sf: `"black"` | `datalabels.colour,datalabels.colour_fill,datalabels.size,datalabels.angle,datalabels.lineheight` |  |  | 4 | 6 | 1 / 1 | 0 |
| 105 | `datalabels.colour_fill` | datalabels | `NULL` |  | `datalabels.colour,datalabels.colour_fill,datalabels.size,datalabels.angle,datalabels.lineheight` |  |  | 3 | 6 | 0 / 0 | 0 |
| 106 | `datalabels.size` | datalabels | `(3 * text_factor)` |  | `datalabels.colour,datalabels.colour_fill,datalabels.size,datalabels.angle,datalabels.lineheight` |  |  | 3 | 6 | 0 / 0 | 0 |
| 107 | `datalabels.angle` | datalabels | `0` |  | `datalabels.colour,datalabels.colour_fill,datalabels.size,datalabels.angle,datalabels.lineheight` |  |  | 3 | 6 | 0 / 0 | 0 |
| 108 | `datalabels.lineheight` | datalabels | `1` |  | `datalabels.colour,datalabels.colour_fill,datalabels.size,datalabels.angle,datalabels.lineheight` |  |  | 2 | 6 | 0 / 0 | 0 |
| 109 | `decimal.mark` | (core) | `dec_mark()` |  | `decimal.mark` | Decimal mark, defaults to [dec_mark()]. | validate_category_scale, validate_data, validate_x_scale, validate_y_scale | 9 | 6 | 1 / 1 | 0 |
| 110 | `big.mark` | (core) | `big_mark()` |  | `big.mark` | Thousands separator, defaults to [big_mark()]. | validate_category_scale, validate_data, validate_x_scale, validate_y_scale | 10 | 6 | 1 / 1 | 0 |
| 111 | `summarise_function` | (core) | `base::sum` |  | `summarise_function` |  | validate_data | 10 | 18 | 3 / 1 | 0 |
| 112 | `stacked` | (core) | `FALSE` |  | `stacked` |  | validate_y_scale | 8 | 6 | 2 / 2 | 3 |
| 113 | `stacked_fill` | (core) | `FALSE` |  | `stacked_fill` |  | validate_category_scale, validate_y_scale | 8 | 6 | 1 / 1 | 3 |
| 114 | `horizontal` | (core) | `FALSE` |  | `horizontal` |  | validate_colour, validate_data, validate_facet, validate_sorting, validate_theme, validate_x_scale | 17 | 7 | 5 / 4 | 0 |
| 115 | `reverse` | (core) | `horizontal` |  | `reverse` |  |  | 4 | 6 | 0 / 0 | 0 |
| 116 | `smooth` | (core) | `NULL` |  | `smooth` | In histograms, this will add the density count as an overlaying line (default: In all other cases, a smooth will be added using [`geom_sm... |  | 4 | 6 | 4 / 3 | 0 |
| 117 | `smooth.method` | smooth | `NULL` |  | `smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.linewidth,smooth.linetype,smooth.colour` |  |  | 3 | 6 | 0 / 0 | 0 |
| 118 | `smooth.formula` | smooth | `NULL` |  | `smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.linewidth,smooth.linetype,smooth.colour` |  |  | 3 | 6 | 0 / 0 | 0 |
| 119 | `smooth.se` | smooth | `TRUE` |  | `smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.linewidth,smooth.linetype,smooth.colour` |  |  | 3 | 6 | 0 / 0 | 0 |
| 120 | `smooth.level` | smooth | `0.95` |  | `smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.linewidth,smooth.linetype,smooth.colour` |  |  | 3 | 6 | 0 / 0 | 0 |
| 121 | `smooth.alpha` | smooth | `NULL` |  | `smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.linewidth,smooth.linetype,smooth.colour` |  |  | 3 | 6 | 0 / 0 | 0 |
| 122 | `smooth.linewidth` | smooth | `NULL` |  | `smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.linewidth,smooth.linetype,smooth.colour` |  |  | 5 | 6 | 0 / 0 | 0 |
| 123 | `smooth.linetype` | smooth | `NULL` |  | `smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.linewidth,smooth.linetype,smooth.colour` |  |  | 5 | 6 | 0 / 0 | 0 |
| 124 | `smooth.colour` | smooth | `NULL` |  | `smooth.method,smooth.formula,smooth.se,smooth.level,smooth.alpha,smooth.linewidth,smooth.linetype,smooth.colour` |  |  | 6 | 6 | 1 / 1 | 0 |
| 125 | `size` | (core) | `NULL` |  | `size` | Defaults to `2` for geoms [point][ggplot2::geom_point()], [jitter][ggplot2::geom_jitter()], and [beeswarm][ggbeeswarm::geom_beeswarm()]; | validate_size | 14 | 6 | 2 / 2 | 6 |
| 126 | `linetype` | (core) | `1` |  | `linetype` | `"solid"` (default) 2. |  | 4 | 6 | 0 / 0 | 1 |
| 127 | `linewidth` | (core) | `NULL` |  | `linewidth` | Defaults to: - `0.10` for [sf][ggplot2::geom_sf()] - `0.25` for geoms that are continous and have fills (such as [area][ggplot2::geom_are... | validate_linewidth | 12 | 6 | 1 / 1 | 1 |
| 128 | `binwidth` | (core) | `NULL` |  | `binwidth` | It defaults to approx. |  | 4 | 6 | 0 / 0 | 0 |
| 129 | `width` | (core) | `NULL` |  | `width` | Defaults to `0.66` for geoms [boxplot][ggplot2::geom_boxplot()], [violin][ggplot2::geom_violin()] and [jitter][ggplot2::geom_jitter()], a... | validate_width | 13 | 6 | 1 / 1 | 2 |
| 130 | `jitter_seed` | (core) | `NA` |  | `jitter_seed` |  |  | 3 | 6 | 0 / 0 | 0 |
| 131 | `violin_scale` | (core) | `"count"` |  | `violin_scale` |  |  | 3 | 6 | 0 / 0 | 0 |
| 132 | `legend.position` | legend | `NULL` | sf: `"right"` | `legend.position` | Defaults to `"right"` for numeric `category` values and 'sf' plots, and `"top"` otherwise. | validate_category_scale, validate_legend.position | 9 | 6 | 2 / 2 | 0 |
| 133 | `legend.title` | legend | `NULL` |  | `title,subtitle,caption,tag,x.title,y.title,category.title,legend.title,y_secondary.title` | This can be: - A [character], which supports markdown by using [md_to_expression()] internally if `markdown = TRUE` (which is the default... | validate_markdown, validate_title | 13 | 7 | 0 / 0 | 0 |
| 134 | `legend.reverse` | legend | `FALSE` |  | `legend.reverse,legend.barheight,legend.barwidth,legend.nbin,legend.italic,legend.nrow` |  | validate_category_scale | 4 | 6 | 2 / 1 | 0 |
| 135 | `legend.nrow` | legend | `NULL` |  | `legend.reverse,legend.barheight,legend.barwidth,legend.nbin,legend.italic,legend.nrow` |  | validate_category_scale | 6 | 6 | 1 / 1 | 0 |
| 136 | `legend.barheight` | legend | `6` |  | `legend.reverse,legend.barheight,legend.barwidth,legend.nbin,legend.italic,legend.nrow` |  | validate_category_scale | 2 | 6 | 0 / 0 | 0 |
| 137 | `legend.barwidth` | legend | `1.5` |  | `legend.reverse,legend.barheight,legend.barwidth,legend.nbin,legend.italic,legend.nrow` |  | validate_category_scale | 2 | 6 | 0 / 0 | 0 |
| 138 | `legend.nbin` | legend | `300` |  | `legend.reverse,legend.barheight,legend.barwidth,legend.nbin,legend.italic,legend.nrow` |  | validate_category_scale | 2 | 6 | 0 / 0 | 0 |
| 139 | `legend.italic` | legend | `FALSE` |  | `legend.reverse,legend.barheight,legend.barwidth,legend.nbin,legend.italic,legend.nrow` |  | validate_theme | 2 | 7 | 0 / 0 | 0 |
| 140 | `sankey.node_width` | sankey | `0.15` |  | `sankey.node_width` |  |  | 3 | 6 | 0 / 0 | 0 |
| 141 | `sankey.node_whitespace` | sankey | `0.03` |  | `sankey.node_whitespace` |  |  | 4 | 6 | 0 / 0 | 0 |
| 142 | `sankey.alpha` | sankey | `0.5` |  | `sankey.alpha` |  |  | 2 | 6 | 1 / 1 | 0 |
| 143 | `sankey.remove_axes` | sankey | `NULL` |  | `sankey.remove_axes` |  | validate_theme | 4 | 7 | 0 / 0 | 0 |
| 144 | `zoom` | (core) | `FALSE` |  | `zoom` |  |  | 2 | 6 | 2 / 1 | 0 |
| 145 | `sep` | (core) | `" / "` |  | `sep` |  | validate_data | 4 | 6 | 1 / 1 | 0 |
| 146 | `print` | (core) | `FALSE` |  | `print` |  |  | 5 | 8 | 7 / 4 | 0 |
| 147 | `text_factor` | (core) | `getOption("plot2.text_factor", 1)` |  | `text_factor` |  | validate_theme | 4 | 7 | 0 / 0 | 1 |
| 148 | `font` | (core) | `getOption("plot2.font")` |  | `font` |  | validate_category_scale, validate_font, validate_theme | 8 | 7 | 4 / 1 | 1 |
| 149 | `theme` | (core) | `getOption("plot2.theme", "theme_minimal2")` | sf: `theme_minimal2(panel.grid.major = element_blank(), panel....` | `theme` | A valid `ggplot2` [theme][ggplot2::theme()] to apply, or `NULL` to use the default [`theme_grey()`][ggplot2::theme_grey()]. The default i... | validate_theme | 10 | 7 | 2 / 2 | 0 |
| 150 | `background` | (core) | `getOption("plot2.colour_background", "white")` |  | `background` |  | validate_theme | 6 | 7 | 1 / 1 | 0 |
| 151 | `markdown` | (core) | `TRUE` |  | `markdown` | A [logical] to turn all labels and titles into [plotmath] expressions, by converting common markdown language using the [md_to_expression... | validate_markdown, validate_title, validate_y_scale | 26 | 14 | 1 / 1 | 0 |
| 152 | `data` | (core) | `NULL` | default: absent; data.frame: absent; matrix: absent; freq: absent | `data` |  |  | 7 | 2 | 8 / 3 | 1 |
| 153 | `...` | (core) | `(none)` |  | `...` |  | validate_data | 2 | 26 | 0 / 0 | 0 |

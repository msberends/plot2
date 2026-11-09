# A07 Help pages of the exported helpers, options and data

`R/add_type.R`, `R/add_mapping.R`, `R/move_layer.R`, `R/coord_spider.R`, `R/theme_minimal2.R`, `R/get_colour.R`, `R/dec_mark.R`, `R/labellers.R`, `R/md_to_expression.R`, `R/get_plot_title.R`, `R/plotly.R`, `R/options.R`, `R/data.R`, `R/aa_pkg.R`; `create_interactively()` text only (R07).

## Checked

- All examples run (`plotly` examples with plotly installed in a temporary library). Messages only from `register_colour()` and ggplot2's smooth notes.
- `md_to_expression()`: `@param` says only the first value is used; `@return` and the code handle all values (F099).
- `dec_mark()` and `big_mark()`: behaviour as documented, apart from F015.
- `add_type()`: the `move` text contradicts the example (F099; positive moves broken, F083). `add_errorbar()` "only adds error bars to the y values" holds, but only with a category (F082).
- `move_layer()`: no examples, no return value, sign of `move` and its default undocumented (F104).
- `register_colour()`, `unregister_colour()`: no return value documented (F104).
- `?labellers`: title mentions Euro only; examples in `\dontrun{}` without need (F099, F104).
- `get_colour()`: random example colours and the R version in the text (F103).
- `?plot2-options`: errors for `plot2.colour_font_axis`, `_secondary`, `plot2.silent`; no defaults given; `dec_mark` and `big_mark` lack the `plot2.` prefix (F099, I004).
- Data: dimensions and values of `admitted_patients` and `netherlands` match; "geometies" (F099).
- `coord_spider()`: documentation detailed and accurate apart from F086.

## Findings

F099, F103, F104 (new); F015, F082, F083, F086 (existing).

## End-to-end test cases

- [x] A07.01 `md_to_expression()` converts every element.
- [x] A07.02 `big_mark()` follows `dec_mark()` and accepts `""`.
- [x] A07.03 `add_line()` with a single y or x gives hline or vline.
- [x] A07.04 `add_*(move = -1)` puts the new layer below the top layer.
- [x] A07.05 `move_layer()` moves the top layer one down by default.
- [x] A07.06 Every example of seven helper help pages runs without error.
- [x] A07.07 Bundled data match their documentation.
- [x] A07.08 (skipped: F099) The options page describes each option correctly.

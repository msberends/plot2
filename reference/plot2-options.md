# `plot2()` Options

These are the package options for usage in
[`plot2()`](https://msberends.github.io/plot2/reference/plot2.md). These
can all be set using [`options()`](https://rdrr.io/r/base/options.html),
and their values can be retrieved using
[`getOption()`](https://rdrr.io/r/base/options.html).

## Package Options

- `plot2.big.mark`  
  Default character for a thousands separator: `","`, `"."`, `" "` or
  `""`. Defaults to `"."` if the decimal mark is a comma, and `" "`
  otherwise

- `plot2.colour`  
  Default colour to use in plots, defaults to `"ggplot2"` (the colours
  of `ggplot2` itself)

- `plot2.colour_background`  
  Default background colour to use in plots, defaults to `"white"`

- `plot2.colour_font_axis`  
  Default colour of the axis text (`axis.title.x` and `axis.title.y`),
  defaults to `"grey25"`

- `plot2.colour_font_primary`  
  Default colour of the plot title and plot tag, defaults to `"black"`

- `plot2.colour_font_secondary`  
  Default colour of the plot subtitle and caption, defaults to
  `"grey35"`

- `plot2.colour_sf`  
  Default colour of the borders in sf plots, defaults to `"grey50"`

- `plot2.colour_sf_fill`  
  Default colour of the area fills in sf plots, defaults to the option
  `plot2.colour`

- `plot2.decimal.mark`  
  Default character for a decimal separator: `","` or `"."`. Defaults to
  the [option](https://rdrr.io/r/base/options.html) `OutDec`, which is
  `"."` in a standard R session

- `plot2.default_type`  
  Default plot type when `type` is left blank and cannot be determined
  from the data, defaults to `"col"`. Any value of `type` can be used,
  such as `"line"` or `"l"`

- `plot2.font`  
  Default plot font family, defaults to the font of the graphics device.
  See the `font` argument of
  [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)

- `plot2.language`  
  Language of the words that plot2 generates (such as automatic titles
  and the labels of `y.24h` and `y.age`) and of month and weekday names
  in date labels: `"en"` (default) or `"nl"` (Dutch). If not set, date
  labels follow the system locale.

- `plot2.silent`  
  A [logical](https://rdrr.io/r/base/logical.html) to hide the console
  messages when plotting (`TRUE`), default is `FALSE`. Messages are
  printed only in interactive sessions (and pkgdown builds), at the end
  of the
  [`plot2()`](https://msberends.github.io/plot2/reference/plot2.md)
  call. They are signalled as R conditions of class `plot2_message` (and
  `plot2_caution` for cautions), so they can also be caught with
  [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html) or
  hidden with
  [`suppressMessages()`](https://rdrr.io/r/base/message.html). Warnings
  are R warnings of class `plot2_warning` and are not hidden by this
  option.

- `plot2.text_factor`  
  Default factor to multiply all text sizes with, defaults to `1`

- `plot2.theme`  
  Default plot theme, defaults to `"theme_minimal2"`
  ([`theme_minimal2()`](https://msberends.github.io/plot2/reference/theme_minimal2.md))

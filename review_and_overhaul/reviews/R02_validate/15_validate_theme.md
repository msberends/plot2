# 15 validate_theme()

`R/validate.R:2047-2248`

## Role

Resolves the `theme` argument (string, function, theme object, NULL) and applies all theme-related `plot2()` arguments: background, axis removal, italic labels, label angle and alignment, title colours, facet strip styling, font family, `text_factor`, horizontal swapping of grid lines and axis lines, tile and Sankey adjustments.

## Called from

`R/plot2.R:2036`, `R/plot2.R:562` (empty data), `R/back2back_plot.R:477`.

## Contract

- In: `theme` and about 20 styling arguments.
- Out: a complete theme object.
- Side effects: evaluates a character `theme` with `eval(parse())`.

## Branches

1. "ggplot2", string with or without "()", with or without `ggplot2::`; function; theme object; invalid (warning, theme_grey); NULL (theme_grey without background changes).
2. Horizontal or vertical variants of italic, `x.remove`, `y.remove`.
3. Secondary y-axis: axis titles coloured like the geoms.
4. Label angle and automatic `hjust`.
5. Facet strip fill, bold/italic, margin, size.
6. Font family and `text_factor` for every text element (absolute sizes computed from `rel()`).
7. Tile types; horizontal swap; Sankey axis removal.

## Findings

F005 (theme as a function crashes later in `plot2_exec()`), F032 (partial themes, `$` editing of missing elements, `unit()` sizes), F051 (`x.lbl_angle` rotates the value axis when horizontal), F057 (`rel()` sizes computed against the base size instead of the parent element).

## End-to-end test cases

- [ ] 15.01 Default theme is `theme_minimal2()`; `theme = "theme_bw"`, `"theme_bw()"`, `theme_bw`, `theme_bw()` all work (F005).
- [ ] 15.02 `theme = NULL`: grey theme.
- [ ] 15.03 Invalid string: error "unknown theme"; invalid object: warning.
- [ ] 15.04 Partial theme object (F032).
- [ ] 15.05 `background = "lightyellow"` on panel and plot.
- [ ] 15.06 `x.remove`, `y.remove`, `x.lbl_italic` in both orientations.
- [ ] 15.07 `x.lbl_angle = 45`: category labels rotated with `hjust = 1`; with `horizontal = TRUE` still the category labels (F051).
- [ ] 15.08 `title.colour`, `subtitle.colour`.
- [ ] 15.09 `facet.fill`, `facet.bold`, `facet.italic`, `facet.size`, `facet.margin` in the strip element.
- [ ] 15.10 `text_factor = 2`: all text sizes doubled, including rel-sized elements of `theme_bw` (F057).
- [ ] 15.11 Horizontal: major grid lines on the other axis.
- [ ] 15.12 Secondary y-axis: axis titles coloured.

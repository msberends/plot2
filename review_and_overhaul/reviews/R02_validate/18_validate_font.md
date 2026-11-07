# 18 validate_font()

`R/validate.R:2506-2598`

## Role

Makes the requested font family available: already registered, a system font (registered with sysfonts), or downloaded from Google Fonts; enables showtext.

## Called from

`R/plot2.R:1859`, `R/add_type.R:684`.

## Contract

- In: `font` (string or NULL).
- Out: the registered family name, or `""`.
- Side effects: `showtext_auto(enable = TRUE)` for the whole session; may download and install a font; caches `sysfonts::font_files()` in `plot2_env$fonts` (removed again by `clear_plot2_env()`, so the cache does not work across calls); errors in knitr when `fig.showtext` is not set.

## Findings

F011 (download without consent, global showtext), F001 (the font cache lives in the state that is cleared after every call, so `font_files()` is rescanned every time).

## End-to-end test cases

- [ ] 18.01 `font = NULL`: family "" in all text elements.
- [ ] 18.02 Installed system font (e.g. "DejaVu Sans" if present): family set, no download (skip if absent).
- [ ] 18.03 Unknown font with downloads disabled: warning, default font (desired, F011).
- [ ] 18.04 In knitr without `fig.showtext`: informative error.
- [ ] 18.05 After the call, showtext is not left enabled globally (desired, F011).
- [ ] 18.06 Font family reaches datalabel layers.

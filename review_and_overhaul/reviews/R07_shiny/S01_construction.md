# S01 Construction: arguments, logo, inputs

## Checked

`create_interactively()` lines 38 to 586: argument handling, package checks, logo, data set collection (see S02), inputs built from `formals(plot2::plot2)` by name pattern, CSS, UI layout.

- Outside an interactive session the function returns a `shiny.appobj`; inside one it runs the app and returns nothing (F125).
- The logo: the folder of `logo_path` is published with `addResourcePath()`, so every file next to the logo is downloadable (F113). The `tryCatch` that should validate the extension computes and discards the check (F125).
- Inputs: 143 arguments in plot2(); 57 become text inputs, 31 sliders, 43 switches, 13 selectors (probe in S07). The eight `y_secondary*` arguments are excluded from every tab, so the secondary axis is unavailable (F119). Sliders with a default of 0 get the range 0 to 0 (`colour_opacity`, `category.expand`; F119). `legend.title` is left out, consistent with D001.12 because `category.title` is offered.
- `hide_generated_code`, `hide_export_buttons` and `pretty_labels` work as documented. `css_code` is inserted unescaped; it comes from the caller, not from visitors, so this is acceptable.
- Package checks: shiny, clipr, pillar and rio always; readxl with `upload_tab = TRUE`. shinyjs, bslib, DT and fansi are used but not checked (they are in Suggests; F108). clipr is needed only for the copy button (F117).

## Findings

F113, F119, F125 (new); F108 (dependency footprint) extended in effect by F117.

## End-to-end test cases

- [x] S01.01 Returns an app object outside an interactive session.
- [x] S01.02 (skipped: F119) Every plot2() argument has an input.
- [x] S01.03 (skipped: F119) Every slider has a usable range.
- [x] S01.04 (skipped: F113) A logo is served on its own, not with its folder.
- [x] S01.05 A missing logo errors; `logo_path = NULL` removes it.
- [x] S01.06 `pretty_labels` shows readable labels.
- [x] S01.07 `hide_generated_code` and `hide_export_buttons` add their CSS.

# T06 Dependency footprint

## Checked

- Use per package (NAMESPACE imports, `pkg::` calls) and recursive dependencies: see F108. `broom` (1 function, 21 recursive dependencies) and `cleaner` (2 functions, 14) are heavy for their use; `crayon` duplicates `cli`; `grDevices` is imported but listed in Suggests; 16 of 39 Suggests serve only the Shiny app.
- **Hard dependencies only** (scratch library of 43 packages, site library disabled): 12 default-path calls work, because `cli`, `rlang`, `scales`, `stringr`, `viridisLite` and `vctrs` arrive through the Imports. Only `lubridate` is missing (`format2(x, "mmm")`). F008 extended with this measurement.

## Findings

F108 (new); F008 extended; F011 (font packages in Imports).

## End-to-end test cases

- [x] T06.01 (skipped: F108) Every imported package is in Imports.
- [x] T06.02 (skipped: F008) Default-path packages are in Imports.
- [x] T06.03 Shiny-only packages are not used outside the app.

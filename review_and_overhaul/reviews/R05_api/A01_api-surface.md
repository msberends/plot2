# A01 API surface: signatures, help-page coverage and defaults

Sources: `formals()` of the generic, the six methods and `plot2_exec()`; `man/*.Rd`; the inventory (A00).

## Role

The contract users see: 153 arguments in the usage of `?plot2`, six S3 methods documented on `?plot2-methods`, 38 exports.

## Checked

- Every argument of the generic has an `@param` (inventory: none undocumented). `tools::codoc()`, `checkDocFiles()`, `checkDocStyle()`, `undoc()` and `checkRd()` report nothing.
- Methods: `data` is absent from four methods (F020); `y_secondary.colour(_fill)` differ in all six methods (F034); sf, freq and matrix change 3 to 12 defaults, undocumented (F034, F104); `plot2.sf` adds `crs` and `datalabels.centroid` (documented).
- 18 arguments act on `missing()` rather than on their value, while the usage shows a concrete default (F094, new).
- Naming: 112 single-word properties, 35 snake case, 5 dotted base-R names; exceptions collected in I004.
- Exported functions without examples: `move_layer()`; `?plot2-methods` has none either (F104).
- `man/colour.Rd` is regenerated differently on every run (F103, new).
- CLAUDE.md lists a `plot2.lm` method and an exported `format2()`; neither exists (add to F039).

## Findings

F094, F103, F104 (new); F020, F034, F035, F039 (existing); I004 and D001 (proposal and decision).

## End-to-end test cases

- [x] A01.01 Every argument of the generic is documented on `?plot2`.
- [x] A01.02 (skipped: F020) Every method accepts every argument of the generic.
- [x] A01.03 (skipped: F034) Methods use the defaults shown in the generic's usage.
- [x] A01.04 (skipped: F104) Every exported function has a help page with examples.
- [x] A01.05 (skipped: F094) Passing the usage default gives the same plot as omitting it (`datalabels`).
- [x] A01.06 (skipped: F103) No random or session-dependent text in roxygen.

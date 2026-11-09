# S07 Helpers and D001 coupling

## Checked

`make_input()` (1127), `create_field()` (1195), `label_with_dims()` (1229), `install_shiny_deps()` (1246).

- `make_input()` chooses the widget from the class of the default and from name patterns; `create_field()` builds pretty labels with regular expressions and writes each default into `plot2_env` (F112).
- Widget table (probe over all 143 arguments): 57 text, 31 slider, 43 switch, 13 selector. Script: `repro/shiny_widgets_r07.R`; rerun it after D001 to compare.
- D001 coupling (F124): D001.2 (`NULL` defaults) turns switches into text inputs; D001.7 and D001.8 break the `lbl` and `repeat_lbls_*` patterns; D001.9 the `sankey` label rule; D001.1, D001.10, D001.14 and D001.3 move arguments between tabs by name. Not testable before D001 is implemented.
- `label_with_dims()` evaluates a name to read its dimensions, with the same `eval(parse())` as S02 (F006, F114).
- `install_shiny_deps()` lists every package the app calls (S07.04 passes).

## Findings

F124 (new); F112, F118, F119.

## End-to-end test cases

- [x] S07.01 Logical arguments get a switch, sizes a slider, sorting a selector.
- [x] S07.02 Pretty labels drop the argument prefix.
- [x] S07.03 Data set labels carry the dimensions.
- [x] S07.04 `install_shiny_deps()` lists every package the app calls.

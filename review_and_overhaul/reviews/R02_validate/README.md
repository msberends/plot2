# R02 function sheets for R/validate.R

One sheet per function in `R/validate.R` (commit `d09e04d`), numbered in file order. Each sheet is the specification for that function's end-to-end tests (idea I002) and is kept up to date when the function changes.

Sheet sections:

- **Role**: what the function contributes to the plot.
- **Called from**: call sites (line numbers at `d09e04d`).
- **Contract**: inputs, output, side effects (reads or writes of `plot2_env`, messages, global state).
- **Branches**: the distinct behaviours that tests must cover.
- **Findings**: linked F-items (defects, risks).
- **End-to-end test cases**: checklist with stable case IDs (`<sheet number>.<case number>`, e.g. `07.03`). A test in `tests/testthat/` references the case ID in its description; tick the box when the test exists and passes. Cases tied to an open finding are written as the *desired* behaviour and stay red until the finding is fixed (use `skip("F041")` or `expect_failure` deliberately, never delete them).

"End-to-end" means: driven through the public `plot2()` (or `add_*()`) API and asserted on the built plot (`ggplot_build()`, `get_labs()`, `layer_data()`, scales, theme, facet). Direct unit tests of the internal function are welcome in addition, never instead.

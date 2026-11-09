# S03 Code generation

## Checked

`call_string()` (913 to 981) and the change tracking it depends on (596 to 608, 825).

- Directions come first in a fixed order (type, x, y, category, facet), multiple values as `c()`; the y transformation wraps y (S03.02, S03.03).
- Only changed inputs are emitted. The change tracker creates observers for every argument each time the set of inputs changes, never removes them, and ignores the first value of each (F123).
- "Skip if unchanged" compares with `plot2_env$shiny_defaults`, which every `plot2()` call clears, so after the first plot a setting set back to its default stays in the code (F112).
- Choosing "Automatic" after another type errors (F116).
- Types are guessed from content: numeric-looking text becomes a number (`sep = "."` becomes `NA`), `"TRUE"`/`"NULL"` become constants, everything else becomes one quoted string, so vector and expression arguments cannot be entered (F118).
- Strings are quoted without escaping: a `"` breaks the code and opens route 2 of F006. Direction values are pasted bare: route 1 of F006. `summarise_function` is evaluated directly while the code is built: route 3 of F006.
- The generated code reproduces the plot of the app (S03.10).

## Findings

F112, F116, F118, F123 (new); F006 (extended).

## End-to-end test cases

- [x] S03.01 Without changes the code is a bare `plot2()` call.
- [x] S03.02 Directions first in a fixed order, several values as `c()`.
- [x] S03.03 The y transformation wraps y.
- [x] S03.04 (skipped: F112) A setting changed back to its default is left out.
- [x] S03.05 (skipped: F116) Automatic again after another type gives valid code.
- [x] S03.06 (skipped: F006) A title with a double quote is used literally.
- [x] S03.07 (skipped: F118) A vector setting such as `y.limits` can be entered.
- [x] S03.08 (skipped: F118) Text that looks like a number stays text.
- [x] S03.09 A number typed in a text input becomes a number.
- [x] S03.10 The generated code reproduces the plot in the app.

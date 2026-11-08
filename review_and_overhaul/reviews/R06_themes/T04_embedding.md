# T04 Embedding: functions, knitr, Quarto, Shiny

## Checked

| Context | Result |
|---|---|
| Wrapper function with `{{ col }}` | data correct; title `{ { col } }` (F105, new) |
| `.data[[v]]`, `!!sym(v)`, loops over column names | data correct; titles show the code (F105) |
| Character literal `plot2(df, "Species")` | a constant, as in tidy evaluation generally; title with quotes |
| Local variables in in-line calculations | work |
| Titles and colours from function arguments | work |
| A column and a global variable with the same name | the column wins |
| Plot rendered after another `plot2()` call (Shiny-like) | unaffected in the tested case; the mechanism of F001 remains |
| knitr (R Markdown, `md_document`) | no plot2 notes in the output with or without `message = FALSE` (notes are interactive only, F014); fonts need `fig.showtext` (F011) |
| Quarto | not installed on this server, not tested |
| Shiny | not run here (R07); the per-call overhead of F110 and the shared state of F001 are the relevant risks |

## Findings

F105 (new); F001, F011, F014.

## End-to-end test cases

- [x] T04.01 (skipped: F105) Titles show the column name from a function.
- [x] T04.02 Data correct from a function.
- [x] T04.03 Local variables in in-line calculations.
- [x] T04.04 Delayed rendering unaffected.
- [x] T04.05 Knitting puts no plot2 notes in the document.

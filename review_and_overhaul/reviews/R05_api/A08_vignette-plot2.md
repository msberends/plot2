# A08 Vignette "An Introduction to plot2()"

`vignettes/plot2.Rmd` (639 lines, 46 chunks).

## Checked

- Rendered with `rmarkdown::render()` (detached): no errors. Output contains many "font family 'Fira Code' not found" warnings from the comparison figures (the font is set via `theme()`, not via plot2, and is not installed on this server).
- Every chunk run expression by expression (test A08.01, font chunks excluded): no errors.
- Prose versus code: six contradictions (F101): admissions versus patients, "unique age groups", the description of the combined example, `x.max_items` "top 5 most frequent" without sorting, "DPI set for you" versus `fig.showtext`, `top_n()`.
- The carburettor examples rely on numbers 1 to 12 becoming character (F013).
- Fonts are downloaded during the build (F011).

## Findings

F101 (new); F011, F013 (existing).

## End-to-end test cases

- [x] A08.01 Every chunk runs without error (font chunks excluded).
- [x] A08.02 `x.max_items` without sorting keeps the first values (the prose must change, not the code).
- [x] A08.03 Custom `x.sort` order with appended missing values.

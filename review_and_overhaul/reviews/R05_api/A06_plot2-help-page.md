# A06 ?plot2: description, details and examples

`R/plot2.R:15-21, 189-397`.

## Checked

- All 41 top-level example expressions run (detached run, R 4.5.1, ggplot2 4.x). Warnings: one `mbcsToSbcs` substitution from the PDF device only. The Google-font example downloads a font (F011) and is excluded from the test.
- The examples start with `options(plot2.colour = NULL, plot2.colour_sf_fill = NULL)` without restoring the user's options.
- Details: "currently has 152 arguments, all with a default value": 153, and `.data` has none (F099).
- Details list "Support for any `ggplot2` extension based on `fortify()`"; the `lm` example works through `plot2.default` and `fortify` (no `plot2.lm` method exists, contrary to CLAUDE.md; F039).
- Typos and wording: F099.
- The randomly printed note "Use options(plot2.silent = TRUE)" appeared in the example run (F002, F014).

## Findings

F099 (new); F002, F011, F014, F039 (existing).

## End-to-end test cases

- [x] A06.01 Every example of `?plot2` runs without error or warning (font example excluded).
- [x] A06.02 (skipped: F099) The details state the actual number of arguments.
- [x] A06.03 The `x.transform` list names valid transformations.

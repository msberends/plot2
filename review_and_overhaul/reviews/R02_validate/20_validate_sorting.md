# 20 validate_sorting()

`R/validate.R:2634-2659`

## Role

Normalises a sort specification: `TRUE` to "asc", `FALSE` to "inorder", "order" to "inorder", "freq" to "freq-desc", vectors to "manual"; swaps asc and desc when horizontal.

## Called from

`sort_data()`, `R/plot2.R:1152` (UpSet), `R/plot2.R:2124` (legend reversal).

## Contract

Pure function. Unknown values pass through and only fail later in `sort_data()`.

## Findings

None separately. By reading: "alpha" passes through unchanged and is handled in `sort_data()`; any non-letter characters are removed, so `"freq_desc"` becomes "freqdesc" and errors.

## End-to-end test cases

- [ ] 20.01 Each accepted spelling (TRUE, FALSE, "asc", "ascending", "desc", "descending", "order", "inorder", "freq", "freq-asc", "freq-desc", "infreq", "alpha", "alpha-desc") gives the documented x order.
- [ ] 20.02 Same set with `horizontal = TRUE`: order reversed on the axis as documented (top to bottom).
- [ ] 20.03 Invalid value: error naming the value.
- [ ] 20.04 Legend order reversed for horizontal plots except with frequency sorting.

# 04 validate_taxonomy()

`R/validate.R:679-733`

## Role

With `x.lbl_taxonomy = TRUE` (and the AMR package installed), italicises taxonomic names on the x-axis by matching words against `AMR::microorganisms` and converting the labels to plotmath expressions.

## Called from

`R/plot2.R:1328`.

## Contract

- In: prepared `df`.
- Out: `df` with x (and the named x column) replaced by expression strings (`paste(...)`), keeping factor order.
- Side effects: loads AMR; builds a vector of all family, genus, species and subspecies names on every call.

## Branches

1. No x: unchanged. 2. No taxonomic words: unchanged. 3. Factor x: levels mapped in original order. 4. "E. coli" style abbreviations.

## Findings

F058 (rebuilds the name list from about 78,000 rows per call; only x is supported, not category or facet; generic words that are also epithets may be italicised), F018 (expression building shares the markdown code path).

## End-to-end test cases

All skip without AMR.

- [ ] 04.01 "Escherichia coli" rendered in italics (label is an expression containing `italic`).
- [ ] 04.02 "E. coli" abbreviation italicised as a whole.
- [ ] 04.03 Mixed labels ("Other", "Klebsiella pneumoniae"): only the taxonomic part italic.
- [ ] 04.04 Factor x with sorting (`x.sort = "freq-desc"`) keeps its order after conversion.
- [ ] 04.05 Factor x with unused levels does not error.
- [ ] 04.06 Without AMR installed: `x.lbl_taxonomy = TRUE` is ignored with a message.
- [ ] 04.07 Setting `x.labels` together with `x.lbl_taxonomy = TRUE`: warning, taxonomy wins.

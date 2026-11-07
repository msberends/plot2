# 14 validate_title()

`R/validate.R:1971-2044`

## Role

Evaluates a title argument (literal, expression or calculation on the data such as `paste("n =", n())`), converts markdown and comparison operators, and wraps long titles.

## Called from

About 35 call sites: all titles in `plot2.R`, `back2back_plot.R`, `add_type.R`, and the secondary axis name.

## Contract

- In: `x` (quoted with `{{ }}`), `markdown`, `df` (optional, for calculations), `max_length`.
- Out: character, expression, `TRUE` or `NULL`.
- Side effects: warning on evaluation errors (the error text is turned into a warning and the title becomes empty).

## Branches

1. Empty (`NULL`, `FALSE`, `""`, `NA`): `NULL`. 2. Expression or `TRUE`: returned as is. 3. With `df`: evaluated in the data (first unique value). 4. `<br>` to newline. 5. Markdown: `!=`, `<=`, `>=` to symbols; markdown patterns to expression. 6. Wrapping at `max_length`, unless an expression.

## Findings

F018 (apostrophes, `$`), F025 (evaluates every title over the full data with `as.data.frame()` + `mutate()`), F056 (wrapping counts only ASCII characters). By reading: a calculation returning several values silently uses the first; an evaluation error becomes a warning and an empty title rather than an error.

## End-to-end test cases

- [ ] 14.01 Literal title, subtitle, caption, tag set in `get_labs()`.
- [ ] 14.02 `title = paste("n =", n())` evaluated on the data.
- [ ] 14.03 `title = FALSE`, `x.title = NULL`, `x.title = ""` remove the title.
- [ ] 14.04 `x.title = TRUE`: column name; for `y = n()` the surrogate name.
- [ ] 14.05 "a >= b" becomes "a ≥ b" with markdown; unchanged without.
- [ ] 14.06 Long title wrapped at `title.linelength`; accented title wrapped at the same length (F056).
- [ ] 14.07 Markdown title longer than the limit: warning, not wrapped.
- [ ] 14.08 Title with an apostrophe and markdown (F018).
- [ ] 14.09 `<br>` gives a line break.
- [ ] 14.10 Title referring to a non-existent column: warning that can be caught, plot still produced.
- [ ] 14.11 Expression title (`expression(alpha^2)`) passed through.

# 13 validate_markdown()

`R/validate.R:1933-1968`

## Role

Decides whether markdown parsing applies. Returns `isTRUE(markdown)` when `markdown` is given; otherwise guesses from all titles and data names.

## Called from

`R/plot2.R:555` (empty data), `R/plot2.R:1376`.

## Contract

- Out: logical.
- Side effects: message "Assuming markdown = TRUE" when guessing.

## Branches

1. `markdown` set: returned. 2. `markdown = NULL`: regex on the concatenation of all titles and direction names.

## Findings

F018 (detection concatenates titles, so patterns can span titles; any `$` counts). By reading: since the default of `markdown` is `TRUE`, the guessing branch is only reached with an explicit `markdown = NULL`, which is undocumented.

## End-to-end test cases

- [x] 13.01 `markdown = FALSE`: "*text*" stays literal in the title.
- [x] 13.02 Default: "*E. coli*" in the title becomes italic (expression).
- [x] 13.03 `markdown = NULL` with a superscript in the y name: guessed TRUE, message.
- [x] 13.04 `markdown = NULL` with two titles each containing one underscore: not guessed TRUE (F018).

# M09 get_colour() methods and colour registration

`R/get_colour.R` (methods from line 275; `get_colour()` itself was reviewed in R01)

## Role

The `colour` S3 class (methods for `as.character`, `print`, `c`, `rep`, `rev`, `unique`, vctrs casts) and session-level registration of named colours and colour sets (`register_colour()`, `unregister_colour()`), stored in `plot2_env$reg_cols`.

## Findings

F088 (new: one invalid colour collapses the vector; invalid registrations accepted; case-sensitive names), F090 (new: class lost on `[`), F016 (short hex), F033.

## End-to-end test cases

- [x] M09.01 `get_colour()` returns class colour with names kept; R colour names are case-insensitive.
- [x] M09.02 `c()`, `rev()`, `rep()` keep the class.
- [x] M09.03 `vctrs::vec_c()` and a tibble column keep the class.
- [x] M09.04 `register_colour()` of a single colour and a set; usable in `plot2()`.
- [x] M09.05 Registration requires names; `unregister_colour()` removes by pattern.
- [x] M09.06 Registered colours survive the internal state clean-up.
- [x] M09.07 (skipped: F088) One invalid colour does not replace the valid ones.
- [x] M09.08 (skipped: F088) Registering an invalid colour errors.
- [x] M09.09 (skipped: F090) Subsetting keeps the class.

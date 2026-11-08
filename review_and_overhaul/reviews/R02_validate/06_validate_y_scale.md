# 06 validate_y_scale()

`R/validate.R:951-1301`

## Role

Builds the continuous y scale (or a discrete one for factor y), including breaks, labels (percent, age, 24h, scientific), limits, expansion, transformation, and the secondary axis.

## Called from

`R/plot2.R:1969` (primary), `R/plot2.R:1998` (with `add_y_secondary = TRUE`, after building the plot once to obtain primary breaks).

## Contract

- In: `df`, `type`, all `y.*` arguments, `stacked`, `stacked_fill`, `facet.fixed_y`, marks, secondary-axis arguments.
- Out: `scale_y_continuous()` (or `scale_y_discrete()`).
- Side effects: writes `plot2_env$y_lim`, `plot2_env$facet.fixed_y`; messages; the default labels function emits a message at render time (F001).

## Branches

1. `y.zoom`; expansion defaults; `y.limits` length handling.
2. Non-numeric y: error; factor y: discrete scale.
3. Automatic `facet.fixed_y` (coefficient of variation of facet maxima < 25%).
4. Breaks: transformation (waiver), user breaks, age (by 10, max 120), 24h (by 24), `stacked_fill` and percent (by `y.percent_break`, automatic adjustment to about 10 labels), small whole numbers, pretty.
5. Labels: user, scientific, 24h, age, percent, default (automatic scientific for ranges > 1e6 or non-unique labels).
6. Limits: user (0 replaced by data minimum), age, fixed facets (stacked sums), function.
7. Expansion: function, age and `stacked_fill` (none), user with limits.
8. Reverse transform: limits dropped, expansion permuted.
9. Secondary axis with rescaling factor.

## Findings

F022, F024, F041, F042, F043, F044, F052, F063, F003 (24h and age labels depend on the locale), F001 (render-time message).

## End-to-end test cases

- [x] 06.01 Default breaks and labels for 0-100 data.
- [ ] 06.02 Integer data from -3 to 2: negative breaks present (F041). (test written, parts parked: F041)
- [ ] 06.03 `y.percent = TRUE`: labels end in "%", breaks every 10%; with negative values breaks aligned to multiples of the break (F042). (test written, parts parked: F042)
- [x] 06.04 `y.percent_break` adjusted automatically to keep about 10 labels, message.
- [x] 06.05 `stacked_fill = TRUE`: limits 0-1, no expansion, percent labels.
- [x] 06.06 `y.age = TRUE`: breaks by 10, labels "yrs" (language per F003 decision).
- [x] 06.07 `y.24h = TRUE`: breaks by 24.
- [x] 06.08 `y.scientific = TRUE` and automatic scientific labels for range > 1e6 (F063 decision).
- [ ] 06.09 `y.limits = c(0, 10)` with negative data keeps 0 (F024). (test written, parts parked: F024)
- [ ] 06.10 `y.limits` length 1 and length 3. (test written, parts parked: F130)
- [x] 06.11 `y.transform = "log10"` with zeros: no error, zero limit dropped.
- [ ] 06.12 `y.transform` as a transformation object (F043). (test written, parts parked: F043)
- [ ] 06.13 `y.transform = "reverse"` with `y.expand = c(0.1, 0.5)`: expansion swapped correctly (F044). (test written, parts parked: F044)
- [x] 06.14 Facets with similar maxima: fixed y, message; `facet.fixed_y = FALSE` forces free.
- [x] 06.15 Stacked with fixed facets: upper limit equals the largest stacked sum.
- [x] 06.16 All-zero y: limits 0-8 for integers.
- [x] 06.17 `y.position = "right"`.
- [ ] 06.18 Secondary axis: breaks transformed back, labels correct, zero or negative series do not error (F022). (test written, parts parked: F022)
- [ ] 06.19 Character y: clear error; factor y: discrete scale without crash (F026). (test written, parts parked: F026)
- [ ] 06.20 Rendering the plot queues no plot2 messages (F001). (test written, parts parked: F001)

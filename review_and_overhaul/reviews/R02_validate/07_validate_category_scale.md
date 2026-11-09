# 07 validate_category_scale()

`R/validate.R:1305-1556`

## Role

Builds the colour/fill (or size, alpha, ...) scale for a numeric or date category: gradient with one, two, three or more colours, colour bar legend, breaks, labels, limits, transformation, midpoint.

## Called from

`R/plot2.R:1862` (only when the category is numeric or a date).

## Contract

- In: category values, `type`, colours from `validate_colour()`, all `category.*` and `legend.*` arguments, marks, `original_colours`.
- Out: one scale object.
- Side effects: messages (automatic date breaks and labels, midpoint, white added).

## Branches

1. Labels: user, percent, date, default.
2. Breaks: transformation, user, percent, date (`seq.Date`), small whole numbers, pretty.
3. Limits: log with 0 (warning), user, percent (function), date (range +/- 1), heuristic rounding.
4. Aesthetics: colour+fill, fill only, other category types.
5. Original ggplot2 colours: plain continuous scale.
6. One colour (white to colour), two (low-high), three (diverging with midpoint), more (gradientn).

## Findings

F004 (POSIXct), F023 (function limits), F041 (small-integer breaks ignore negatives), F059 (undocumented limit heuristic), F003 (date labels in Dutch). By reading: with `category.type = c("size", "alpha")` and original colours, `paste0("scale_", category.type, "_continuous")` yields two names; currently survives because only the first matters, but it is fragile.

## End-to-end test cases

- [x] 07.01 Numeric category, default colours: continuous colour bar, legend on the right.
- [x] 07.02 One colour: gradient from white, message.
- [x] 07.03 Two colours: low and high.
- [x] 07.04 Three colours: diverging, midpoint at the centre of the limits, message; `category.midpoint` honoured.
- [x] 07.05 Viridis option name: five-colour gradient.
- [x] 07.06 `category.percent = TRUE` (F023). (parked on F023 until it was fixed)
- [x] 07.07 Date category: breaks and labels sensible; POSIXct category likewise (F004). (parked on F004 until it was fixed)
- [x] 07.08 `category.transform = "log10"` with `category.limits` containing 0: warning.
- [x] 07.09 `category.breaks` and `category.labels` honoured.
- [x] 07.10 Integer category from -2 to 3: negative breaks present (F041). (parked on F041 until it was fixed)
- [x] 07.11 `legend.reverse`, `legend.barheight`, `legend.barwidth`, `legend.nbin` reach the guide; top or bottom legend swaps height and width.
- [x] 07.12 `category.type = "size"` with a numeric category.
- [x] 07.13 No deprecation warnings from ggplot2 when building.

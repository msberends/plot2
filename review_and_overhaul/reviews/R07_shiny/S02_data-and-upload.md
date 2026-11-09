# S02 Data sets and upload

## Checked

Data set collection (lines 79 to 134), the selector (436), the observer that loads a data set (807 to 911), the upload tab (316 to 430) and its server logic (624 to 805).

- The data passed via `data` is kept by name when `get(name)` finds anything, including functions on the search path: `data = df` becomes `stats::df` (F114). Otherwise it is stored as `Provided`.
- Names are evaluated with `eval(parse())` (811, 1105, 1230): non-syntactic names crash the session (F114, F115) and any value sent as `input$dataset` runs as code (F006, route 4).
- All data frames of the global environment are offered; on a server that exposes the hosting script's objects (F114).
- `selected` receives a label instead of a value (439; F114).
- Upload in two phases (preview, then commit) works: CSV with delimiter, header, skip, comment character and NA strings (S02.04, S02.05).
- Free-text encoding fails for common spellings; `.txt` with the default delimiter fails (F120).
- `.rds`/`.rda` uploads are deserialised without `trust` (F111); rio warns that this stops working in rio 2.0.0.
- `upload_tab = FALSE` hides the tab with CSS only; the selector item reveals it and the server accepts uploads anyway (F111).
- A missing reader package raises an error in an observer, which ends the session (F115).
- `.xlsx` sheet detection uses `readxl::excel_sheets()` inside `tryCatch`; correct. Upload size is limited by Shiny's default (5 MB); not changed by the app.

## Findings

F111, F114, F115, F120 (new); F006 (extended).

## End-to-end test cases

- [x] S02.01 Data passed to the app is offered as `Provided` and plotted.
- [x] S02.02 (skipped: F114) A global data frame with a non-syntactic name can be selected.
- [x] S02.03 (skipped: F006) A selector value that is not a choice is not evaluated.
- [x] S02.04 CSV is previewed, then used after pressing Upload data.
- [x] S02.05 Comment character, skip and NA strings are applied.
- [x] S02.06 (skipped: F120) The encoding can be given as R spells it.
- [x] S02.07 (skipped: F120) A `.txt` file with the default delimiter can be read.
- [x] S02.08 (skipped: F111) An uploaded R serialisation file is not deserialised.
- [x] S02.09 (skipped: F111) The app can be started without any way to upload.
- [x] S02.10 (skipped: F115) A missing reader package gives a notification and keeps the session.
- [x] S02.11 (skipped: F114) Data passed under a name that is also a function is still the data.

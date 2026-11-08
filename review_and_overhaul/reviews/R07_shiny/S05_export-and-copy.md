# S05 Export and copy button

## Checked

Download handlers (1030 to 1078), the pixel preview (1087 to 1099), the export toggle and the copy button (1025 to 1028).

- PNG, JPG, PDF and SVG export work and respect width, height and DPI (cm units, so text size is independent of DPI, as the code comment explains).
- All exports read `plot2_env$last_shiny_plot`: shared between sessions and cleared by any `plot2()` call (F112).
- Width, height and DPI are not checked on the server; the app's own maxima mean 47,244 x 47,244 pixels (F122). Not executed because of its memory use; ragg's 50,000 pixel limit is the only bound today.
- SVG needs svglite (ggplot2's `ggsave()` requirement); the app does not check it.
- The copy button writes to the clipboard of the R process, which fails on a server and ends the session (F117).

## Findings

F117, F122 (new); F112.

## End-to-end test cases

- [x] S05.01 PNG and JPG export have the size and resolution asked for.
- [x] S05.02 PDF and SVG export write files.
- [x] S05.03 The pixel size of raster exports is shown.
- [x] S05.04 (skipped: F122) An export size beyond a pixel budget is refused. Do not unpark without the fix.
- [x] S05.05 (skipped: F117) The copy button works on a server.

# T01 ggplot2 4.x internals and compatibility

## Checked

- Direct field access on ggplot objects outside the Shiny app: about 45 places. Most frequent: `plot$mapping` (add_type, add_mapping, get_plot_title, restore_mapping), `p$facet$params` edited in place (`R/utils.R:615-624`, `R/add_type.R:202`), `plot$labels` read and written (`R/get_plot_title.R:44`, `R/coord_spider.R:425-430`), `p$theme$panel.background$fill` on an S7 element (`R/validate.R:2371`), `p$layers[[1]]$stat_params` and `$aes_params` (`R/plot2.R:1802`, `1236`), `upper_right@layers` with a `$` fallback (`R/plot2.R:1233`). No `ggplot2:::` calls. `CoordSpider` is a ggproto subclass with a `ggplot_add` method.
- All of this works in ggplot2 4.0.3 through the compatibility layer. **The full test suite passes against ggplot2 4.0.3.9000 (development)**: 329 tests, no failures or warnings.
- Lifecycle: 211 expressions from examples, vignette and README run with `lifecycle_verbosity = "warning"` gave no deprecation warnings outside testthat; inside testthat only the known F075 and F078 appear.
- `ggplot2::get_labs()` returns plot2's titles correctly.

## Findings

No new finding; F037 confirmed in scope and still open (the inventory above is its starting list), F075, F078.

## End-to-end test cases

- [x] T01.01 Main types build and render without warnings.
- [x] T01.02 Titles available through `get_labs()`.
- [x] T01.03 (skipped: F037) No editing of `p$facet$params`.
- [x] T01.04 `coord_spider()` added with `+`.

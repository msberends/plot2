# ===================================================================== #
#  An R package for Fast 'ggplot2' Plotting:                            #
#  https://github.com/msberends/plot2                                   #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

# Small, canonical data sets for tests (see review_and_overhaul/ideas/I001).
# All are deterministic. Keep them small: a test should be readable with the
# fixture in mind. Add new fixtures here rather than inline in test files when
# more than one test needs them.

# numeric x and y
fx_xy <- data.frame(x = c(1, 2, 3, 4, 5),
                    y = c(2, 4, 3, 8, 6))

# character x, three groups of two rows (duplicates are summed by default),
# with a category of two levels
fx_cat <- data.frame(x = c("a", "a", "b", "b", "c", "c"),
                     y = c(1, 2, 3, 4, 5, 6),
                     category = c("u", "v", "u", "v", "u", "v"),
                     stringsAsFactors = FALSE)

# one row per x and category, so no summarising takes place
fx_cat_unique <- data.frame(x = c("a", "b", "c", "a", "b", "c"),
                            y = c(10, 20, 30, 15, 25, 35),
                            category = c("u", "u", "u", "v", "v", "v"),
                            stringsAsFactors = FALSE)

# facets with clearly different maxima (so plot2 does not fix the y scales)
fx_facet <- data.frame(x = rep(c("a", "b", "c"), 2),
                       y = c(1, 2, 3, 100, 200, 300),
                       facet = rep(c("f1", "f2"), each = 3),
                       stringsAsFactors = FALSE)

# facets with similar maxima (plot2 assumes fixed y scales)
fx_facet_similar <- data.frame(x = rep(c("a", "b", "c"), 2),
                               y = c(10, 20, 30, 11, 21, 31),
                               facet = rep(c("f1", "f2"), each = 3),
                               stringsAsFactors = FALSE)

# dates spanning about two months
fx_dates <- data.frame(date = as.Date("2024-01-01") + seq(0, 60, by = 5),
                       y = seq_len(13))

# date-times within one day, explicit time zone
fx_datetimes <- data.frame(time = as.POSIXct("2024-01-01 00:00", tz = "UTC") + 3600 * c(0, 2, 5, 9, 14, 20),
                           y = c(3, 5, 2, 8, 6, 4))

# missing values in every direction
fx_na <- data.frame(x = c("a", "b", NA, "c", "d"),
                    y = c(1, NA, 3, 4, 5),
                    category = c("u", "v", "u", NA, "v"),
                    facet = c("f1", "f1", "f2", "f2", NA),
                    stringsAsFactors = FALSE)

# negative and integer values
fx_neg <- data.frame(x = c("a", "b", "c", "d"),
                     y = c(-3L, -1L, 0L, 2L),
                     stringsAsFactors = FALSE)

# several numeric columns (for y = c(...) and automatic y selection)
fx_wide <- data.frame(group = c("a", "b", "c"),
                      first = c(1, 2, 3),
                      second = c(4, 5, 6),
                      third = c(7, 8, 9),
                      stringsAsFactors = FALSE)

# count data with a column named n (preferred as y by plot2)
fx_counts <- data.frame(group = c("a", "b", "c", "d"),
                        n = c(12L, 7L, 30L, 1L),
                        stringsAsFactors = FALSE)

# awkward but valid names and values
fx_special <- data.frame(`rate (%)` = c(0.1, 0.2, 0.3),
                         `n[1` = c(1, 2, 3),
                         label = c("Children's ward", "Résumé ward", "Ward 3"),
                         check.names = FALSE,
                         stringsAsFactors = FALSE)

# numeric category for continuous colour scales
fx_numcat <- data.frame(x = c(1, 2, 3, 4, 5),
                        y = c(5, 3, 4, 1, 2),
                        value = c(0.1, 0.3, 0.5, 0.7, 0.9))

# percentages (proportions) for y.percent
fx_pct <- data.frame(x = c("a", "b", "c"),
                     y = c(0.12, 0.45, 0.83),
                     stringsAsFactors = FALSE)

# larger data, generated on demand for performance tests
fx_large <- function(n = 1e5, seed = 1) {
  withr::with_seed(seed, data.frame(x = sample(letters, n, replace = TRUE),
                                    y = stats::runif(n),
                                    stringsAsFactors = FALSE))
}

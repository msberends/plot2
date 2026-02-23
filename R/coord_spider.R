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

#' Spider (Radar) Coordinate System for ggplot2
#'
#' @description
#' `coord_spider()` implements a new spider / radar coordinate system using a linear (non-munched) polar-like transform. It is intended for spider charts where connected paths are drawn as *straight chords* between axes (not circular arcs), and where background grid rings are polygons (not circles).
#' Compared to `ggplot2::coord_radial()`, this coordinate system:
#' 
#'  * uses `is_linear() = TRUE` to prevent path "munching" into arcs;
#'  * supports a polygonal background grid (rings + spokes);
#'  * optionally draws axis labels (theta) and radial (r) labels using `grid` grobs in the panel background.
#'
#' Typical usage is to map a discrete variable (categories) to `x` (theta) and a continuous variable (scores) to `y` (radius), then draw a polygon with `geom_polygon()`.
#' 
#' [plot2()] uses this novel coordinate system to plot spider plots, created using `plot2(..., type = "spider")`.
#' @param theta Character scalar specifying which position aesthetic is interpreted as the angular coordinate (theta). Must be `"x"` or `"y"`. In the common spider-chart case, use `theta = "x"` with a discrete `x` scale (categories around the circle) and a continuous `y` scale (radius). Setting `theta = "y"` swaps these roles.
#' @param start Numeric scalar giving the angular offset in **radians** for the first axis. Angles increase according to the `direction` field stored in the coord (this implementation fixes `direction = 1` internally). With `start = 0`, the first axis is at 12 o'clock (positive vertical). Use `start = pi/2` to rotate the first axis to 3 o'clock, etc.
#' @param clip Character scalar passed through to the coordinate system to control clipping of grobs to the panel. Use `"on"` to clip all drawing (including axis labels drawn in `render_bg()`) to the panel region, and `"off"` to allow grobs to extend outside the panel. When `clip = "on"`, labels that lie beyond the panel boundary will be cut off, so in that case it is recommended to set `outer.radius < 1` to reserve headroom inside the panel.
#' @param inner.radius Numeric scalar in \[0, 1) specifying the inner radius of the spider coordinate system as a fraction of the panel radius. Values greater than 0 create a "donut" / hollow center. The data radius is linearly rescaled onto \[`inner.radius`, `outer.radius`\].
#' @param outer.radius Numeric scalar in (0, 1] specifying the maximum radius used for drawing the spider geometry as a fraction of the panel radius. Setting `outer.radius < 1` shrinks the spider grid and data polygon inward, thereby reserving whitespace inside the panel for labels while keeping `clip = "on"`. This is the preferred way to create consistent headroom independent of plot margins and device size.
#' @param axis_labels Logical; if `TRUE`, draw the theta-axis labels (typically the discrete category labels) at the ends of the spokes using `grid::textGrob` in the panel background. Labels are taken from the trained discrete limits of the theta scale.
#' @param axis_label_size Numeric scalar giving the font size (in points) for theta-axis labels drawn when `axis_labels = TRUE`.
#' @param axis_label_pad_mm Numeric scalar giving the outward padding (in mm) applied to theta-axis labels beyond the spoke endpoints. This padding is applied in npc coordinates via `grid::unit()`. Increase this to separate labels from the grid. When `clip = "on"`, excessive padding may push labels outside the panel and cause clipping; prefer reducing padding or decreasing outer.radius` in that case.
#' @param r_labels Logical; if `TRUE`, draw radial (r / y-scale) labels along the "upper" spoke (the spoke at angle `start`, i.e. 12 o'clock when `start = 0`). Breaks and labels are taken from the trained radial scale (typically the `y` scale when `theta = "x"`). This means user-supplied `scale_y_continuous(breaks = ...)` will control both the grid rings and the printed radial labels.
#' @param r_label_size Numeric scalar giving the font size (in points) for radial labels drawn when `r_labels = TRUE`.
#' @param r_label_pad_mm Numeric scalar giving the horizontal padding (in mm) applied to radial labels from the upper spoke. Use positive values to move labels to the right of the spoke. With `clip = "on"`, large padding can cause clipping; consider smaller padding and/or smaller `outer.radius`.
#' @importFrom ggplot2 ggproto
#' @return A `ggproto` object that inherits from [ggplot2::Coord]. This object is used by ggplot2 to transform data coordinates and to render the panel background (grid rings/spokes and optional axis labels).
#' @rdname coord_spider
#' @export
#' @examples
#' # Example data: one observation across five metrics.
#' # 'metric' is the discrete theta axis, 'value' is the radial axis.
#' library(ggplot2)
#'
#' df <- data.frame(
#'   metric = c("Final exam grade",
#'              "Midterm grade",
#'              "Assignments completed",
#'              "Hours studying",
#'              "Classes attended"),
#'   value = c(70, 60, 10, 15, 25))
#' df
#'
#' ggplot(df, aes(metric, value, group = 1)) +
#'     # Spider/Radar plots only require geom_polygon() with coord_spider():
#'     geom_polygon(colour = "red", fill = NA, linewidth = 1) +
#'     coord_spider()
#'     
#' ggplot(df, aes(metric, value, group = 1)) +
#'     geom_polygon(colour = "red", fill = NA, linewidth = 1) +
#'     geom_point(colour = "red", size = 3) +
#'     coord_spider(inner.radius = 0.25,
#'                  r_label_pad_mm = 2) +
#'     scale_y_continuous(limits = c(0, 100),
#'                        breaks = c(0, 25, 50, 75, 100)) +
#'     theme(panel.grid.major.y = element_line(colour = "blue"))
#'     
#' df |>
#'   plot2(type = "spider")
coord_spider <- function(theta = "x",
                         start = 0,
                         clip = "off",
                         inner.radius = 0,
                         outer.radius = 0.8,
                         axis_labels = TRUE,
                         axis_label_size = 9,
                         axis_label_pad_mm = 1,
                         r_labels = TRUE,
                         r_label_size = 8,
                         r_label_pad_mm = 0) {
  theta <- match.arg(theta, c("x", "y"))
  
  stopifnot(is.numeric(outer.radius), length(outer.radius) == 1L,
            outer.radius > 0, outer.radius <= 1)
  
  rlang::check_installed("grid")
  rlang::check_installed("scales")
  
  ggproto(NULL, CoordSpider,
          theta = theta,
          start = start,
          direction = 1,
          clip = clip,
          inner.radius = inner.radius,
          outer.radius = outer.radius,
          axis_labels = axis_labels,
          axis_label_size = axis_label_size,
          axis_label_pad_mm = axis_label_pad_mm,
          r_labels = r_labels,
          r_label_size = r_label_size,
          r_label_pad_mm = r_label_pad_mm
  )
}

#' @rdname coord_spider
#' @format NULL
#' @importFrom ggplot2 ggproto Coord calc_element is_waiver expansion theme_grey element_grob
#' @export
CoordSpider <- ggproto(
  "CoordSpider",
  Coord,
  
  # critical: keep segments straight (avoid coord_munch() arc approximation)
  is_linear = function(self) TRUE,
  
  aspect = function(self, details) 1,
  
  # panel params: cache ranges and discrete axis count
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    
    th_scale <- if (self$theta == "x") scale_x else scale_y
    r_scale  <- if (self$theta == "x") scale_y else scale_x
    
    # --- Respect scale expansion (including waiver) ---
    th_expand <- if (!is_waiver(th_scale$expand)) th_scale$expand else expansion()
    r_expand  <- if (!is_waiver(r_scale$expand))  r_scale$expand  else expansion()
    
    th_range <- th_scale$dimension(expand = th_expand)
    
    # expanded radial range (for headroom)
    r_range_exp <- r_scale$dimension(expand = r_expand)
    
    # trained (no expansion) radial range, used to decide anchoring at 0
    r_range_trained <- r_scale$dimension(expand = c(0, 0))
    
    # Anchor the mapping at 0 when the trained range is non-negative.
    # This prevents a "0 ring" from being drawn away from the centre
    # when expansion makes the lower bound negative.
    r_min_map <- if (is.finite(r_range_trained[1]) && r_range_trained[1] >= 0) 0 else r_range_exp[1]
    r_range_map <- c(r_min_map, r_range_exp[2])
    
    # theta metadata
    th_limits <- tryCatch(th_scale$get_limits(), error = function(e) NULL)
    th_n <- if (!is.null(th_limits)) length(th_limits) else NA_integer_
    
    # ---- radial breaks ----
    # If the user supplied breaks, respect them; otherwise compute from expanded range.
    user_breaks <- r_scale$breaks
    if (!is_waiver(user_breaks)) {
      r_breaks <- tryCatch(r_scale$get_breaks(r_range_map), error = function(e) numeric(0))
      r_breaks <- r_breaks[is.finite(r_breaks)]
    } else {
      r_breaks <- scales::breaks_extended(n = 5)(r_range_exp)
      r_breaks <- r_breaks[is.finite(r_breaks)]
    }
    
    # Keep within the *mapping* range (anchored at 0 if applicable)
    r_breaks <- r_breaks[r_breaks >= min(r_range_map)]
    
    # Radial labels: use scale formatting if available
    r_labels <- tryCatch(r_scale$get_labels(r_breaks), error = function(e) NULL)
    if (is.null(r_labels)) r_labels <- as.character(r_breaks)
    
    list(
      theta_range   = th_range,
      r_range_exp   = r_range_exp,
      r_range_map   = r_range_map,
      theta_n       = th_n,
      theta_limits  = th_limits,
      r_breaks      = r_breaks,
      r_labels      = r_labels,
      start         = self$start,
      direction     = self$direction,
      inner_radius  = self$inner.radius,
      outer_radius  = self$outer.radius
    )
  },
  
  # (theta,r) -> npc x,y in [0,1]
  transform = function(self, data, panel_params) {
    if (is.null(data) || nrow(data) == 0) return(data)
    
    if (self$theta == "x") {
      theta_val <- data$x
      r_val <- data$y
    } else {
      theta_val <- data$y
      r_val <- data$x
    }
    
    if (!is.null(panel_params$theta_limits) &&
        !is.na(panel_params$theta_n) &&
        panel_params$theta_n > 0) {
      
      theta_idx <- suppressWarnings(as.integer(theta_val))
      
      bad <- is.na(theta_idx) & !is.na(theta_val)
      if (any(bad)) {
        theta_idx[bad] <- match(as.character(theta_val[bad]),
                                as.character(panel_params$theta_limits))
      }
      
      grp <- if (!is.null(data$group)) data$group else rep(1L, nrow(data))
      pnl <- if (!is.null(data$PANEL)) data$PANEL else rep(1L, nrow(data))
      
      ord <- order(pnl, grp, theta_idx, na.last = TRUE)
      data <- data[ord, , drop = FALSE]
      
      if (self$theta == "x") {
        theta_val <- data$x
        r_val <- data$y
      } else {
        theta_val <- data$y
        r_val <- data$x
      }
    }
    
    # radius scaling to [inner, outer] using the anchored mapping range
    r_scaled <- scales::rescale(
      r_val,
      from = panel_params$r_range_map,
      to   = c(panel_params$inner_radius, panel_params$outer_radius)
    )
    
    # theta to angles:
    if (!is.na(panel_params$theta_n) && panel_params$theta_n > 0) {
      n <- panel_params$theta_n
      idx <- as.numeric(theta_val)
      angle <- panel_params$start + panel_params$direction * (idx - 1) / n * 2 * pi
    } else {
      th0 <- panel_params$theta_range[1]
      th1 <- panel_params$theta_range[2]
      angle <- panel_params$start + panel_params$direction *
        (theta_val - th0) / (th1 - th0) * 2 * pi
    }
    
    x_cart <- r_scaled * sin(angle)
    y_cart <- r_scaled * cos(angle)
    
    data$x <- (x_cart + 1) / 2
    data$y <- (y_cart + 1) / 2
    data
  },
  
  backtransform_range = function(self, panel_params) {
    list(x = c(0, 1), y = c(0, 1))
  },
  
  range = function(self, panel_params) {
    list(x = c(0, 1), y = c(0, 1))
  },
  
  render_bg = function(self, panel_params, theme) {
    
    # ---- helpers / early exits ----
    th_n <- panel_params$theta_n
    if (is.na(th_n) || th_n <= 1) return(grid::nullGrob())
    
    r_breaks <- panel_params$r_breaks
    if (is.null(r_breaks) || length(r_breaks) == 0) return(grid::nullGrob())
    
    # map helpers (use anchored mapping range)
    r_to_unit <- function(r) scales::rescale(
      r, from = panel_params$r_range_map,
      to = c(panel_params$inner_radius, panel_params$outer_radius)
    )
    angle_k <- function(k) panel_params$start + panel_params$direction * (k - 1) / th_n * 2 * pi
    
    # effective rim radius = mapped max break (fallback to outer_radius)
    outer_eff <- if (length(r_breaks) && is.finite(max(r_breaks))) {
      r_to_unit(max(r_breaks))
    } else {
      panel_params$outer_radius
    }
    
    # ---- panel background from theme, clipped to spider polygon at outer_eff ----
    bg <- calc_element("panel.background", theme)
    if (is.null(bg)) {
      bg <- calc_element("panel.background", theme_grey())
    }
    
    bg_base <- if (inherits(bg, "element_blank")) {
      grid::nullGrob()
    } else {
      element_grob(bg)  # a rect over the whole panel
    }
    
    if (!inherits(bg_base, "null") && th_n > 2) {
      ang <- panel_params$start + panel_params$direction * (seq_len(th_n) - 1) / th_n * 2 * pi
      x_poly <- (outer_eff * sin(ang) + 1) / 2
      y_poly <- (outer_eff * cos(ang) + 1) / 2
      x_poly <- c(x_poly, x_poly[1])
      y_poly <- c(y_poly, y_poly[1])
      
      clip_path <- grid::pathGrob(x = x_poly, y = y_poly, default.units = "npc")
      bg_grob <- grid::grobTree(bg_base, vp = grid::viewport(clip = clip_path))
    } else {
      bg_grob <- bg_base
    }
    
    # ---- grid lines (spider web) ----
    grid_col <- calc_element("panel.grid.major.y", theme)
    if (inherits(grid_col, "element_blank")) {
      # keep background even if grid is blank
      return(bg_grob)
    } else if (is.null(grid_col)) {
      grid_col <- calc_element("panel.grid.major.y", theme_grey())
    }
    
    # spokes: end at outer_eff, not outer_radius
    spoke_x <- (outer_eff * sin(vapply(seq_len(th_n), angle_k, numeric(1))) + 1) / 2
    spoke_y <- (outer_eff * cos(vapply(seq_len(th_n), angle_k, numeric(1))) + 1) / 2
    
    inner <- (panel_params$inner_radius + 1e-12)
    inner_x <- (inner * sin(vapply(seq_len(th_n), angle_k, numeric(1))) + 1) / 2
    inner_y <- (inner * cos(vapply(seq_len(th_n), angle_k, numeric(1))) + 1) / 2
    
    spokes <- grid::segmentsGrob(
      x0 = inner_x, y0 = inner_y,
      x1 = spoke_x, y1 = spoke_y,
      gp = grid::gpar(
        col = grid_col$colour,
        lwd = (grid_col$linewidth %or% 0.5) * 2,
        lty = grid_col$linetype %or% 1
      )
    )
    
    # rings (polygons) at r_breaks
    rings <- lapply(r_breaks, function(rb) {
      rr <- r_to_unit(rb)
      ang <- vapply(seq_len(th_n), angle_k, numeric(1))
      x <- (rr * sin(ang) + 1) / 2
      y <- (rr * cos(ang) + 1) / 2
      x <- c(x, x[1])
      y <- c(y, y[1])
      grid::polylineGrob(
        x = x, y = y,
        gp = grid::gpar(
          col = grid_col$colour,
          lwd = (grid_col$linewidth %or% 0.5) * 2,
          lty = grid_col$linetype %or% 1
        )
      )
    })
    
    # ---- theta labels (metric names) on rim at outer_eff ----
    theta_label_grobs <- list()
    if (isTRUE(self$axis_labels) &&
        !is.null(panel_params$theta_limits) &&
        length(panel_params$theta_limits) == th_n) {
      
      ang <- panel_params$start + panel_params$direction * (seq_len(th_n) - 1) / th_n * 2 * pi
      x0 <- (outer_eff * sin(ang) + 1) / 2
      y0 <- (outer_eff * cos(ang) + 1) / 2
      
      pad <- grid::unit(self$axis_label_pad_mm, "mm")
      dx  <- sign(sin(ang))
      dy  <- sign(cos(ang))
      
      hj <- ifelse(x0 < 0.5, 1, ifelse(x0 > 0.5, 0, 0.5))
      vj <- ifelse(y0 < 0.5, 1, ifelse(y0 > 0.5, 0, 0.5))
      
      extra <- ifelse(abs(sin(ang)) < 1e-8 & cos(ang) > 0, 1, 0)
      
      theta_label_grobs <- lapply(seq_len(th_n), function(i) {
        pad_i <- pad * (1 + extra[i])
        grid::textGrob(
          label = as.character(panel_params$theta_limits[i]),
          x = grid::unit(x0[i], "npc") + pad_i * dx[i],
          y = grid::unit(y0[i], "npc") + pad_i * dy[i],
          just = c(hj[i], vj[i]),
          gp = grid::gpar(fontsize = self$axis_label_size)
        )
      })
    }
    
    # ---- radial (y) labels on upper vertical spoke ----
    r_label_grobs <- list()
    if (isTRUE(self$r_labels) &&
        length(panel_params$r_breaks) > 0) {
      
      rr <- r_to_unit(panel_params$r_breaks)
      
      x_base <- grid::unit(0.5, "npc") + grid::unit(self$r_label_pad_mm, "mm")
      aligned <- ifelse(self$r_label_pad_mm == 0,
                        "center",
                        ifelse(self$r_label_pad_mm > 0, "left", "right"))
      y_pos  <- (rr * cos(panel_params$start) + 1) / 2
      
      r_label_grobs <- lapply(seq_along(rr), function(i) {
        grid::textGrob(
          label = as.character(panel_params$r_labels[i]),
          x = x_base,
          y = grid::unit(y_pos[i], "npc"),
          just = aligned,
          gp = grid::gpar(fontsize = self$r_label_size)
        )
      })
    }
    
    # Compose: background first, then web/labels on top
    children <- c(list(bg_grob, spokes), rings, theta_label_grobs, r_label_grobs)
    do.call(grid::grobTree, children)
  },
  
  render_axis_h = function(self, panel_params, theme) {
    list(top = grid::nullGrob(), bottom = grid::nullGrob())
  },
  render_axis_v = function(self, panel_params, theme) {
    list(left = grid::nullGrob(), right = grid::nullGrob())
  }
)

#' @export
#' @method ggplot_add CoordSpider
#' @importFrom ggplot2 ggplot_add
#' @noRd
ggplot_add.CoordSpider <- function(object, plot, ...) {
  plot$coordinates <- object
  
  # Prevent ggplot2 from regenerating default axis titles at build time
  plot$labels$x <- ""
  plot$labels$y <- ""
  
  # Secondary axes if present
  if (!is.null(plot$labels$x.sec)) plot$labels$x.sec <- ""
  if (!is.null(plot$labels$y.sec)) plot$labels$y.sec <- ""
  
  plot
}

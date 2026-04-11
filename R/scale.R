#' Configure a Scale
#'
#' Add or modify scale settings for a given aesthetic channel. When called
#' immediately after a `mark_*()` function (or after `style_mark()`,
#' `label()`, etc. that target the last mark), the scale is applied to that
#' mark only. Otherwise it is applied at the chart level and affects all marks.
#' This context-sensitivity enables dual-axis charts: pipe `scale_y()` right
#' after each mark to give it its own independent y scale.
#'
#' G2 scale types: `'linear'`, `'ordinal'`, `'band'`, `'point'`, `'time'`,
#' `'log'`, `'pow'`, `'sqrt'`, `'threshold'`, `'quantize'`, `'quantile'`,
#' `'sequential'`, `'identity'`, `'constant'`.
#'
#' @param chart A `g2` object.
#' @param field Character string naming the channel (e.g., `'x'`, `'y'`,
#'   `'color'`).
#' @param ... Scale options passed to G2 (e.g., `type = 'log'`, `nice = TRUE`,
#'   `domain`, `range`, `zero = TRUE`).
#' @return The modified `g2` object.
#' @examples
#' p = g2(mtcars, hp ~ mpg)
#'
#' # Log-scaled x axis
#' p |> scale_x(type = 'log')
#'
#' # Square-root y axis
#' p |> scale_y(type = 'sqrt')
#'
#' # Ordinal color palette
#' g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species) |>
#'   scale_color(palette = 'category10')
scale_ = function(chart = NULL, field, ...) {
  mod = check_chart(scale_, chart, c(if (!missing(field)) list(field), list(...)))
  if (!is.null(mod)) return(mod)
  if (mark_ctx(chart)) {
    chart$layers[[length(chart$layers)]]$scale[[field]] = list(...)
  } else {
    chart$scales[[field]] = list(...)
  }
  chart
}

#' @rdname scale_
#' @export
scale_x = function(chart = NULL, ...) scale_(chart, 'x', ...)

#' @rdname scale_
#' @export
scale_y = function(chart = NULL, ...) scale_(chart, 'y', ...)

#' @rdname scale_
#' @export
scale_color = function(chart = NULL, ...) scale_(chart, 'color', ...)

#' @rdname scale_
#' @export
scale_size = function(chart = NULL, ...) scale_(chart, 'size', ...)

#' @rdname scale_
#' @export
scale_shape = function(chart = NULL, ...) scale_(chart, 'shape', ...)

#' @rdname scale_
#' @export
scale_opacity = function(chart = NULL, ...) scale_(chart, 'opacity', ...)

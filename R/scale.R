#' Configure a Scale
#'
#' Add or modify scale settings for a given aesthetic channel. When called
#' immediately after a `mark_*()` function (or after `style_mark()`,
#' `labels_()`, etc. that target the last mark), the scale is applied to that
#' mark only. Otherwise it is applied at the chart level and affects all marks.
#' This context-sensitivity enables dual-axis charts: add marks and pipe
#' `scale_y(independent = TRUE)` to give one mark its own y scale.
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
#' @export
#' @examples
#' p = g2(mtcars, hp ~ mpg)
#' # Log-scaled x axis (chart-level, before marks)
#' p |> scale_('x', type = 'log')
#'
#' # Square-root scale on y (chart-level)
#' p |> scale_('y', type = 'sqrt')
#'
#' # Ordinal color palette (chart-level)
#' g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species) |>
#'   scale_('color', palette = 'category10')
#'
#' # Mark-level independent y scale for dual-axis charts
#' df = data.frame(x = 1:5, a = c(1, 4, 2, 5, 3), b = c(100, 200, 150, 300, 250))
#' g2(df, ~ x) |>
#'   mark_interval(encode = list(y = 'a')) |>
#'   mark_line(encode = list(y = 'b')) |>
#'   scale_y(independent = TRUE) |>
#'   axis_y(position = 'right', grid = FALSE)
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

#' @details `scale_x()`: Shortcut for `scale_(chart, 'x', ...)`.
#' @rdname scale_
#' @export
#' @examples
#'
#' # Log-scaled x axis
#' p |> scale_x(type = 'log')
scale_x = function(chart = NULL, ...) scale_(chart, 'x', ...)

#' @details `scale_y()`: Shortcut for `scale_(chart, 'y', ...)`.
#' @rdname scale_
#' @export
#' @examples
#'
#' # Square-root y axis
#' p |> scale_y(type = 'sqrt')
scale_y = function(chart = NULL, ...) scale_(chart, 'y', ...)

#' @details `scale_color()`: Shortcut for `scale_(chart, 'color', ...)`.
#' @rdname scale_
#' @export
#' @examples
#'
#' # Ordinal color palette
#' g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species) |>
#'   scale_color(palette = 'category10')
scale_color = function(chart = NULL, ...) scale_(chart, 'color', ...)

#' @details `scale_size()`: Shortcut for `scale_(chart, 'size', ...)`.
#' @rdname scale_
#' @export
#' @examples
#'
#' # Size scale with custom range
#' g2(mtcars, hp ~ mpg, size = ~ wt) |>
#'   scale_size(range = c(2, 10))
scale_size = function(chart = NULL, ...) scale_(chart, 'size', ...)

#' @details `scale_shape()`: Shortcut for `scale_(chart, 'shape', ...)`.
#' @rdname scale_
#' @export
#' @examples
#'
#' # Custom shape range
#' g2(iris, Sepal.Length ~ Sepal.Width, shape = ~ Species) |>
#'   scale_shape(range = c('circle', 'square', 'triangle'))
scale_shape = function(chart = NULL, ...) scale_(chart, 'shape', ...)

#' @details `scale_opacity()`: Shortcut for `scale_(chart, 'opacity', ...)`.
#' @rdname scale_
#' @export
#' @examples
#'
#' # Opacity scale with custom range
#' g2(mtcars, hp ~ mpg, opacity = ~ wt) |>
#'   scale_opacity(range = c(0.2, 1))
scale_opacity = function(chart = NULL, ...) scale_(chart, 'opacity', ...)

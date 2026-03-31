#' Configure a Scale
#'
#' Add or modify scale settings for a given aesthetic channel.
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
#' # Log-scaled x axis
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |>
#'   scale_('x', type = 'log')
#'
#' # Square-root scale on y
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |>
#'   scale_('y', type = 'sqrt')
#'
#' # Ordinal colour palette
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Species') |>
#'   mark_point() |>
#'   scale_('color', palette = 'category10')
scale_ = function(chart, field, ...) {
  chart$scales[[field]] = list(...)
  chart
}

#' Configure the X Scale
#'
#' @inheritParams scale_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |>
#'   scale_x(type = 'log')
scale_x = function(chart, ...) scale_(chart, 'x', ...)

#' Configure the Y Scale
#'
#' @inheritParams scale_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |>
#'   scale_y(type = 'sqrt')
scale_y = function(chart, ...) scale_(chart, 'y', ...)

#' Configure the Color Scale
#'
#' @inheritParams scale_
#' @export
#' @examples
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Species') |>
#'   mark_point() |>
#'   scale_color(palette = 'category10')
scale_color = function(chart, ...) scale_(chart, 'color', ...)

#' Configure the Size Scale
#'
#' @inheritParams scale_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp', size = 'wt') |>
#'   mark_point() |>
#'   scale_size(range = c(2, 10))
scale_size = function(chart, ...) scale_(chart, 'size', ...)

#' Configure the Shape Scale
#'
#' @inheritParams scale_
#' @export
#' @examples
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', shape = 'Species') |>
#'   mark_point() |>
#'   scale_shape(range = c('circle', 'square', 'triangle'))
scale_shape = function(chart, ...) scale_(chart, 'shape', ...)

#' Configure the Opacity Scale
#'
#' @inheritParams scale_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp', opacity = 'wt') |>
#'   mark_point() |>
#'   scale_opacity(range = c(0.2, 1))
scale_opacity = function(chart, ...) scale_(chart, 'opacity', ...)

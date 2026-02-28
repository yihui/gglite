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
#'   scale_of('x', type = 'log')
#'
#' # Square-root scale on y
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |>
#'   scale_of('y', type = 'sqrt')
#'
#' # Ordinal colour palette
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Species') |>
#'   mark_point() |>
#'   scale_of('color', palette = 'category10')
scale_of = function(chart, field, ...) {
  chart$scales[[field]] = list(...)
  chart
}

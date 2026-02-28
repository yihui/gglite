#' Set the Chart Theme
#'
#' G2 built-in themes: `'classic'` (default), `'classicDark'`, `'light'`,
#' `'dark'`, `'academy'`.
#'
#' @param chart A `g2` object.
#' @param type Theme name string, or a list of custom theme options.
#' @param ... Additional theme options merged with the type.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Dark theme
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   theme_of('dark')
#'
#' # Academy theme
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   theme_of('academy')
theme_of = function(chart, type, ...) {
  if (is.character(type)) {
    chart$theme = c(list(type = type), list(...))
  } else {
    chart$theme = c(type, list(...))
  }
  chart
}

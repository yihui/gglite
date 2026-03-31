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

#' Classic Theme
#'
#' Shortcut for `theme_of(chart, 'classic', ...)`. This is the default theme.
#'
#' @inheritParams theme_of
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_classic()
theme_classic = function(chart, ...) theme_of(chart, 'classic', ...)

#' Classic Dark Theme
#'
#' Shortcut for `theme_of(chart, 'classicDark', ...)`.
#'
#' @inheritParams theme_of
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_classicDark()
theme_classicDark = function(chart, ...) theme_of(chart, 'classicDark', ...)

#' Light Theme
#'
#' Shortcut for `theme_of(chart, 'light', ...)`.
#'
#' @inheritParams theme_of
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_light()
theme_light = function(chart, ...) theme_of(chart, 'light', ...)

#' Dark Theme
#'
#' Shortcut for `theme_of(chart, 'dark', ...)`.
#'
#' @inheritParams theme_of
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_dark()
theme_dark = function(chart, ...) theme_of(chart, 'dark', ...)

#' Academy Theme
#'
#' Shortcut for `theme_of(chart, 'academy', ...)`.
#'
#' @inheritParams theme_of
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_academy()
theme_academy = function(chart, ...) theme_of(chart, 'academy', ...)

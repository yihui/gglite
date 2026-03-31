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
#'   theme_('dark')
#'
#' # Academy theme
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   theme_('academy')
theme_ = function(chart, type, ...) {
  if (is.character(type)) {
    chart$theme = c(list(type = type), list(...))
  } else {
    chart$theme = c(type, list(...))
  }
  chart
}

#' Classic Theme
#'
#' Shortcut for `theme_(chart, 'classic', ...)`. This is the default theme.
#'
#' @inheritParams theme_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_classic()
theme_classic = function(chart, ...) theme_(chart, 'classic', ...)

#' Classic Dark Theme
#'
#' Shortcut for `theme_(chart, 'classicDark', ...)`.
#'
#' @inheritParams theme_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_classicDark()
theme_classicDark = function(chart, ...) theme_(chart, 'classicDark', ...)

#' Light Theme
#'
#' Shortcut for `theme_(chart, 'light', ...)`.
#'
#' @inheritParams theme_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_light()
theme_light = function(chart, ...) theme_(chart, 'light', ...)

#' Dark Theme
#'
#' Shortcut for `theme_(chart, 'dark', ...)`.
#'
#' @inheritParams theme_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_dark()
theme_dark = function(chart, ...) theme_(chart, 'dark', ...)

#' Academy Theme
#'
#' Shortcut for `theme_(chart, 'academy', ...)`.
#'
#' @inheritParams theme_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point() |> theme_academy()
theme_academy = function(chart, ...) theme_(chart, 'academy', ...)

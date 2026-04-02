#' Set the Chart Theme
#'
#' G2 built-in themes: `'classic'` (default), `'classicDark'`, `'light'`,
#' `'dark'`, `'academy'`.
#'
#' To set global theme options for all charts, use `options(gglite.theme =
#' list(...))`. This is useful for changing default font sizes, grid line
#' visibility, and other theme properties without modifying each chart
#' individually. For example:
#'
#' ```r
#' options(gglite.theme = list(
#'   title = list(titleFontSize = 20),
#'   axis = list(labelFontSize = 16, gridStrokeOpacity = 0.3),
#'   legendCategory = list(itemLabelFontSize = 14)
#' ))
#' ```
#'
#' Per-chart `theme_()` settings are merged on top of the global option.
#'
#' @param chart A `g2` object.
#' @param type Theme name string, or a list of custom theme options.
#' @param ... Additional theme options merged with the type.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Dark theme
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   theme_('dark')
#'
#' # Academy theme
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   theme_('academy')
theme_ = function(chart = NULL, type, ...) {
  mod = check_chart(theme_, chart, c(if (!missing(type)) list(type), list(...)))
  if (!is.null(mod)) return(mod)
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
#' g2(mtcars, x = 'mpg', y = 'hp') |> theme_classic()
theme_classic = function(chart = NULL, ...) theme_(chart, 'classic', ...)

#' Classic Dark Theme
#'
#' Shortcut for `theme_(chart, 'classicDark', ...)`.
#'
#' @inheritParams theme_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> theme_classicDark()
theme_classicDark = function(chart = NULL, ...) theme_(chart, 'classicDark', ...)

#' Light Theme
#'
#' Shortcut for `theme_(chart, 'light', ...)`.
#'
#' @inheritParams theme_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> theme_light()
theme_light = function(chart = NULL, ...) theme_(chart, 'light', ...)

#' Dark Theme
#'
#' Shortcut for `theme_(chart, 'dark', ...)`.
#'
#' @inheritParams theme_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> theme_dark()
theme_dark = function(chart = NULL, ...) theme_(chart, 'dark', ...)

#' Academy Theme
#'
#' Shortcut for `theme_(chart, 'academy', ...)`.
#'
#' @inheritParams theme_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> theme_academy()
theme_academy = function(chart = NULL, ...) theme_(chart, 'academy', ...)

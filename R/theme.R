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
#' @examples
#' p = g2(mtcars, hp ~ mpg)
#' p |> theme_dark()
#' p |> theme_academy()
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

#' Set the Chart Theme
#'
#' Shortcut functions to apply a built-in G2 theme. Available themes:
#' `theme_classic()` (default), `theme_classic_dark()`, `theme_light()`,
#' `theme_dark()`, `theme_academy()`.
#'
#' @param chart A `g2` object.
#' @param ... Additional theme options.
#' @return The modified `g2` object.
#' @export
#' @examples
#' p = g2(mtcars, hp ~ mpg)
#'
#' # Classic (default) theme
#' p |> theme_classic()
theme_classic = function(chart = NULL, ...) theme_(chart, 'classic', ...)

#' @rdname theme_classic
#' @export
#' @examples
#'
#' # Classic dark theme
#' p |> theme_classic_dark()
theme_classic_dark = function(chart = NULL, ...) theme_(chart, 'classicDark', ...)

#' @rdname theme_classic
#' @export
#' @examples
#'
#' # Light theme
#' p |> theme_light()
theme_light = function(chart = NULL, ...) theme_(chart, 'light', ...)

#' @rdname theme_classic
#' @export
#' @examples
#'
#' # Dark theme
#' p |> theme_dark()
theme_dark = function(chart = NULL, ...) theme_(chart, 'dark', ...)

#' @rdname theme_classic
#' @export
#' @examples
#'
#' # Academy theme
#' p |> theme_academy()
theme_academy = function(chart = NULL, ...) theme_(chart, 'academy', ...)

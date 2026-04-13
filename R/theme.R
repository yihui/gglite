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
#' @param type Theme name string or a list of custom theme options. Use the
#'   specific wrappers (`theme_classic()`, `theme_dark()`, etc.) instead.
#' @param ... Additional theme options merged with the type.
#' @return The modified `g2` object.
#' @examples
#' p = g2(mtcars, hp ~ mpg)
#' p |> theme_classic()
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

#' @rdname theme_
#' @export
theme_classic = function(chart = NULL, ...) theme_(chart, 'classic', ...)

#' @rdname theme_
#' @export
theme_classic_dark = function(chart = NULL, ...) theme_(chart, 'classicDark', ...)

#' @rdname theme_
#' @export
theme_light = function(chart = NULL, ...) theme_(chart, 'light', ...)

#' @rdname theme_
#' @export
theme_dark = function(chart = NULL, ...) theme_(chart, 'dark', ...)

#' @rdname theme_
#' @export
theme_academy = function(chart = NULL, ...) theme_(chart, 'academy', ...)

#' Style Plot Region Backgrounds
#'
#' Set background fill colors for the different spatial regions of a G2 chart.
#' This is especially useful for visualizing how `margin`, `padding`, and
#' `inset` affect the layout (see [canvas()]).
#'
#' The chart area is organized as nested layers:
#'
#' \describe{
#'   \item{`margin`}{Outermost area — space outside the chart container. Set as
#'     a CSS `background-color` on the container `<div>`.}
#'   \item{`padding`}{Inside the container but outside the plot area — where
#'     axis tick labels are drawn. Set via `theme.view.viewFill`.}
#'   \item{`plot`}{The plot area where data marks are rendered. Set via
#'     `theme.view.plotFill`.}
#'   \item{`inset`}{Inside the plot area, surrounding the innermost data region.
#'     Set via `theme.view.mainFill`.}
#'   \item{`content`}{The innermost data region (inside `inset`). Set via
#'     `theme.view.contentFill`.}
#' }
#'
#' @param chart A `g2` object.
#' @param margin CSS background color for the margin area (outside the chart).
#' @param padding Background fill for the padding area (between chart edge and
#'   plot area, where axis labels live).
#' @param plot Background fill for the plot area (where data is rendered).
#' @param inset Background fill for the main area inside the inset spacing.
#' @param content Background fill for the innermost content area.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Visualize the three layout regions with distinct colors
#' g2(mtcars, hp ~ mpg) |>
#'   canvas(margin = 20, padding = 60, inset = 20) |>
#'   style_view(margin = '#fff3bf', padding = '#ffc9c9', plot = '#a5d8ff')
style_view = function(
  chart = NULL, margin = NULL, padding = NULL,
  plot = NULL, inset = NULL, content = NULL
) {
  args = list(
    margin = margin, padding = padding,
    plot = plot, inset = inset, content = content
  )
  mod = check_chart(style_view, chart, args)
  if (!is.null(mod)) return(mod)
  if (!is.null(margin)) chart$bg = margin
  fills = dropNulls(list(
    viewFill = padding, plotFill = plot,
    mainFill = inset, contentFill = content
  ))
  if (length(fills)) {
    cur_view = as.list(chart$theme$view)
    chart$theme = modifyList(
      as.list(chart$theme),
      list(view = modifyList(cur_view, fills))
    )
  }
  chart
}

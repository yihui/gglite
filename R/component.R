#' Configure an Axis
#'
#' Customise the axis for a positional channel (`'x'` or `'y'`). Set to
#' `FALSE` to hide the axis.
#'
#' @param chart A `g2` object.
#' @param channel Positional channel: `'x'` or `'y'`.
#' @param ... Axis options such as `title`, `labelFormatter`, `tickCount`,
#'   `grid`, etc., or `FALSE` to hide.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   axis_of('x', title = 'Miles per Gallon') |>
#'   axis_of('y', title = 'Horsepower')
axis_of = function(chart, channel, ...) {
  args = list(...)
  if (length(args) == 1 && is.logical(args[[1]])) {
    chart$axes[[channel]] = args[[1]]
  } else {
    chart$axes[[channel]] = args
  }
  chart
}

#' Configure a Legend
#'
#' Customise the legend for a visual channel (`'color'`, `'size'`, `'shape'`,
#' `'opacity'`). Set to `FALSE` to hide.
#'
#' @param chart A `g2` object.
#' @param channel Visual channel name.
#' @param ... Legend options such as `position` (`'top'`, `'bottom'`, `'left'`,
#'   `'right'`), `layout`, `title`, etc.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Species') |>
#'   mark_point() |>
#'   legend_of('color', position = 'right')
legend_of = function(chart, channel, ...) {
  args = list(...)
  if (length(args) == 1 && is.logical(args[[1]])) {
    chart$legends[[channel]] = args[[1]]
  } else {
    chart$legends[[channel]] = args
  }
  chart
}

#' Set the Chart Title
#'
#' @param chart A `g2` object.
#' @param text Title text string.
#' @param ... Additional title options such as `subtitle`, `align`, `style`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   title_of('Motor Trend Cars', subtitle = 'mpg vs hp')
title_of = function(chart, text, ...) {
  dots = list(...)
  if (length(dots)) {
    chart$chart_title = c(list(title = text), dots)
  } else {
    chart$chart_title = text
  }
  chart
}

#' Configure the Tooltip
#'
#' Set chart-level tooltip options. Mark-level tooltips can be passed via `...`
#' in `mark_*()` functions.
#'
#' @param chart A `g2` object.
#' @param ... Tooltip options such as `shared`, `crosshairs`, `marker`, or
#'   `FALSE` to disable.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   tooltip_of(crosshairs = TRUE)
tooltip_of = function(chart, ...) {
  args = list(...)
  if (length(args) == 1 && is.logical(args[[1]])) {
    chart$tooltip_config = args[[1]]
  } else {
    chart$tooltip_config = args
  }
  chart
}

#' Add Labels to the Last Mark
#'
#' Append a label configuration to the most recently added mark. Can be called
#' multiple times to add several label layers.
#'
#' @param chart A `g2` object.
#' @param ... Label options such as `text` (channel name), `position`,
#'   `formatter`, `style`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
#' g2(df, x = 'x', y = 'y') |>
#'   mark_interval() |>
#'   labels_of(text = 'y', position = 'inside')
labels_of = function(chart, ...) {
  n = length(chart$layers)
  if (n == 0) stop('add a mark before setting labels')
  chart$layers[[n]]$labels = c(chart$layers[[n]]$labels, list(list(...)))
  chart
}

#' Set Style on the Last Mark
#'
#' @param chart A `g2` object.
#' @param ... Style options such as `fill`, `stroke`, `lineWidth`,
#'   `fillOpacity`, `strokeOpacity`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   style_mark(fill = 'steelblue', stroke = 'white', lineWidth = 1)
style_mark = function(chart, ...) {
  n = length(chart$layers)
  if (n == 0) stop('add a mark before setting style')
  chart$layers[[n]]$style = list(...)
  chart
}

#' Add a Slider
#'
#' Add a range slider to a positional channel for zooming/panning.
#'
#' @param chart A `g2` object.
#' @param channel Positional channel: `'x'` or `'y'`.
#' @param ... Slider options.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   slider_of('x')
slider_of = function(chart, channel, ...) {
  if (is.null(chart$sliders)) chart$sliders = list()
  args = list(...)
  chart$sliders[[channel]] = if (length(args)) args else TRUE
  chart
}

#' Add a Scrollbar
#'
#' @param chart A `g2` object.
#' @param channel Positional channel: `'x'` or `'y'`.
#' @param ... Scrollbar options.
#' @return The modified `g2` object.
#' @export
#' @examples
#' df = data.frame(x = 1:100, y = cumsum(rnorm(100)))
#' g2(df, x = 'x', y = 'y') |>
#'   mark_line() |>
#'   scrollbar_of('x')
scrollbar_of = function(chart, channel, ...) {
  if (is.null(chart$scrollbars)) chart$scrollbars = list()
  args = list(...)
  chart$scrollbars[[channel]] = if (length(args)) args else TRUE
  chart
}

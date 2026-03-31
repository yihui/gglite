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
#'   axis_('x', title = 'Miles per Gallon') |>
#'   axis_('y', title = 'Horsepower')
axis_ = function(chart, channel, ...) {
  args = list(...)
  if (length(args) == 1 && is.logical(args[[1]])) {
    chart$axes[[channel]] = args[[1]]
  } else {
    chart$axes[[channel]] = args
  }
  chart
}

#' Configure the X Axis
#'
#' @inheritParams axis_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   axis_x(title = 'Miles per Gallon')
axis_x = function(chart, ...) axis_(chart, 'x', ...)

#' Configure the Y Axis
#'
#' @inheritParams axis_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   axis_y(title = 'Horsepower')
axis_y = function(chart, ...) axis_(chart, 'y', ...)

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
#'   legend_('color', position = 'right')
legend_ = function(chart, channel, ...) {
  args = list(...)
  if (length(args) == 1 && is.logical(args[[1]])) {
    chart$legends[[channel]] = args[[1]]
  } else {
    chart$legends[[channel]] = args
  }
  chart
}

#' Configure the Color Legend
#'
#' @inheritParams legend_
#' @export
#' @examples
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Species') |>
#'   mark_point() |>
#'   legend_color(position = 'right')
legend_color = function(chart, ...) legend_(chart, 'color', ...)

#' Configure the Size Legend
#'
#' @inheritParams legend_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp', size = 'wt') |>
#'   mark_point() |>
#'   legend_size(position = 'bottom')
legend_size = function(chart, ...) legend_(chart, 'size', ...)

#' Configure the Shape Legend
#'
#' @inheritParams legend_
#' @export
#' @examples
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', shape = 'Species') |>
#'   mark_point() |>
#'   legend_shape(position = 'bottom')
legend_shape = function(chart, ...) legend_(chart, 'shape', ...)

#' Configure the Opacity Legend
#'
#' @inheritParams legend_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp', opacity = 'wt') |>
#'   mark_point() |>
#'   legend_opacity(position = 'bottom')
legend_opacity = function(chart, ...) legend_(chart, 'opacity', ...)

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
#'   title_('Motor Trend Cars', subtitle = 'mpg vs hp')
title_ = function(chart, text, ...) {
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
#'   tooltip_(crosshairs = TRUE)
tooltip_ = function(chart, ...) {
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
#'   labels_(text = 'y', position = 'inside')
labels_ = function(chart, ...) {
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
#'   slider_('x')
slider_ = function(chart, channel, ...) {
  if (is.null(chart$sliders)) chart$sliders = list()
  args = list(...)
  chart$sliders[[channel]] = if (length(args)) args else TRUE
  chart
}

#' Add an X Slider
#'
#' @inheritParams slider_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   slider_x()
slider_x = function(chart, ...) slider_(chart, 'x', ...)

#' Add a Y Slider
#'
#' @inheritParams slider_
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   slider_y()
slider_y = function(chart, ...) slider_(chart, 'y', ...)

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
#'   scrollbar_('x')
scrollbar_ = function(chart, channel, ...) {
  if (is.null(chart$scrollbars)) chart$scrollbars = list()
  args = list(...)
  chart$scrollbars[[channel]] = if (length(args)) args else TRUE
  chart
}

#' Add an X Scrollbar
#'
#' @inheritParams scrollbar_
#' @export
#' @examples
#' df = data.frame(x = 1:100, y = cumsum(rnorm(100)))
#' g2(df, x = 'x', y = 'y') |>
#'   mark_line() |>
#'   scrollbar_x()
scrollbar_x = function(chart, ...) scrollbar_(chart, 'x', ...)

#' Add a Y Scrollbar
#'
#' @inheritParams scrollbar_
#' @export
#' @examples
#' df = data.frame(x = 1:100, y = cumsum(rnorm(100)))
#' g2(df, x = 'x', y = 'y') |>
#'   mark_line() |>
#'   scrollbar_y()
scrollbar_y = function(chart, ...) scrollbar_(chart, 'y', ...)

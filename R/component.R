#' Configure an Axis
#'
#' Customise the axis for a positional channel (`'x'` or `'y'`). Set to
#' `FALSE` to hide the axis. When called immediately after a `mark_*()`
#' function (or `style_mark()`, `labels_()`, etc.), the axis is applied to
#' that mark only, enabling per-mark axis customization (e.g., a right-side
#' y-axis for a dual-axis chart). Otherwise it applies at the chart level.
#'
#' @param chart A `g2` object.
#' @param channel Positional channel: `'x'` or `'y'`.
#' @param ... Axis options such as `title`, `labelFormatter`, `tickCount`,
#'   `grid`, `position`, etc., or `FALSE` to hide.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Chart-level axis titles (no marks yet)
#' g2(mtcars, hp ~ mpg) |>
#'   axis_('x', title = 'Miles per Gallon') |>
#'   axis_('y', title = 'Horsepower')
#'
#' # Mark-level axis for dual-axis chart
#' df = data.frame(x = 1:5, a = c(1, 4, 2, 5, 3), b = c(100, 200, 150, 300, 250))
#' g2(df, ~ x) |>
#'   mark_interval(encode = list(y = 'a')) |>
#'   mark_line(encode = list(y = 'b')) |>
#'   scale_y(independent = TRUE) |>
#'   axis_y(position = 'right', grid = FALSE)
axis_ = function(chart = NULL, channel, ...) {
  mod = check_chart(axis_, chart, c(if (!missing(channel)) list(channel), list(...)))
  if (!is.null(mod)) return(mod)
  args = list(...)
  is_hide = length(args) == 1 && is.logical(args[[1]])
  val = if (is_hide) args[[1]] else args
  if (mark_ctx(chart)) {
    chart$layers[[length(chart$layers)]]$axis[[channel]] = val
  } else {
    chart$axes[[channel]] = val
  }
  chart
}

#' @details `axis_x()`: Shortcut for `axis_(chart, 'x', ...)`.
#' @rdname axis_
#' @export
axis_x = function(chart = NULL, ...) axis_(chart, 'x', ...)

#' @details `axis_y()`: Shortcut for `axis_(chart, 'y', ...)`.
#' @rdname axis_
#' @export
axis_y = function(chart = NULL, ...) axis_(chart, 'y', ...)

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
#' p = g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species)
#' p |> legend_('color', position = 'right')
legend_ = function(chart = NULL, channel, ...) {
  mod = check_chart(legend_, chart, c(if (!missing(channel)) list(channel), list(...)))
  if (!is.null(mod)) return(mod)
  args = list(...)
  if (length(args) == 1 && is.logical(args[[1]])) {
    chart$legends[[channel]] = args[[1]]
  } else {
    # G2's default theme hides legend title (title: false). When the user
    # provides a title string, translate it into the underlying component
    # props so the title becomes visible.
    if (is.character(args$title)) {
      args$titleText = args$title
      args$title = NULL
      args$showTitle = TRUE
    }
    chart$legends[[channel]] = args
  }
  chart
}

#' @details `legend_color()`: Shortcut for `legend_(chart, 'color', ...)`.
#' @rdname legend_
#' @export
#' @examples
#'
#' # Color legend via shortcut
#' p |> legend_color(position = 'right')
legend_color = function(chart = NULL, ...) legend_(chart, 'color', ...)

#' @details `legend_size()`: Shortcut for `legend_(chart, 'size', ...)`.
#' @rdname legend_
#' @export
#' @examples
#'
#' # Size legend
#' g2(mtcars, hp ~ mpg, size = ~ wt) |>
#'   legend_size(position = 'bottom')
legend_size = function(chart = NULL, ...) legend_(chart, 'size', ...)

#' @details `legend_shape()`: Shortcut for `legend_(chart, 'shape', ...)`.
#' @rdname legend_
#' @export
#' @examples
#'
#' # Shape legend
#' p = g2(iris, Sepal.Length ~ Sepal.Width, shape = ~ Species)
#' p |> legend_shape(position = 'bottom')
legend_shape = function(chart = NULL, ...) legend_(chart, 'shape', ...)

#' @details `legend_opacity()`: Shortcut for `legend_(chart, 'opacity', ...)`.
#' @rdname legend_
#' @export
#' @examples
#'
#' # Opacity legend
#' g2(mtcars, hp ~ mpg, opacity = ~ wt) |>
#'   legend_opacity(position = 'bottom')
legend_opacity = function(chart = NULL, ...) legend_(chart, 'opacity', ...)

#' Set the Chart Title
#'
#' @param chart A `g2` object.
#' @param text Title text string.
#' @param ... Additional title options such as `subtitle`, `align`, `style`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars, hp ~ mpg) |>
#'   title_('Motor Trend Cars', subtitle = 'mpg vs hp')
title_ = function(chart = NULL, text, ...) {
  mod = check_chart(title_, chart, c(if (!missing(text)) list(text), list(...)))
  if (!is.null(mod)) return(mod)
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
#' g2(mtcars, hp ~ mpg) |>
#'   tooltip_(crosshairs = TRUE)
tooltip_ = function(chart = NULL, ...) {
  mod = check_chart(tooltip_, chart, list(...))
  if (!is.null(mod)) return(mod)
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
#' @param ... Label options such as `text` (channel name as `~col` or
#'   `'col'`), `position`, `formatter`, `style`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
#' g2(df, y ~ x) |>
#'   labels_(text = ~ y, position = 'inside')
labels_ = function(chart = NULL, ...) {
  mod = check_chart(labels_, chart, list(...))
  if (!is.null(mod)) return(mod)
  was_empty = !length(chart$layers)
  if (was_empty) chart = ensure_mark(chart)
  n = if (was_empty) 1L else length(chart$layers)
  chart$layers[[n]]$labels = c(chart$layers[[n]]$labels, list(as_vars(list(...))))
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
#' g2(mtcars, hp ~ mpg) |>
#'   style_mark(fill = 'steelblue', stroke = 'white', lineWidth = 1)
style_mark = function(chart = NULL, ...) {
  mod = check_chart(style_mark, chart, list(...))
  if (!is.null(mod)) return(mod)
  was_empty = !length(chart$layers)
  if (was_empty) chart = ensure_mark(chart)
  n = if (was_empty) 1L else length(chart$layers)
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
#' p = g2(mtcars, hp ~ mpg)
#' p |> slider_('x')
slider_ = function(chart = NULL, channel, ...) {
  mod = check_chart(slider_, chart, c(if (!missing(channel)) list(channel), list(...)))
  if (!is.null(mod)) return(mod)
  if (is.null(chart$sliders)) chart$sliders = list()
  args = list(...)
  chart$sliders[[channel]] = if (length(args)) args else TRUE
  chart
}

#' @details `slider_x()`: Shortcut for `slider_(chart, 'x', ...)`.
#' @rdname slider_
#' @export
#' @examples
#'
#' # Slider shortcuts
#' p |> slider_x()
slider_x = function(chart = NULL, ...) slider_(chart, 'x', ...)

#' @details `slider_y()`: Shortcut for `slider_(chart, 'y', ...)`.
#' @rdname slider_
#' @export
#' @examples
#' p |> slider_y()
slider_y = function(chart = NULL, ...) slider_(chart, 'y', ...)

#' Add a Scrollbar
#'
#' Add a scrollbar to a positional channel for zooming/panning.
#'
#' @param chart A `g2` object.
#' @param channel Positional channel: `'x'` or `'y'`.
#' @param ... Scrollbar options.
#' @return The modified `g2` object.
#' @export
#' @examples
#' df = data.frame(x = 1:100, y = cumsum(rnorm(100)))
#' p = g2(df, y ~ x) |> mark_line()
#' p |> scrollbar_('x')
scrollbar_ = function(chart = NULL, channel, ...) {
  mod = check_chart(scrollbar_, chart, c(if (!missing(channel)) list(channel), list(...)))
  if (!is.null(mod)) return(mod)
  if (is.null(chart$scrollbars)) chart$scrollbars = list()
  args = list(...)
  chart$scrollbars[[channel]] = if (length(args)) args else TRUE
  chart
}

#' @details `scrollbar_x()`: Shortcut for `scrollbar_(chart, 'x', ...)`.
#' @rdname scrollbar_
#' @export
#' @examples
#'
#' # Scrollbar shortcuts
#' p |> scrollbar_x()
scrollbar_x = function(chart = NULL, ...) scrollbar_(chart, 'x', ...)

#' @details `scrollbar_y()`: Shortcut for `scrollbar_(chart, 'y', ...)`.
#' @rdname scrollbar_
#' @export
#' @examples
#' p |> scrollbar_y()
scrollbar_y = function(chart = NULL, ...) scrollbar_(chart, 'y', ...)

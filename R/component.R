#' Configure an Axis
#'
#' Customise the axis for a positional channel (`'x'` or `'y'`). Set to
#' `FALSE` to hide the axis. When called immediately after a `mark_*()`
#' function (or `style_mark()`, `label()`, etc.), the axis is applied to
#' that mark only, enabling per-mark axis customization for dual-axis charts.
#' Otherwise it applies at the chart level.
#'
#' @param chart A `g2` object.
#' @param channel Positional channel: `'x'` or `'y'`.
#' @param ... Axis options such as `title`, `labelFormatter`, `tickCount`,
#'   `grid`, `position`, etc., or `FALSE` to hide.
#' @return The modified `g2` object.
#' @examples
#' # Chart-level axis titles (no marks yet)
#' g2(mtcars, hp ~ mpg) |>
#'   axis_x(title = 'Miles per Gallon') |>
#'   axis_y(title = 'Horsepower')
#'
#' # Dual-axis chart: each mark gets its own axis immediately after mark_*()
#' air = aggregate(cbind(Temp, Wind) ~ Month, data = airquality, FUN = mean)
#' air$Month = month.abb[air$Month]
#' g2(air, x = 'Month') |>
#'   mark_interval(encode = list(y = 'Temp')) |>
#'   scale_y(independent = TRUE) |>
#'   axis_y(title = 'Temperature (°F)') |>
#'   mark_line(encode = list(y = 'Wind')) |>
#'   scale_y(independent = TRUE) |>
#'   axis_y(position = 'right', grid = FALSE, title = 'Wind Speed (mph)')
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

#' Configure an Axis
#'
#' Shortcut functions for `axis_(chart, 'x', ...)` and
#' `axis_(chart, 'y', ...)`. Customise the axis for a positional channel.
#' Set `...` to `FALSE` to hide the axis. When called immediately after a
#' `mark_*()` function (or `style_mark()`, `label()`, etc.), the axis is
#' applied to that mark only, enabling per-mark axis customization for
#' dual-axis charts. Otherwise it applies at the chart level.
#'
#' @param chart A `g2` object.
#' @param ... Axis options such as `title`, `labelFormatter`, `tickCount`,
#'   `grid`, `position`, etc., or `FALSE` to hide.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Chart-level axis titles
#' g2(mtcars, hp ~ mpg) |>
#'   axis_x(title = 'Miles per Gallon') |>
#'   axis_y(title = 'Horsepower')
#'
#' # Dual-axis chart: each mark gets its own axis immediately after mark_*()
#' air = aggregate(cbind(Temp, Wind) ~ Month, data = airquality, FUN = mean)
#' air$Month = month.abb[air$Month]
#' g2(air, x = 'Month') |>
#'   mark_interval(encode = list(y = 'Temp')) |>
#'   scale_y(independent = TRUE) |>
#'   axis_y(title = 'Temperature (°F)') |>
#'   mark_line(encode = list(y = 'Wind')) |>
#'   scale_y(independent = TRUE) |>
#'   axis_y(position = 'right', grid = FALSE, title = 'Wind Speed (mph)')
axis_x = function(chart = NULL, ...) axis_(chart, 'x', ...)

#' @rdname axis_x
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
#' @examples
#' p = g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species)
#' p |> legend_color(position = 'right')
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

#' Configure a Legend
#'
#' Shortcut functions to customise the legend for a visual channel. Set
#' `...` to `FALSE` to hide the legend.
#'
#' @param chart A `g2` object.
#' @param ... Legend options such as `position` (`'top'`, `'bottom'`, `'left'`,
#'   `'right'`), `layout`, `title`, etc., or `FALSE` to hide.
#' @return The modified `g2` object.
#' @export
#' @examples
#' p = g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species)
#'
#' # Color legend via shortcut
#' p |> legend_color(position = 'right')
legend_color = function(chart = NULL, ...) legend_(chart, 'color', ...)

#' @rdname legend_color
#' @export
#' @examples
#'
#' # Size legend
#' g2(mtcars, hp ~ mpg, size = ~ wt) |>
#'   legend_size(position = 'bottom')
legend_size = function(chart = NULL, ...) legend_(chart, 'size', ...)

#' @rdname legend_color
#' @export
#' @examples
#'
#' # Shape legend
#' p = g2(iris, Sepal.Length ~ Sepal.Width, shape = ~ Species)
#' p |> legend_shape(position = 'bottom')
legend_shape = function(chart = NULL, ...) legend_(chart, 'shape', ...)

#' @rdname legend_color
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
#' Configure tooltip interaction behavior. All options are applied to
#' `interaction.tooltip` in the G2 spec: pass `FALSE` to disable the tooltip,
#' or pass named options such as `crosshairs`, `shared`, `marker`, and any
#' `crosshairs*`/`marker*` style properties. To configure the data displayed in
#' a tooltip for a specific mark (e.g., `channel`, `valueFormatter`, `items`),
#' pass a `tooltip` list argument directly to the mark function instead, e.g.,
#' `mark_line(tooltip = list(channel = 'y', valueFormatter = '.0%'))`.
#'
#' @param chart A `g2` object.
#' @param ... Tooltip interaction options such as `shared`, `crosshairs`,
#'   `marker`, `series`, `crosshairsStroke`, or `FALSE` to disable the tooltip.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Enable crosshairs (works best with line/area marks which use series tooltip)
#' df = data.frame(x = 1:6, y = c(3, 1, 4, 1, 5, 2))
#' g2(df, y ~ x) |>
#'   mark_line() |>
#'   tooltip(crosshairs = TRUE)
#'
#' # Shared tooltip for multi-series line chart
#' df2 = data.frame(
#'   x = rep(1:5, 2), y = c(3, 1, 4, 1, 5, 2, 7, 1, 8, 3),
#'   group = rep(c('A', 'B'), each = 5)
#' )
#' g2(df2, y ~ x, color = ~ group) |>
#'   mark_line() |>
#'   tooltip(shared = TRUE)
#'
#' # Disable tooltip
#' g2(mtcars, hp ~ mpg) |>
#'   tooltip(FALSE)
tooltip = function(chart = NULL, ...) {
  mod = check_chart(tooltip, chart, list(...))
  if (!is.null(mod)) return(mod)
  args = list(...)
  if (length(args) == 1 && is.null(names(args)) && is.logical(args[[1]])) {
    chart$interactions[['tooltip']] = args[[1]]
    return(chart)
  }
  cur = if (is.list(chart$interactions[['tooltip']])) chart$interactions[['tooltip']] else list()
  chart$interactions[['tooltip']] = modifyList(cur, args)
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
#'   label(text = ~ y, position = 'inside')
label = function(chart = NULL, ...) {
  mod = check_chart(label, chart, list(...))
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
#' @examples
#' p = g2(mtcars, hp ~ mpg)
#' p |> slider_x()
slider_ = function(chart = NULL, channel, ...) {
  mod = check_chart(slider_, chart, c(if (!missing(channel)) list(channel), list(...)))
  if (!is.null(mod)) return(mod)
  if (is.null(chart$sliders)) chart$sliders = list()
  args = list(...)
  chart$sliders[[channel]] = if (length(args)) args else TRUE
  chart
}

#' Add a Slider
#'
#' Add a range slider to a positional channel for zooming/panning.
#'
#' @param chart A `g2` object.
#' @param ... Slider options.
#' @return The modified `g2` object.
#' @export
#' @examples
#' p = g2(mtcars, hp ~ mpg)
#' p |> slider_x() |> slider_y()
slider_x = function(chart = NULL, ...) slider_(chart, 'x', ...)

#' @rdname slider_x
#' @export
slider_y = function(chart = NULL, ...) slider_(chart, 'y', ...)

#' Add a Scrollbar
#'
#' Add a scrollbar to a positional channel for zooming/panning.
#'
#' @param chart A `g2` object.
#' @param channel Positional channel: `'x'` or `'y'`.
#' @param ... Scrollbar options.
#' @return The modified `g2` object.
#' @examples
#' df = data.frame(x = 1:100, y = cumsum(rnorm(100)))
#' p = g2(df, y ~ x) |> mark_line()
#' p |> scroll_x()
scroll_ = function(chart = NULL, channel, ...) {
  mod = check_chart(scroll_, chart, c(if (!missing(channel)) list(channel), list(...)))
  if (!is.null(mod)) return(mod)
  if (is.null(chart$scrollbars)) chart$scrollbars = list()
  args = list(...)
  chart$scrollbars[[channel]] = if (length(args)) args else TRUE
  chart
}

#' Add a Scrollbar
#'
#' Add a scrollbar to a positional channel for zooming/panning.
#'
#' @param chart A `g2` object.
#' @param ... Scrollbar options.
#' @return The modified `g2` object.
#' @export
#' @examples
#' df = data.frame(x = 1:100, y = cumsum(rnorm(100)))
#' p = g2(df, y ~ x) |> mark_line()
#' p |> scroll_x() |> scroll_y()
scroll_x = function(chart = NULL, ...) scroll_(chart, 'x', ...)

#' @rdname scroll_x
#' @export
scroll_y = function(chart = NULL, ...) scroll_(chart, 'y', ...)

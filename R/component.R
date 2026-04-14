#' Configure an Axis
#'
#' Customize the axis for a positional channel (`'x'` or `'y'`). Set to
#' `FALSE` to hide the axis. When called immediately after a `mark_*()`
#' function (or `style_mark()`, `labels()`, etc.), the axis is applied to
#' that mark only, enabling per-mark axis customization for dual-axis charts.
#' Otherwise it applies at the chart level.
#'
#' @param chart A `g2` object.
#' @param channel Positional channel: `'x'` or `'y'`.
#' @param ... Axis options such as `title`, `labelFormatter`, `tickCount`,
#'   `grid`, `position`, etc., or `FALSE` to hide.
#' @return The modified `g2` object.
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

#' @rdname axis_
#' @export
axis_x = function(chart = NULL, ...) axis_(chart, 'x', ...)

#' @rdname axis_
#' @export
axis_y = function(chart = NULL, ...) axis_(chart, 'y', ...)

#' Configure a Legend
#'
#' Customize the legend for a visual channel (`'color'`, `'size'`, `'shape'`,
#' `'opacity'`). Set to `FALSE` to hide.
#'
#' @param chart A `g2` object.
#' @param channel Visual channel name.
#' @param ... Legend options such as `position` (`'top'`, `'bottom'`, `'left'`,
#'   `'right'`), `layout`, `title`, etc., or `FALSE` to hide.
#' @return The modified `g2` object.
#' @examples
#' p = g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species)
#' p |> legend_color(position = 'right')
#'
#' g2(mtcars, hp ~ mpg, size = ~ wt) |>
#'   legend_size(position = 'bottom')
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

#' @rdname legend_
#' @export
legend_color = function(chart = NULL, ...) legend_(chart, 'color', ...)

#' @rdname legend_
#' @export
legend_size = function(chart = NULL, ...) legend_(chart, 'size', ...)

#' @rdname legend_
#' @export
legend_shape = function(chart = NULL, ...) legend_(chart, 'shape', ...)

#' @rdname legend_
#' @export
legend_opacity = function(chart = NULL, ...) legend_(chart, 'opacity', ...)

#' Set the Chart Title
#'
#' Set the chart title and subtitle, as well as their styles.
#'
#' When called as `title('string')` with an active graphics device that already
#' has a plot drawn (i.e., `dev.cur() > 1` and `!par('page')`), the call is
#' dispatched to [graphics::title()].  Otherwise a deferred modifier is
#' created so it can be used in a `|>` pipeline.
#'
#' **Note for `+` users:** `g2_chart + title('string')` is ambiguous when a
#' graphics device is active.  Use the named argument explicitly to guarantee
#' gglite behavior: `g2_chart + title(main = 'string')`.  With `|>` there is
#' no ambiguity because the chart object is passed as the first argument.
#'
#' @param chart A `g2` object passed via `|>`, or `NULL` when using `+`.
#' @param main Title text string.
#' @param ... Additional title options such as `subtitle`, `align`, `style`,
#'   or arguments forwarded to [graphics::title()].
#' @return The modified `g2` object, or the result of [graphics::title()].
#' @export
#' @examples
#' g2(mtcars, hp ~ mpg) |>
#'   title('Motor Trend Cars', subtitle = 'mpg vs hp')
#' @importFrom grDevices dev.cur
#' @importFrom graphics par
title = function(chart = NULL, main, ...) {
  if (is.character(chart) && dev.cur() > 1L && !par('page'))
    return(graphics::title(main = chart, ...))
  mod = check_chart(title, chart, c(if (!missing(main)) list(main), list(...)))
  if (!is.null(mod)) return(mod)
  dots = list(...)
  if (length(dots)) {
    chart$chart_title = c(list(title = main), dots)
  } else {
    chart$chart_title = main
  }
  chart
}

#' Add Labels to the Last Mark
#'
#' Append a label configuration to the most recently added mark. Can be called
#' multiple times to add several label layers. When the first argument is not a
#' `g2` object or `NULL`, the call is dispatched to [base::labels()].
#'
#' @param chart A `g2` object, `NULL` (for deferred use with `+`), or any
#'   object to be passed to [base::labels()].
#' @param ... Label options such as `text` (channel name as `~col` or
#'   `'col'`), `position`, `formatter`, `style`, or arguments passed to
#'   [base::labels()].
#' @return The modified `g2` object, or the result of [base::labels()].
#' @export
#' @examples
#' df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
#' g2(df, y ~ x) |>
#'   labels(text = ~ y, position = 'inside')
labels = function(chart = NULL, ...) {
  if (not_g2(chart)) return(base::labels(chart, ...))
  mod = check_chart(labels, chart, list(...))
  if (!is.null(mod)) return(mod)
  was_empty = !length(chart$layers)
  if (was_empty) chart = ensure_mark(chart)
  n = if (was_empty) 1L else length(chart$layers)
  chart$layers[[n]]$labels = c(chart$layers[[n]]$labels, list(as_vars(list(...))))
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
#'   `marker`, `series`, `crosshairsStroke`, `crosshairsLineWidth`,
#'   `crosshairsStrokeOpacity`, or `FALSE` to disable the tooltip.
#'   Series marks (`mark_line()`, `mark_area()`) show a vertical crosshair by
#'   default (`crosshairsY = TRUE`); pass `crosshairs = FALSE` to suppress it.
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
#' p |> slider_x() |> slider_y()
slider_ = function(chart = NULL, channel, ...) {
  mod = check_chart(slider_, chart, c(if (!missing(channel)) list(channel), list(...)))
  if (!is.null(mod)) return(mod)
  if (is.null(chart$sliders)) chart$sliders = list()
  args = list(...)
  chart$sliders[[channel]] = if (length(args)) args else TRUE
  chart
}

#' @rdname slider_
#' @export
slider_x = function(chart = NULL, ...) slider_(chart, 'x', ...)

#' @rdname slider_
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
#' p |> scroll_x() |> scroll_y()
scroll_ = function(chart = NULL, channel, ...) {
  mod = check_chart(scroll_, chart, c(if (!missing(channel)) list(channel), list(...)))
  if (!is.null(mod)) return(mod)
  if (is.null(chart$scrollbars)) chart$scrollbars = list()
  args = list(...)
  chart$scrollbars[[channel]] = if (length(args)) args else TRUE
  chart
}

#' @rdname scroll_
#' @export
scroll_x = function(chart = NULL, ...) scroll_(chart, 'x', ...)

#' @rdname scroll_
#' @export
scroll_y = function(chart = NULL, ...) scroll_(chart, 'y', ...)

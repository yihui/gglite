#' Set the Coordinate System
#'
#' Specify the coordinate system for the chart. G2 supports these coordinate
#' types: `'cartesian'` (default), `'polar'`, `'theta'`, `'radial'`, `'radar'`,
#' `'helix'`, `'parallel'`. Use the `transform` argument to apply coordinate
#' transforms such as `'transpose'` (equivalent to ggplot2's `coord_flip()`) or
#' `'fisheye'`.
#'
#' The `'parallel'` coordinate requires a `position` encoding (a character
#' vector of column names) instead of separate `x`/`y` encodings. For radar
#' charts, use the `'polar'` coordinate with long-format data (x/y/color).
#'
#' @param chart A `g2` object.
#' @param type Coordinate type string.
#' @param ... Additional options such as `innerRadius`, `outerRadius`,
#'   `startAngle`, `endAngle`, or `transform`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
#' p = g2(df, y ~ x, color = ~ x)
#' # Polar coordinate (rose chart)
#' g2(df, y ~ x) |> coord_polar()
#'
#' # Theta coordinate (pie / donut chart)
#' p |> transform('stackY') |> coord_theta()
#' p |> transform('stackY') |> coord_theta(innerRadius = 0.5)
#'
#' # Radial coordinate (radial bar chart)
#' p |> coord_radial()
#'
#' # Parallel coordinate (uses position encoding)
#' g2(iris, position = names(iris)[-5], color = ~ Species,
#'   padding = c(30, NA, NA, NA)) |>
#'   coord_parallel() |>
#'   legend_color(position = 'bottom')
#'
#' # Radar coordinate (polar with long-format data)
#' df2 = data.frame(
#'   item = rep(c('Design', 'Dev', 'Marketing', 'Sales', 'Support'), 2),
#'   score = c(80, 90, 65, 75, 85, 60, 70, 85, 80, 70),
#'   team = rep(c('A', 'B'), each = 5)
#' )
#' g2(df2, score ~ item, color = ~ team) |>
#'   mark_area(style = list(fillOpacity = 0.5)) |>
#'   mark_line(style = list(lineWidth = 2)) |>
#'   coord_polar() |>
#'   scale_x(padding = 0.5, align = 0) |>
#'   scale_y(domainMin = 0, domainMax = 100) |>
#'   axis_x(grid = TRUE)
coord_ = function(chart = NULL, type, ...) {
  mod = check_chart(coord_, chart, c(if (!missing(type)) list(type), list(...)))
  if (!is.null(mod)) return(mod)
  chart$coords = c(list(type = type), list(...))
  chart$last_op = NULL
  chart
}

#' Transpose (Flip) the Coordinate System
#'
#' Swap x and y axes, equivalent to ggplot2's `coord_flip()`. This adds a
#' `transpose` transform to the current coordinate system.
#'
#' @param chart A `g2` object.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Horizontal bar chart (coord_flip equivalent)
#' df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
#' g2(df, y ~ x) |>
#'   coord_transpose()
coord_transpose = function(chart = NULL) {
  mod = check_chart(coord_transpose, chart, list())
  if (!is.null(mod)) return(mod)
  if (is.null(chart$coords)) chart$coords = list()
  chart$coords$transform = c(
    chart$coords$transform, list(list(type = 'transpose'))
  )
  chart$last_op = NULL
  chart
}

#' @details `coord_polar()`: Shortcut for `coord_(chart, 'polar', ...)`.
#' @rdname coord_
#' @export
coord_polar = function(chart = NULL, ...) coord_(chart, 'polar', ...)

#' @details `coord_theta()`: Shortcut for `coord_(chart, 'theta', ...)`. Used for
#'   pie and donut charts.
#' @rdname coord_
#' @export
coord_theta = function(chart = NULL, ...) coord_(chart, 'theta', ...)

#' @details `coord_radial()`: Shortcut for `coord_(chart, 'radial', ...)`. Suitable
#'   for radial bar charts.
#' @rdname coord_
#' @export
coord_radial = function(chart = NULL, ...) coord_(chart, 'radial', ...)

#' @details `coord_radar()`: Shortcut for `coord_(chart, 'radar', ...)`. Used with
#'   `position` encoding for radar (spider) charts.
#' @rdname coord_
#' @export
coord_radar = function(chart = NULL, ...) coord_(chart, 'radar', ...)

#' @details `coord_helix()`: Shortcut for `coord_(chart, 'helix', ...)`.
#' @rdname coord_
#' @export
coord_helix = function(chart = NULL, ...) coord_(chart, 'helix', ...)

#' @details `coord_parallel()`: Shortcut for `coord_(chart, 'parallel', ...)`. Used
#'   with `position` encoding for parallel coordinate plots.
#' @rdname coord_
#' @export
coord_parallel = function(chart = NULL, ...) coord_(chart, 'parallel', ...)

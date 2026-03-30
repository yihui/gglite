#' Set the Coordinate System
#'
#' Specify the coordinate system for the chart. G2 supports these coordinate
#' types: `'cartesian'` (default), `'polar'`, `'theta'`, `'radial'`, `'radar'`,
#' `'helix'`, `'parallel'`. Use the `transform` argument to apply coordinate
#' transforms such as `'transpose'` (equivalent to ggplot2's `coord_flip()`) or
#' `'fisheye'`.
#'
#' The `'radar'` and `'parallel'` coordinates require a `position` encoding
#' (a character vector of column names) instead of separate `x`/`y` encodings.
#'
#' @param chart A `g2` object.
#' @param type Coordinate type string.
#' @param ... Additional options such as `innerRadius`, `outerRadius`,
#'   `startAngle`, `endAngle`, or `transform`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Polar coordinate (rose chart)
#' df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
#' g2(df, x = 'x', y = 'y') |>
#'   mark_interval() |>
#'   coordinate('polar')
#'
#' # Theta coordinate (pie / donut chart)
#' g2(df, x = 'x', y = 'y', color = 'x') |>
#'   mark_interval() |>
#'   transform_of('stackY') |>
#'   coordinate('theta')
#'
#' # Radial coordinate (radial bar chart)
#' g2(df, x = 'x', y = 'y', color = 'x') |>
#'   mark_interval() |>
#'   coordinate('radial')
#'
#' # Parallel coordinate (uses position encoding)
#' g2(iris, position = c('Sepal.Length', 'Sepal.Width',
#'     'Petal.Length', 'Petal.Width'), color = 'Species') |>
#'   mark_line() |>
#'   coordinate('parallel') |>
#'   legend_of('color', position = 'bottom')
#'
#' # Radar coordinate (uses position encoding)
#' df2 = data.frame(
#'   item = rep(c('A', 'B', 'C', 'D', 'E'), 2),
#'   score = c(70, 90, 60, 80, 75, 85, 65, 80, 70, 90),
#'   team = rep(c('X', 'Y'), each = 5)
#' )
#' g2(df2, position = c('item', 'score'), color = 'team') |>
#'   mark_line(style = list(closed = TRUE)) |>
#'   mark_point() |>
#'   coordinate('radar')
coordinate = function(chart, type, ...) {
  chart$coords = c(list(type = type), list(...))
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
#' g2(data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2)), x = 'x', y = 'y') |>
#'   mark_interval() |>
#'   coord_transpose()
coord_transpose = function(chart) {
  if (is.null(chart$coords)) chart$coords = list()
  chart$coords$transform = c(
    chart$coords$transform, list(list(type = 'transpose'))
  )
  chart
}

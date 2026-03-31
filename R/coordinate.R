#' Set the Coordinate System
#'
#' Specify the coordinate system for the chart. G2 supports these coordinate
#' types: `'cartesian'` (default), `'polar'`, `'theta'`, `'radial'`, `'radar'`,
#' `'helix'`, `'parallel'`. Use the `transform` argument to apply coordinate
#' transforms such as `'transpose'` (equivalent to ggplot2's `coord_flip()`) or
#' `'fisheye'`.
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
#' # Parallel coordinate
#' g2(iris, position = c('Sepal.Length', 'Sepal.Width',
#'     'Petal.Length', 'Petal.Width'), color = 'Species') |>
#'   mark_line(transform = list(list(type = 'normalizeY'))) |>
#'   coordinate('parallel')
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

#' Polar Coordinate System
#'
#' Shortcut for `coordinate(chart, 'polar', ...)`.
#'
#' @inheritParams coordinate
#' @export
#' @examples
#' g2(data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2)),
#'   x = 'x', y = 'y', color = 'x') |>
#'   mark_interval() |> coord_polar()
coord_polar = function(chart, ...) coordinate(chart, 'polar', ...)

#' Theta Coordinate System
#'
#' Shortcut for `coordinate(chart, 'theta', ...)`. Used for pie and donut
#' charts.
#'
#' @inheritParams coordinate
#' @export
#' @examples
#' g2(data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2)),
#'   x = 'x', y = 'y', color = 'x') |>
#'   mark_interval() |> transform_of('stackY') |>
#'   coord_theta(innerRadius = 0.5)
coord_theta = function(chart, ...) coordinate(chart, 'theta', ...)

#' Radial Coordinate System
#'
#' Shortcut for `coordinate(chart, 'radial', ...)`. Suitable for radial bar
#' charts.
#'
#' @inheritParams coordinate
#' @export
#' @examples
#' g2(data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2)),
#'   x = 'x', y = 'y', color = 'x') |>
#'   mark_interval() |> coord_radial()
coord_radial = function(chart, ...) coordinate(chart, 'radial', ...)

#' Radar Coordinate System
#'
#' Shortcut for `coordinate(chart, 'radar', ...)`. Used with `position`
#' encoding for radar (spider) charts.
#'
#' @inheritParams coordinate
#' @export
coord_radar = function(chart, ...) coordinate(chart, 'radar', ...)

#' Helix Coordinate System
#'
#' Shortcut for `coordinate(chart, 'helix', ...)`.
#'
#' @inheritParams coordinate
#' @export
coord_helix = function(chart, ...) coordinate(chart, 'helix', ...)

#' Parallel Coordinate System
#'
#' Shortcut for `coordinate(chart, 'parallel', ...)`. Used with `position`
#' encoding for parallel coordinate plots.
#'
#' @inheritParams coordinate
#' @export
coord_parallel = function(chart, ...) coordinate(chart, 'parallel', ...)

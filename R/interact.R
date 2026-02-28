#' Add an Interaction
#'
#' Enable an interaction behaviour on the chart. G2 interaction types include:
#' `'tooltip'`, `'elementHighlight'`, `'elementHighlightByX'`,
#' `'elementHighlightByColor'`, `'elementSelect'`, `'elementSelectByX'`,
#' `'elementSelectByColor'`, `'elementHoverScale'`, `'fisheye'`,
#' `'chartIndex'`, `'legendFilter'`, `'legendHighlight'`,
#' `'brushHighlight'`, `'brushXHighlight'`, `'brushYHighlight'`,
#' `'brushFilter'`, `'brushXFilter'`, `'brushYFilter'`,
#' `'sliderFilter'`, `'poptip'`, `'drillDown'`.
#'
#' @param chart A `g2` object.
#' @param type Interaction type string.
#' @param ... Additional interaction options.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Tooltip on scatter plot
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   interact('tooltip')
#'
#' # Highlight elements on hover
#' g2(mtcars, x = 'mpg', y = 'hp', color = 'cyl') |>
#'   mark_point() |>
#'   interact('elementHighlight')
#'
#' # Brush to highlight a region
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   interact('brushHighlight')
#'
#' # Legend filter
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Species') |>
#'   mark_point() |>
#'   interact('legendFilter')
interact = function(chart, type, ...) {
  entry = c(list(type = type), list(...))
  chart$interactions = c(chart$interactions, list(entry))
  chart
}

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
#' g2(mtcars, hp ~ mpg) |>
#'   interact('tooltip')
#'
#' # Highlight elements on hover
#' g2(mtcars, hp ~ mpg, color = ~ cyl) |>
#'   interact('elementHighlight')
#'
#' # Brush to highlight a region
#' g2(mtcars, hp ~ mpg) |>
#'   interact('brushHighlight')
#'
#' # Legend filter
#' g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species) |>
#'   interact('legendFilter')
interact = function(chart = NULL, type, ...) {
  mod = check_chart(interact, chart, c(if (!missing(type)) list(type), list(...)))
  if (!is.null(mod)) return(mod)
  args = list(...)
  chart$interactions[[type]] = if (length(args)) args else TRUE
  chart
}

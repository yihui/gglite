#' Facet by a Rectangular Grid
#'
#' Split the chart into a grid of panels based on one or two variables,
#' similar to ggplot2's `facet_grid()` / `facet_wrap()`. Use the `x` and/or
#' `y` arguments to specify faceting variables.
#'
#' @param chart A `g2` object.
#' @param ... Facet encoding and options. Pass `x = 'var'` and/or `y = 'var'`
#'   to specify the faceting variable(s).
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length') |>
#'   mark_point() |>
#'   facet_rect(x = 'Species')
facet_rect = function(chart, ...) {
  chart$facet = list(type = 'facetRect')
  enc = list(...)
  if (length(enc)) chart$facet$encode = enc
  chart
}

#' Facet in a Circular Layout
#'
#' @param chart A `g2` object.
#' @param ... Facet encoding and options. Pass `position = 'var'` to specify
#'   the faceting variable.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length') |>
#'   mark_point() |>
#'   facet_circle(position = 'Species')
facet_circle = function(chart, ...) {
  chart$facet = list(type = 'facetCircle')
  enc = list(...)
  if (length(enc)) chart$facet$encode = enc
  chart
}

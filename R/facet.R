#' Facet by a Rectangular Grid
#'
#' Split the chart into a grid of panels based on one or two variables,
#' similar to ggplot2's `facet_grid()` / `facet_wrap()`. Use the `x` and/or
#' `y` arguments to specify faceting variables. An unnamed first formula
#' argument is interpreted as: `~var` sets `x = 'var'`; `y ~ x` sets both
#' `x = 'x'` and `y = 'y'`.
#'
#' @param chart A `g2` object.
#' @param ... Facet encoding and options. Pass `x = ~var` and/or `y = ~var`
#'   to specify the faceting variable(s). An unnamed formula `~var` sets
#'   `x = 'var'`; `y ~ x` sets `x = 'x'` and `y = 'y'`. Character strings
#'   are also accepted.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(iris, Sepal.Length ~ Sepal.Width) |>
#'   facet_rect(x = ~ Species)
#'
#' # Unnamed formula shorthand
#' g2(iris, Sepal.Length ~ Sepal.Width) |>
#'   facet_rect(~ Species)
#'
#' # Two-sided formula: y ~ x
#' df = data.frame(
#'   x = rnorm(200), y = rnorm(200),
#'   sex = sample(c('M', 'F'), 200, replace = TRUE),
#'   species = sample(c('A', 'B'), 200, replace = TRUE)
#' )
#' g2(df, y ~ x) |>
#'   facet_rect(sex ~ species)
facet_rect = function(chart = NULL, ...) {
  dots = list(...)
  # Handle unnamed first formula argument
  if (length(dots) &&
      (is.null(names(dots)) || !nzchar(names(dots)[1])) &&
      inherits(dots[[1]], 'formula')) {
    f = dots[[1]]
    dots = dots[-1]
    if (length(f) == 3) {
      # y ~ x: x = rhs, y = lhs
      dots = c(list(x = as.character(f[[3]]), y = as.character(f[[2]])), dots)
    } else {
      # ~ x: x = rhs
      dots = c(list(x = as.character(f[[2]])), dots)
    }
  }
  mod = check_chart(facet_rect, chart, dots)
  if (!is.null(mod)) return(mod)
  chart$facet = list(type = 'facetRect')
  enc = as_vars(dots)
  if (length(enc)) chart$facet$encode = enc
  chart
}

#' Facet in a Circular Layout
#'
#' @param chart A `g2` object.
#' @param ... Facet encoding and options. Pass `position = ~var` to specify
#'   the faceting variable. Character strings are also accepted.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(iris, Sepal.Length ~ Sepal.Width) |>
#'   facet_circle(position = ~ Species)
facet_circle = function(chart = NULL, ...) {
  mod = check_chart(facet_circle, chart, list(...))
  if (!is.null(mod)) return(mod)
  chart$facet = list(type = 'facetCircle')
  enc = as_vars(list(...))
  if (length(enc)) chart$facet$encode = enc
  chart
}

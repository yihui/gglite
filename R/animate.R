#' Set Animation Options
#'
#' Configure animation for the most recently added mark, or the
#' automatically-inferred mark when none has been added explicitly. G2 supports
#' `enter`, `update`, and `exit` animations.
#'
#' @param chart A `g2` object.
#' @param ... Animation configuration as named lists. Use `enter`, `update`,
#'   and `exit` to control each phase. Set to `FALSE` to disable animation
#'   entirely.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Fade-in animation on bars
#' g2(data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2)), y ~ x) |>
#'   animate(enter = list(type = 'fadeIn', duration = 1000))
#'
#' # Wave-in animation
#' g2(data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2)), y ~ x) |>
#'   animate(enter = list(type = 'waveIn', duration = 800))
#'
#' # Disable animation
#' g2(mtcars, hp ~ mpg) |>
#'   animate(FALSE)
animate = function(chart = NULL, ...) {
  mod = check_chart(animate, chart, list(...))
  if (!is.null(mod)) return(mod)
  was_empty = !length(chart$layers)
  if (was_empty) chart = ensure_mark(chart)
  n = if (was_empty) 1L else length(chart$layers)
  args = list(...)
  if (length(args) == 1 && is.logical(args[[1]])) {
    chart$layers[[n]]$animate = args[[1]]
  } else {
    chart$layers[[n]]$animate = args
  }
  chart
}

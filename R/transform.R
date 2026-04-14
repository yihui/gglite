#' Add a Data Transform to the Last Mark
#'
#' Append a data transform to the most recently added mark. G2 transforms
#' correspond roughly to ggplot2's `stat_*()` and `position_*()` functions.
#' When the first argument is not a `g2` object or a transform type string,
#' the call is dispatched to [base::transform()].
#'
#' Common transforms: `'stackY'` (stack, like `position_stack()`), `'dodgeX'`
#' (dodge, like `position_dodge()`), `'normalizeY'` (normalize to 100%, like
#' `position_fill()`), `'jitterX'` / `'jitterY'` (jitter, like
#' `position_jitter()`), `'bin'` / `'binX'` (bin data, like `stat_bin()`),
#' `'groupX'` / `'groupY'` / `'groupColor'` (group and aggregate, like
#' `stat_summary()`), `'sortX'` / `'sortY'` / `'sortColor'` (sort data),
#' `'symmetryY'` (mirror, for pyramid / funnel charts), `'diffY'` (difference,
#' for waterfall charts), `'select'` / `'selectX'` / `'selectY'` (filter),
#' `'sample'` (down-sample), `'pack'` (circle-packing layout), `'flexX'`
#' (flexible x spacing, Marimekko charts).
#'
#' @param chart A `g2` object, a transform type string (for deferred use with
#'   `+`), or any object to be passed to [base::transform()].
#' @param type Transform type string.
#' @param ... Additional transform options, or expressions passed to
#'   [base::transform()] when `chart` is a data frame.
#' @return The modified `g2` object, or the result of [base::transform()].
#' @export
#' @examples
#' # Stacked bar chart
#' df = data.frame(
#'   x = rep(c('A', 'B'), each = 2), y = c(3, 2, 5, 4),
#'   color = rep(c('a', 'b'), 2)
#' )
#' g2(df, y ~ x, color = ~ color) |>
#'   mark_interval() |>
#'   transform('stackY')
#'
#' # Grouped (dodged) bar chart
#' g2(df, y ~ x, color = ~ color) |>
#'   mark_interval() |>
#'   transform('dodgeX')
#'
#' # Percent stacked bar (normalizeY + stackY)
#' g2(df, y ~ x, color = ~ color) |>
#'   mark_interval() |>
#'   transform('stackY') |>
#'   transform('normalizeY')
#'
#' # Jitter on a scatter plot
#' g2(mtcars, hp ~ cyl) |>
#'   transform('jitterX')
#'
#' # Histogram using binX
#' g2(mtcars, ~ mpg) |>
#'   mark_interval(encode = list(y = 'count')) |>
#'   transform('binX', thresholds = 15)
#'
#' # Base R dispatch: add a computed column to a data frame
#' transform(mtcars, kpl = mpg * 0.4251)
transform = function(chart = NULL, type, ...) {
  if (not_g2(chart) && !is.character(chart))
    return(base::transform(chart, ...))
  mod = check_chart(transform, chart, c(if (!missing(type)) list(type), list(...)))
  if (!is.null(mod)) return(mod)
  was_empty = !length(chart$layers)
  if (was_empty) chart = ensure_mark(chart)
  n = if (was_empty) 1L else length(chart$layers)
  t = c(list(type = type), list(...))
  chart$layers[[n]]$transform = c(chart$layers[[n]]$transform, list(t))
  chart
}

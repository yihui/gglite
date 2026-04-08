#' Add a Data Transform to the Last Mark
#'
#' Append a data transform to the most recently added mark. G2 transforms
#' correspond roughly to ggplot2's `stat_*()` and `position_*()` functions.
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
#' @param chart A `g2` object.
#' @param type Transform type string.
#' @param ... Additional transform options.
#' @return The modified `g2` object.
#' @export
#' @examples
#' # Stacked bar chart
#' df = data.frame(
#'   x = rep(c('A', 'B'), each = 2), y = c(3, 2, 5, 4),
#'   color = rep(c('a', 'b'), 2)
#' )
#' g2(df, y ~ x, color = ~ color) |>
#'   mark_interval() |>
#'   transform_('stackY')
#'
#' # Grouped (dodged) bar chart
#' g2(df, y ~ x, color = ~ color) |>
#'   mark_interval() |>
#'   transform_('dodgeX')
#'
#' # Percent stacked bar (normalizeY + stackY)
#' g2(df, y ~ x, color = ~ color) |>
#'   mark_interval() |>
#'   transform_('stackY') |>
#'   transform_('normalizeY')
#'
#' # Jitter on a scatter plot
#' g2(mtcars, hp ~ cyl) |>
#'   transform_('jitterX')
#'
#' # Histogram using binX
#' g2(mtcars, ~ mpg) |>
#'   mark_interval(encode = list(y = 'count')) |>
#'   transform_('binX', thresholds = 15)
transform_ = function(chart = NULL, type, ...) {
  mod = check_chart(transform_, chart, c(if (!missing(type)) list(type), list(...)))
  if (!is.null(mod)) return(mod)
  was_empty = !length(chart$layers)
  if (was_empty) chart = ensure_mark(chart)
  n = if (was_empty) 1L else length(chart$layers)
  t = c(list(type = type), list(...))
  chart$layers[[n]]$transform = c(chart$layers[[n]]$transform, list(t))
  chart
}

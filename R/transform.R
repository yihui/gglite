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
#' This is an S3 method on [base::transform()], so `chart |> transform(type,
#' ...)` dispatches here automatically. For the `+` deferred-modifier syntax
#' use `transform.g2(...)` directly, e.g. `chart + transform.g2('stackY')`.
#' @param chart A `g2` object.
#' @param type Transform type string.
#' @param ... Additional transform options.
#' @return The modified `g2` object.
#' @export
#' @rawNamespace export(transform.g2)
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
transform.g2 = function(chart, type, ...) {
  mod = check_chart(transform.g2, chart, c(if (!missing(type)) list(type), list(...)))
  if (!is.null(mod)) return(mod)
  was_empty = !length(chart$layers)
  if (was_empty) chart = ensure_mark(chart)
  n = if (was_empty) 1L else length(chart$layers)
  t = c(list(type = type), list(...))
  chart$layers[[n]]$transform = c(chart$layers[[n]]$transform, list(t))
  chart
}


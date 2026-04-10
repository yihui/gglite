#' gglite: Lightweight Data Visualization via the Grammar of Graphics
#'
#' An R interface to the AntV G2 JavaScript visualization library.
#'
#' @name gglite-package
#' @keywords internal
"_PACKAGE"

#' CDN URLs for the G2 Library
#'
#' Returns CDN URLs for loading the G2 JavaScript library. When the chart uses
#' a non-default renderer or the `gglite.renderer` global option is set, the
#' lite bundle plus renderer packages are returned instead of `g2.min.js`.
#' The default `g2.min.js` URL is customizable via the `gglite.g2_cdn` option.
#'
#' @param chart A `g2` object, or `NULL` for the default (Canvas) CDN.
#' @return A character vector of CDN URLs.
#' @noRd
g2_cdn = function(chart = NULL) {
  if (is.null(chart) || !needs_lite(chart))
    return(c(
      getOption('gglite.g2_cdn', 'https://unpkg.com/@antv/g2@5/dist/g2.min.js'),
      g2_patches_cdn
    ))
  r = effective_renderer(chart)
  r_url = switch(r,
    svg    = 'https://unpkg.com/@antv/g-svg',
    webgl  = 'https://unpkg.com/@antv/g-webgl',
    canvas = 'https://unpkg.com/@antv/g-canvas'
  )
  c(
    'https://unpkg.com/@antv/g',
    r_url,
    'https://unpkg.com/@antv/g2@5/dist/g2.lite.min.js',
    g2_patches_cdn
  )
}

g2_patches_cdn = 'https://cdn.jsdelivr.net/npm/@xiee/utils@v1.14.32/js/g2-patches.min.js'

#' @importFrom xfun js
#' @export
xfun::js

# Returns TRUE when scale_/axis_ should target the last mark rather than the
# chart: any time the user just added a mark (last_op == 'mark'), regardless
# of how many marks exist. This means scale/axis always stay with their mark,
# enabling correct dual-axis charts even when only one mark has been added.
mark_ctx = function(chart) isTRUE(chart$last_op == 'mark')

#' Create a Deferred Chart Modifier
#'
#' Wrap a modifier function and its arguments into a closure that can be applied
#' later via the `+` operator. This enables ggplot2-style syntax like
#' `g2(data) + mark_point() + theme_('dark')`.
#'
#' @param fn The modifier function to defer.
#' @param args A list of arguments (excluding `chart`) to pass when applied.
#' @return A function of class `g2_mod`.
#' @noRd
g2_mod = function(fn, args = list()) {
  f = function(chart) do.call(fn, c(list(chart), args))
  class(f) = 'g2_mod'
  f
}

#' Check Chart and Defer if Needed
#'
#' If `chart` is a `g2` object, return `NULL` so the caller proceeds normally.
#' If `chart` is a `g2_mod` (from a piped modifier), compose the two modifiers
#' so that mixing `|>` and `+` operators works correctly. Otherwise, capture
#' the arguments into a `g2_mod()` closure for later application via `+`.
#'
#' @param fn The modifier function to defer to.
#' @param chart The `chart` argument from the modifier.
#' @param args A list of remaining arguments (excluding `chart`).
#' @return `NULL` if `chart` is a `g2` object, or a `g2_mod` closure.
#' @noRd
check_chart = function(fn, chart, args) {
  if (inherits(chart, 'g2')) return()
  if (inherits(chart, 'g2_mod')) {
    prev = chart
    this = g2_mod(fn, args)
    f = function(c) this(prev(c))
    class(f) = 'g2_mod'
    return(f)
  }
  g2_mod(fn, c(if (!is.null(chart)) list(chart), args))
}

#' Add a Modifier to a G2 Chart
#'
#' Enables ggplot2-style `+` syntax for building charts. The right-hand side
#' must be a deferred modifier created by calling a modifier function without
#' a chart argument (e.g., `mark_point()`, `theme_('dark')`).
#'
#' @param e1 A `g2` object (the chart).
#' @param e2 A `g2_mod` object (a deferred modifier).
#' @return The modified `g2` object.
#' @export
#' @examples
#' # These two are equivalent:
#' g2(mtcars, hp ~ mpg) |> mark_point() |> theme_('dark')
#' g2(mtcars, hp ~ mpg) + mark_point() + theme_('dark')
`+.g2` = function(e1, e2) {
  if (inherits(e2, 'g2_mod')) return(e2(e1))
  stop(
    'Cannot add an object of class "', class(e2)[1], '" to a g2 chart. ',
    'Use a gglite modifier function (e.g., mark_point(), theme_(), scale_x()).'
  )
}

#' Create a G2 Chart Object
#'
#' Construct a base chart object, optionally with data and aesthetic mappings.
#' Use R formulas for aesthetic mappings (see **Formula Interface**).
#'
#' @section Formula Interface:
#' Aesthetic mappings use R formulas as a shorthand for column names:
#' \describe{
#'   \item{`y ~ x`}{Sets `x` and `y` as the first positional argument.}
#'   \item{`~ x`}{Sets only `x` (e.g., for histograms or bar counts).}
#'   \item{`~ x1 + x2 + x3`}{Creates a `position` encoding with multiple
#'     fields (for parallel coordinates).}
#'   \item{`y ~ x | z`}{Facets the chart by `z` (column direction).}
#'   \item{`y ~ x | z1 + z2`}{Facets by `z1` (columns) and `z2` (rows).}
#'   \item{`color = ~ var`}{Maps the `color` channel to `var` (one-sided
#'     formula in a named argument). Works for any aesthetic channel.}
#' }
#'
#' @param data A data frame, a `ts`/`mts` time series object, or `NULL`. Time
#'   series objects are automatically converted to data frames (with columns
#'   `time` and `value` for univariate series, or `time`, `series`, and `value`
#'   for multivariate series) and default aesthetic mappings are set
#'   accordingly. By default, only the columns actually referenced by the chart
#'   are included in the HTML output. Wrap `data` in [I()] to opt out of
#'   trimming and preserve all columns — useful when extra columns are accessed
#'   inside inline JavaScript functions that cannot be statically detected.
#' @param ... Aesthetic mappings as `name = ~column` formulas or a positional
#'   formula for `x`/`y`. Character strings are also accepted.
#' @param title Chart title string, a convenient alternative to piping into
#'   [title_()] separately.
#' @param subtitle Chart subtitle string.
#' @return A `g2` object (S3 class).
#' @import stats utils
#' @export
#' @examples
#' g2(mtcars, hp ~ mpg)
#' g2(mtcars, hp ~ mpg, color = ~ cyl)
#' g2(mtcars, ~ mpg)
#'
#' # Time series
#' g2(sunspot.year)
#' g2(EuStockMarkets)
#'
#' # Title and subtitle
#' g2(mtcars, hp ~ mpg, title = 'Motor Trend Cars', subtitle = 'mpg vs hp')
g2 = function(data = NULL, ..., title = NULL, subtitle = NULL) {
  dots = list(...)
  # A positional (unnamed) formula like `hp ~ mpg` or `~ mpg` as the first arg
  has_formula = length(dots) &&
    (is.null(names(dots)) || !nzchar(names(dots)[1])) &&
    inherits(dots[[1]], 'formula')
  facet_from_formula = if (has_formula) {
    parsed = parse_formula(dots[[1]])
    dots = c(parsed$aesthetics, as_vars(dots[-1]))
    parsed$facet
  } else {
    dots = as_vars(dots)
    NULL
  }
  # Convert time series to data frame with default aesthetics
  ts_aes = NULL
  ts_name = NULL
  if (is.ts(data)) {
    ts_name = deparse(substitute(data))
    converted = ts_to_df(data)
    data = converted$data
    ts_aes = converted$aesthetics
  }
  chart = structure(list(
    data = data,
    options = NULL,
    layers = list(),
    scales = list(),
    coords = NULL,
    interactions = list(),
    aesthetics = if (!is.null(ts_aes) && !has_formula) ts_aes else list(),
    ts_origin = !is.null(ts_aes),
    ts_name = ts_name,
    theme = NULL,
    axes = list(),
    legends = list(),
    chart_title = dropNulls(list(title = title, subtitle = subtitle)),
    facet = facet_from_formula,
    layout = list()
  ), class = 'g2')
  if (length(dots)) chart$aesthetics = modifyList(chart$aesthetics, dots)
  chart
}

#' Configure Canvas Options
#'
#' Set chart dimensions, layout spacing, and renderer for a G2 chart. See
#' <https://pkg.yihui.org/gglite/examples/canvas.html> for full examples.
#'
#' @param chart A `g2` object, or `NULL` to create a deferred modifier.
#' @param width Width of the chart in pixels. `NULL` (default) enables
#'   auto-fit to the container width.
#' @param height Height of the chart in pixels. Default is `480`.
#' @param padding,margin,inset Layout spacing in pixels. Each can be a scalar
#'   (applied to all sides) or a length-4 vector `c(top, right, bottom, left)`;
#'   use `NA` to skip individual sides. `NULL` (the default) leaves the value
#'   unset.
#' @param renderer The rendering backend: `"Canvas"` (default), `"SVG"`, or
#'   `"WebGL"` (case-insensitive).
#' @param ... Additional top-level chart options passed to `chart.options()` in
#'   JavaScript (e.g., `clip = TRUE`).
#' @return The modified `g2` object (or a `g2_mod` when `chart` is `NULL`).
#' @export
#' @examples
#' p = g2(mtcars, hp ~ mpg)
#' p |> canvas(width = 600, height = 400)
#' p |> canvas(padding = 30)
#' p |> canvas(renderer = 'svg')
canvas = function(
  chart = NULL, width = NULL, height = 480,
  padding = NULL, margin = NULL, inset = NULL,
  renderer = NULL, ...
) {
  args = list(
    width = width, height = height,
    padding = padding, margin = margin, inset = inset,
    renderer = renderer, ...
  )
  mod = check_chart(canvas, chart, args)
  if (!is.null(mod)) return(mod)

  # Dimensions
  chart$options = dropNulls(list(
    width = width,
    height = height,
    autoFit = if (is.null(width)) TRUE
  ))

  # Layout spacing
  chart$layout = c(
    process_layout('padding', padding),
    process_layout('margin', margin),
    process_layout('inset', inset)
  )

  # Renderer
  if (!is.null(renderer)) {
    r = tolower(renderer)
    r = match.arg(r, c('canvas', 'svg', 'webgl'))
    chart$renderer = r
  }

  # Extra top-level chart.options() args (e.g., clip, depth)
  extra = list(...)
  if (length(extra)) chart$canvas_extra = modifyList(
    as.list(chart$canvas_extra), extra
  )

  chart
}

#' Set Aesthetic Mappings
#'
#' Map data columns to visual channels (x, y, color, size, shape, etc.).
#' Use formulas (e.g., `x = ~ col`) or character strings (e.g., `x = 'col'`).
#'
#' @param chart A `g2` object.
#' @param ... Named mappings as `name = ~column` formulas or character strings,
#'   e.g., `x = ~ col1, color = ~ col2`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars) |> encode(x = ~ mpg, y = ~ hp)
encode = function(chart = NULL, ...) {
  mod = check_chart(encode, chart, list(...))
  if (!is.null(mod)) return(mod)
  chart$aesthetics = modifyList(chart$aesthetics, as_vars(list(...)))
  chart
}


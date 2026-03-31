#' gglite: Lightweight Data Visualization via the Grammar of Graphics
#'
#' An R interface to the AntV G2 JavaScript visualization library.
#'
#' @name gglite-package
#' @keywords internal
"_PACKAGE"

#' CDN URL for the G2 Library
#'
#' Returns the URL for loading the G2 JavaScript library. Customizable via the
#' `gglite.g2_cdn` option. The default `@5` resolves to the latest v5.x
#' release.
#'
#' @return A character string.
#' @keywords internal
g2_cdn = function() {
  getOption('gglite.g2_cdn', 'https://unpkg.com/@antv/g2@5/dist/g2.min.js')
}

g2_col_cdn = 'https://cdn.jsdelivr.net/npm/@xiee/utils/js/g2-column.min.js'

#' Set or Get Global Chart Defaults
#'
#' Similar to [par()], get or set default theme options applied to all charts.
#' When set, these options are merged with per-chart [theme_of()] settings and
#' passed to G2. Chart size defaults (fonts, point size, grid opacity) are
#' applied automatically via a JavaScript patch loaded with every page.
#'
#' @param ... Named theme options in the same structure accepted by [theme_of()],
#'   e.g., `axis = list(labelFontSize = 16)`. Pass a single list (e.g., the
#'   return value of a previous `g2_defaults()` call) to restore saved settings.
#'   Pass `NULL` to clear all R-level defaults.
#' @return The previous defaults, invisibly (like [par()]).
#' @export
#' @examples
#' # Increase axis label size for all subsequent charts
#' old = g2_defaults(axis = list(labelFontSize = 18))
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point()
#' g2_defaults(old)  # restore previous defaults
g2_defaults = function(...) {
  prev = getOption('gglite.theme')
  args = list(...)
  if (!length(args)) return(invisible(prev))
  # A single unnamed arg is a saved value to restore (or NULL to clear)
  val = if (length(args) == 1 && is.null(names(args))) args[[1]] else args
  options(gglite.theme = val)
  invisible(prev)
}

#' Create a G2 Chart Object
#'
#' Construct a base chart object, optionally with data and aesthetic mappings.
#' Column names are passed as character strings.
#'
#' @param data A data frame (or `NULL`).
#' @param ... Aesthetic mappings as `name = 'column'` pairs (character strings).
#' @param width,height Width and height of the chart in pixels.
#' @return A `g2` object (S3 class).
#' @importFrom utils modifyList
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point()
g2 = function(data = NULL, ..., width = 640, height = 480) {
  chart = structure(list(
    data = data,
    options = list(width = width, height = height, autoFit = TRUE),
    layers = list(),
    scales = list(),
    coords = NULL,
    interactions = list(),
    aesthetics = list(),
    theme = NULL,
    axes = list(),
    legends = list(),
    chart_title = NULL,
    facet = NULL,
    padding = list()
  ), class = 'g2')
  dots = list(...)
  if (length(dots)) chart$aesthetics = modifyList(chart$aesthetics, dots)
  chart
}

#' Set Aesthetic Mappings
#'
#' Map data columns to visual channels (x, y, color, size, shape, etc.).
#' Column names are specified as character strings.
#'
#' @param chart A `g2` object.
#' @param ... Named mappings as character strings, e.g.,
#'   `x = 'col1', color = 'col2'`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars) |> encode(x = 'mpg', y = 'hp') |> mark_point()
encode = function(chart, ...) {
  chart$aesthetics = modifyList(chart$aesthetics, list(...))
  chart
}

#' Annotate Data Frames for Column-Major JSON
#'
#' Recursively walks a nested list and wraps any data frame found in a `data`
#' field with `list(type = 'column', value = df)` so that the G2 column-major
#' helper script can convert it client-side.
#'
#' @param x A nested list.
#' @return The annotated list.
#' @keywords internal
annotate_df = function(x) {
  if (is.data.frame(x) || !is.list(x)) return(x)
  nms = names(x)
  if ('data' %in% nms) {
    if (is.data.frame(d <- x$data)) {
      x$data = list(type = 'column', value = d)
    } else if (is.null(d)) x$data = NULL
  }
  idx = setdiff(nms, '')
  idx = if (length(idx)) setdiff(idx, 'data') else seq_along(x)
  for (i in idx) {
    if (is.list(xi <- x[[i]])) x[[i]] = annotate_df(xi)
  }
  x
}

#' Remove NULL elements from a list
#' @keywords internal
dropNulls = function(x) x[!vapply(x, is.null, logical(1))]

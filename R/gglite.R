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
#' @noRd
g2_cdn = function() {
  getOption('gglite.g2_cdn', 'https://unpkg.com/@antv/g2@5/dist/g2.min.js')
}

g2_col_cdn = 'https://cdn.jsdelivr.net/npm/@xiee/utils/js/g2-column.min.js'

#' Process a layout argument (padding, margin, or inset)
#'
#' Convert a scalar or length-4 vector into named G2 layout options.
#' A scalar sets the property directly (e.g., `padding = 20`). A length-4
#' vector sets `Top`, `Right`, `Bottom`, `Left` variants; `NA` values are
#' omitted.
#'
#' @param name Base name: `'padding'`, `'margin'`, or `'inset'`.
#' @param value `NULL`, a scalar, or a length-4 numeric vector.
#' @return A named list of layout options.
#' @noRd
process_layout = function(name, value) {
  if (is.null(value)) return(list())
  if (length(value) == 1) {
    res = list(value)
    names(res) = name
    return(res)
  }
  if (length(value) != 4) stop(
    "'", name, "' must be a scalar or a length-4 vector (top, right, bottom, left)"
  )
  sides = c('Top', 'Right', 'Bottom', 'Left')
  res = stats::setNames(as.list(value), paste0(name, sides))
  dropNulls(lapply(res, function(v) if (is.na(v)) NULL else v))
}

#' Create a G2 Chart Object
#'
#' Construct a base chart object, optionally with data and aesthetic mappings.
#' Column names are passed as character strings.
#'
#' @param data A data frame (or `NULL`).
#' @param ... Aesthetic mappings as `name = 'column'` pairs (character strings).
#' @param width,height Width and height of the chart in pixels.
#' @param padding,margin,inset Layout spacing in pixels. Each can be a scalar
#'   (applied to all sides) or a length-4 vector `c(top, right, bottom, left)`;
#'   use `NA` to skip individual sides. `NULL` (the default) leaves the value
#'   unset.
#' @return A `g2` object (S3 class).
#' @importFrom utils modifyList
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |> mark_point()
g2 = function(
  data = NULL, ..., width = 640, height = 480,
  padding = NULL, margin = NULL, inset = NULL
) {
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
    layout = c(
      process_layout('padding', padding),
      process_layout('margin', margin),
      process_layout('inset', inset)
    )
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
#' @noRd
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
#' @noRd
dropNulls = function(x) x[!vapply(x, is.null, logical(1))]

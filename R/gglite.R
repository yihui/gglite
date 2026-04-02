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

g2_patches_cdn = 'https://cdn.jsdelivr.net/npm/@xiee/utils@v1.14.30/js/g2-patches.min.js'

#' Create a G2 Chart Object
#'
#' Construct a base chart object, optionally with data and aesthetic mappings.
#' Column names are passed as character strings. You can also use a formula
#' (e.g., `y ~ x`) to specify the mappings (see **Formula Interface**).
#'
#' @section Formula Interface:
#' Instead of named aesthetic arguments, you can pass a formula as the first
#' argument after `data`:
#' \describe{
#'   \item{`y ~ x`}{Maps `x` and `y` to the named columns.}
#'   \item{`~ x`}{Maps only `x` (e.g., for histograms or bar counts).}
#'   \item{`~ x1 + x2 + x3`}{Creates a `position` encoding with multiple
#'     fields (for parallel coordinates).}
#'   \item{`y ~ x | z`}{Facets the chart by `z` (column direction).}
#'   \item{`y ~ x | z1 + z2`}{Facets by `z1` (columns) and `z2` (rows).}
#' }
#' Additional aesthetics (e.g., `color`, `size`) can still be passed as named
#' arguments alongside the formula.
#'
#' @param data A data frame (or `NULL`).
#' @param ... Aesthetic mappings as `name = 'column'` pairs (character strings),
#'   or a formula followed by optional named aesthetics.
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
#'
#' # Formula interface
#' g2(mtcars, hp ~ mpg)
#' g2(mtcars, hp ~ mpg, color = 'cyl')
#' g2(mtcars, ~ mpg)
g2 = function(
  data = NULL, ..., width = 640, height = 480,
  padding = NULL, margin = NULL, inset = NULL
) {
  dots = list(...)
  facet_from_formula = NULL
  if (length(dots) && inherits(dots[[1]], 'formula')) {
    parsed = parse_formula(dots[[1]])
    dots = c(parsed$aesthetics, dots[-1])
    facet_from_formula = parsed$facet
  }
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
    facet = facet_from_formula,
    layout = c(
      process_layout('padding', padding),
      process_layout('margin', margin),
      process_layout('inset', inset)
    )
  ), class = 'g2')
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



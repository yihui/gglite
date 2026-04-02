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
#'   \item{`y ~ x | z`}{Groups by `z` (maps to color), inspired by
#'     tinyplot's formula convention.}
#' }
#' Additional aesthetics (e.g., `color`, `size`) can still be passed as named
#' arguments alongside the formula.
#'
#' @param data A data frame, a `ts`/`mts` time series object, or `NULL`. Time
#'   series objects are automatically converted to data frames (with columns
#'   `time` and `value` for univariate series, or `time`, `series`, and `value`
#'   for multivariate series) and default aesthetic mappings are set
#'   accordingly.
#' @param ... Aesthetic mappings as `name = 'column'` pairs (character strings),
#'   or a formula followed by optional named aesthetics.
#' @param width,height Width and height of the chart in pixels.
#' @param padding,margin,inset Layout spacing in pixels. Each can be a scalar
#'   (applied to all sides) or a length-4 vector `c(top, right, bottom, left)`;
#'   use `NA` to skip individual sides. `NULL` (the default) leaves the value
#'   unset.
#' @param main Chart title string, a convenient alternative to calling
#'   [title_()] separately (inspired by base [plot()] and tinyplot).
#' @param sub Chart subtitle string. Only used when `main` is also provided.
#' @param by Column name (character string) to group the data by color,
#'   inspired by tinyplot's `by` argument. Equivalent to passing
#'   `color = 'column'`.
#' @param facet Faceting specification: a one-sided formula (`~ z` for
#'   column faceting, `y ~ x` for grid faceting), or a character string
#'   naming the faceting variable. Inspired by tinyplot's `facet` argument.
#'   For more control, use [facet_rect()] or [facet_circle()] instead.
#' @param alpha Numeric in `[0, 1]` controlling the fill and stroke opacity
#'   of marks. A convenient alternative to calling
#'   `style_mark(fillOpacity = ..., strokeOpacity = ...)`. Inspired by
#'   tinyplot's `alpha` argument.
#' @param palette Character string naming a color palette (e.g.,
#'   `'category10'`, `'tableau10'`, `'set2'`). A convenient alternative to
#'   calling `scale_color(palette = ...)`. Inspired by tinyplot's `palette`
#'   argument.
#' @return A `g2` object (S3 class).
#' @import stats utils
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp')
#'
#' # Formula interface
#' g2(mtcars, hp ~ mpg)
#' g2(mtcars, hp ~ mpg, color = 'cyl')
#' g2(mtcars, ~ mpg)
#'
#' # Time series
#' g2(sunspot.year)
#' g2(EuStockMarkets)
#'
#' # Title and subtitle (like base plot's main/sub)
#' g2(mtcars, hp ~ mpg, main = 'Motor Trend Cars', sub = 'mpg vs hp')
#'
#' # Grouping by color (like tinyplot's by argument)
#' g2(iris, Sepal.Length ~ Sepal.Width, by = 'Species')
#'
#' # Formula | for color grouping (tinyplot convention)
#' g2(iris, Sepal.Length ~ Sepal.Width | Species)
#'
#' # Faceting via the facet argument (like tinyplot)
#' g2(iris, Sepal.Length ~ Sepal.Width, facet = ~Species)
#' g2(mtcars, hp ~ mpg, facet = gear ~ cyl)
#'
#' # Adjust opacity (like tinyplot's alpha)
#' g2(iris, Sepal.Length ~ Sepal.Width | Species, alpha = 0.5)
#'
#' # Set color palette (like tinyplot's palette)
#' g2(iris, Sepal.Length ~ Sepal.Width | Species, palette = 'set2')
g2 = function(
  data = NULL, ..., width = 640, height = 480,
  padding = NULL, margin = NULL, inset = NULL,
  main = NULL, sub = NULL, by = NULL, facet = NULL, alpha = NULL,
  palette = NULL
) {
  dots = list(...)
  by_from_formula = NULL
  has_formula = length(dots) && inherits(dots[[1]], 'formula')
  if (has_formula) {
    parsed = parse_formula(dots[[1]])
    dots = c(parsed$aesthetics, dots[-1])
    by_from_formula = parsed$by
  }
  # Build facet from the facet argument (formula or character)
  facet_config = if (inherits(facet, 'formula')) {
    fterms = if (length(facet) == 3) {
      list(y = deparse(facet[[2]]), x = deparse(facet[[3]]))
    } else {
      list(x = deparse(facet[[2]]))
    }
    list(type = 'facetRect', encode = fterms)
  } else if (is.character(facet)) {
    list(type = 'facetRect', encode = list(x = facet))
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
    options = list(width = width, height = height, autoFit = TRUE),
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
    chart_title = if (!is.null(main)) {
      if (!is.null(sub)) list(title = main, subtitle = sub) else main
    },
    facet = facet_config,
    alpha = alpha,
    layout = c(
      process_layout('padding', padding),
      process_layout('margin', margin),
      process_layout('inset', inset)
    )
  ), class = 'g2')
  if (length(dots)) chart$aesthetics = modifyList(chart$aesthetics, dots)
  # by argument (explicit or from formula |) sets color grouping
  if (is.null(by) && !is.null(by_from_formula)) by = by_from_formula
  if (!is.null(by)) chart$aesthetics$color = by
  if (!is.null(palette)) chart$scales$color = list(palette = palette)
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
#' g2(mtcars) |> encode(x = 'mpg', y = 'hp')
encode = function(chart, ...) {
  chart$aesthetics = modifyList(chart$aesthetics, list(...))
  chart
}

#' Shorthand Alias for [g2()]
#'
#' `gg()` is a convenient shorthand for [g2()], inspired by tinyplot's `plt()`
#' alias. All arguments are passed through to [g2()].
#'
#' @inheritParams g2
#' @export
#' @examples
#' gg(mtcars, x = 'mpg', y = 'hp')
gg = g2


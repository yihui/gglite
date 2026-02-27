#' @title gglite: Lightweight Data Visualization via the Grammar of Graphics
#'
#' @description An R interface to the AntV G2 JavaScript visualization library
#'   with a ggplot2-style API.
#'
#' @name gglite-package
#' @keywords internal
"_PACKAGE"

#' Get the CDN URL for the G2 Library
#'
#' Returns the URL for loading the G2 JavaScript library. Customizable via the
#' `gglite.g2_cdn` global option. The default `@5` resolves to the latest v5.x
#' release. To pin a specific version, use e.g.,
#' `options(gglite.g2_cdn = 'https://unpkg.com/@antv/g2@@5.2.1/dist/g2.min.js')`.
#'
#' @return A character string with the CDN URL.
#' @keywords internal
g2_cdn = function() {
  getOption('gglite.g2_cdn', 'https://unpkg.com/@antv/g2@5/dist/g2.min.js')
}

#' Create a G2 Chart Object
#'
#' Construct a base chart object, optionally with data and aesthetic mappings.
#'
#' @param data A data frame (or `NULL`).
#' @param ... Aesthetic mappings passed to [encode()]. Arguments are captured as
#'   unevaluated expressions (variable names from `data`).
#' @param width,height Width and height of the chart in pixels.
#' @return A `g2` object (S3 class).
#' @importFrom utils modifyList
#' @export
#' @examples
#' g2(mtcars, x = mpg, y = hp) |> mark_point()
g2 = function(data = NULL, ..., width = 640, height = 480) {
  chart = structure(list(
    data = data,
    options = list(width = width, height = height, autoFit = TRUE),
    layers = list(),
    scales = list(),
    coords = NULL,
    interactions = list(),
    aesthetics = list()
  ), class = 'g2')
  dots = match.call(expand.dots = FALSE)[['...']]
  if (length(dots)) {
    aes = lapply(dots, function(expr) deparse(expr))
    chart$aesthetics = modifyList(chart$aesthetics, aes)
  }
  chart
}

#' Set Aesthetic Mappings
#'
#' Map data variables to visual channels (x, y, color, size, shape, etc.).
#' Arguments are captured as unevaluated expressions and stored in the chart
#' object's `aesthetics` field.
#'
#' @param chart A `g2` object.
#' @param ... Named aesthetic mappings, e.g., `x = var1, y = var2`.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars) |> encode(x = mpg, y = hp) |> mark_point()
encode = function(chart, ...) {
  dots = match.call(expand.dots = FALSE)[['...']]
  aes = lapply(dots, function(expr) deparse(expr))
  chart$aesthetics = modifyList(chart$aesthetics, aes)
  chart
}

#' Add a Geometry Layer (Mark)
#'
#' Generic function to add a mark (geometry layer) to the chart.
#'
#' @param chart A `g2` object.
#' @param type Character string indicating the mark type (e.g., `'interval'`,
#'   `'line'`, `'point'`, `'area'`).
#' @param ... Additional mark-level options passed to G2.
#' @return The modified `g2` object.
#' @keywords internal
mark = function(chart, type, ...) {
  opts = list(...)
  layer = list(type = type)
  if (length(opts)) layer = c(layer, opts)
  chart$layers = c(chart$layers, list(layer))
  chart
}

#' Add an Interval Mark (Bar/Column Chart)
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2)), x = x, y = y) |>
#'   mark_interval()
mark_interval = function(chart, ...) mark(chart, 'interval', ...)

#' Add a Line Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(data.frame(x = 1:10, y = rnorm(10)), x = x, y = y) |> mark_line()
mark_line = function(chart, ...) mark(chart, 'line', ...)

#' Add a Point Mark (Scatter Plot)
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(mtcars, x = mpg, y = hp) |> mark_point()
mark_point = function(chart, ...) mark(chart, 'point', ...)

#' Add an Area Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(data.frame(x = 1:10, y = cumsum(rnorm(10))), x = x, y = y) |> mark_area()
mark_area = function(chart, ...) mark(chart, 'area', ...)

#' Configure a Scale
#'
#' Add or modify scale settings for a given aesthetic field.
#'
#' @param chart A `g2` object.
#' @param field Character string naming the aesthetic (e.g., `'x'`, `'y'`,
#'   `'color'`).
#' @param ... Scale options passed to G2 (e.g., `type = 'log'`,
#'   `range = c(0, 1)`).
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars, x = mpg, y = hp) |>
#'   mark_point() |>
#'   scale_of('x', type = 'log')
scale_of = function(chart, field, ...) {
  chart$scales[[field]] = list(...)
  chart
}

#' Set the Coordinate System
#'
#' @param chart A `g2` object.
#' @param type Coordinate type. Supported types include `'polar'`, `'theta'`,
#'   `'radial'`, `'radar'`, `'helix'`, `'parallel'`, and `'cartesian'` (the
#'   default). See the G2 coordinate documentation for details.
#' @param ... Additional coordinate options (e.g., `innerRadius`, `outerRadius`,
#'   `startAngle`, `endAngle`, or `transform`).
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(data.frame(x = c('A', 'B'), y = c(3, 7)), x = x, y = y) |>
#'   mark_interval() |>
#'   coordinate('theta')
coordinate = function(chart, type, ...) {
  chart$coords = c(list(type = type), list(...))
  chart
}

#' Add an Interaction
#'
#' @param chart A `g2` object.
#' @param type Interaction type (e.g., `'tooltip'`, `'elementHighlight'`).
#' @param ... Additional interaction options.
#' @return The modified `g2` object.
#' @export
#' @examples
#' g2(mtcars, x = mpg, y = hp) |>
#'   mark_point() |>
#'   interact('tooltip')
interact = function(chart, type, ...) {
  entry = c(list(type = type), list(...))
  chart$interactions = c(chart$interactions, list(entry))
  chart
}

# ---- Configuration builder ----

#' Build G2 Spec
#'
#' Convert a `g2` chart object into a nested list matching G2's
#' `chart.options()` spec format (data, marks, scales, etc.). Chart-level
#' constructor options (width, height, container) are handled separately by
#' [chart_html()].
#'
#' @param chart A `g2` object.
#' @return A list suitable for JSON serialization.
#' @keywords internal
build_config = function(chart) {
  config = list()

  # Convert data frame to list-of-rows
  if (!is.null(chart$data)) {
    config$data = lapply(seq_len(nrow(chart$data)), function(i) {
      as.list(chart$data[i, , drop = FALSE])
    })
  }

  # Build marks (layers), each with encode from aesthetics
  marks = lapply(chart$layers, function(layer) {
    m = list(type = layer$type)
    enc = chart$aesthetics
    if (length(enc)) m$encode = enc
    extra = layer[setdiff(names(layer), 'type')]
    if (length(extra)) m = c(m, extra)
    m
  })
  if (length(marks)) config$children = marks

  if (length(chart$scales)) config$scale = chart$scales
  if (!is.null(chart$coords)) config$coordinate = chart$coords
  if (length(chart$interactions)) config$interaction = chart$interactions

  config
}

# ---- HTML generation ----

#' Generate Chart HTML
#'
#' Create an HTML string containing a container `<div>` and a `<script>` block
#' that renders the chart using G2. Constructor options (container, width,
#' height) are passed to `G2.Chart()`, while the visualization spec (data,
#' marks, scales) is passed to `chart.options()`.
#'
#' @param chart A `g2` object.
#' @param id Container element ID (auto-generated if `NULL`).
#' @param width,height CSS dimensions for the container.
#' @return A character string of HTML.
#' @export
chart_html = function(chart, id = NULL, width = NULL, height = NULL) {
  if (is.null(id)) id = paste0('gglite-', as.integer(Sys.time()), '-',
                                sample.int(1e6, 1))
  # Constructor args for G2.Chart()
  ctor = chart$options
  ctor$container = id

  # Visualization spec for chart.options()
  spec = build_config(chart)

  w = if (!is.null(width)) paste0('width:', width, 'px;') else ''
  h = if (!is.null(height)) paste0('height:', height, 'px;') else ''
  style = paste0(w, h)
  if (nzchar(style)) style = paste0(' style="', style, '"')

  paste0(
    '<div id="', id, '"', style, '></div>\n',
    '<script type="module">\n',
    'const chart = new G2.Chart(', xfun::tojson(ctor), ');\n',
    'chart.options(', xfun::tojson(spec), ');\n',
    'chart.render();\n',
    '</script>'
  )
}

#' Preview a Chart in the Viewer or Browser
#'
#' Wrap a chart in a full HTML document and display it via `xfun::html_view()`.
#'
#' @param chart A `g2` object.
#' @param ... Additional arguments passed to [chart_html()].
#' @return The path to the temporary HTML file (invisibly).
#' @export
preview = function(chart, ...) {
  body = chart_html(chart, ...)
  html = paste0(
    '<!DOCTYPE html>\n<html>\n<head>\n',
    '<meta charset="utf-8">\n',
    '<script defer src="', g2_cdn(), '"></script>\n',
    '</head>\n<body>\n',
    body, '\n',
    '</body>\n</html>'
  )
  xfun::html_view(html)
}

#' @export
print.g2 = function(x, ...) {
  if (requireNamespace('knitr', quietly = TRUE) &&
      isTRUE(getOption('knitr.in.progress'))) {
    print(knit_print.g2(x, ...))
  } else {
    preview(x, ...)
  }
  invisible(x)
}

#' Custom Printing in Knitr
#'
#' @param x A `g2` object.
#' @param ... Ignored.
#' @return A `knit_asis` character vector.
knit_print.g2 = function(x, ...) {
  cdn_tag = paste0('<script defer src="', g2_cdn(), '"></script>')
  body = chart_html(x, ...)
  out = paste0(cdn_tag, '\n', body)
  structure(out, class = c('knit_asis', 'html'))
}

.onLoad = function(libname, pkgname) {
  if (requireNamespace('knitr', quietly = TRUE)) {
    registerS3method('knit_print', 'g2', knit_print.g2,
                     envir = asNamespace('knitr'))
  }
}

#' Render a Chart in Shiny
#'
#' Build the chart configuration and send it to the client via
#' `session$sendCustomMessage()`.
#'
#' @param chart A `g2` object.
#' @param session The Shiny session object.
#' @param output_id The container element ID on the client side.
#' @export
render_shiny = function(chart, session, output_id) {
  spec = build_config(chart)
  ctor = chart$options
  session$sendCustomMessage('g2-render', list(
    id = output_id, ctor = ctor, spec = spec
  ))
}

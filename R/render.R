# ---- Configuration builder ----

#' Build G2 Spec
#'
#' Convert a `g2` chart object into a nested list matching G2's
#' `chart.options()` spec format. Data frames are annotated for column-major
#' JSON serialisation via [annotate_df()]. Constructor options (width, height,
#' container) are handled separately by [chart_html()].
#'
#' @param chart A `g2` object.
#' @return A list suitable for JSON serialization.
#' @keywords internal
build_config = function(chart) {
  config = list()

  # Data (column-major; annotate_df wraps it)
  if (!is.null(chart$data)) config$data = chart$data

  # Build marks (layers)
  marks = lapply(chart$layers, function(layer) {
    m = list(type = layer$type)
    # Top-level encode shared by all marks
    enc = chart$aesthetics
    if (length(enc)) m$encode = enc
    # Layer-specific properties (may override encode, add style, transform, etc.)
    extra = layer[setdiff(names(layer), 'type')]
    if (length(extra)) m = modifyList(m, extra)
    m
  })
  if (length(marks)) config$children = marks

  # Chart-wide config
  if (length(chart$scales)) config$scale = chart$scales
  if (!is.null(chart$coords)) config$coordinate = chart$coords
  if (length(chart$interactions)) config$interaction = chart$interactions
  if (!is.null(chart$theme)) config$theme = chart$theme
  if (length(chart$axes)) config$axis = chart$axes
  if (length(chart$legends)) config$legend = chart$legends
  if (!is.null(chart$chart_title)) config$title = chart$chart_title
  if (!is.null(chart$tooltip_config)) config$tooltip = chart$tooltip_config
  if (!is.null(chart$sliders)) config$slider = chart$sliders
  if (!is.null(chart$scrollbars)) config$scrollbar = chart$scrollbars

  # Faceting wraps the spec as a facet view
  if (!is.null(chart$facet)) {
    config$type = chart$facet$type
    if (!is.null(chart$facet$encode)) {
      config$encode = chart$facet$encode
    }
    facet_extra = chart$facet[setdiff(names(chart$facet), c('type', 'encode'))]
    if (length(facet_extra)) config = modifyList(config, facet_extra)
  }

  annotate_df(config)
}

# ---- HTML generation ----

#' Generate Chart HTML
#'
#' Create an HTML string containing a container `<div>` and a `<script>` block
#' that renders the chart using G2.
#'
#' @param chart A `g2` object.
#' @param id Container element ID (auto-generated if `NULL`).
#' @param width,height Optional CSS dimensions for the container.
#' @return A character string of HTML.
#' @export
chart_html = function(chart, id = NULL, width = NULL, height = NULL) {
  if (is.null(id)) id = paste0('gglite-', as.integer(Sys.time()), '-',
                                sample.int(1e6, 1))
  ctor = chart$options
  ctor$container = id
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
    '<script defer src="', g2_col_cdn(), '"></script>\n',
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
  cdn = paste0(
    '<script defer src="', g2_cdn(), '"></script>\n',
    '<script defer src="', g2_col_cdn(), '"></script>'
  )
  body = chart_html(x, ...)
  out = paste0(cdn, '\n', body)
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

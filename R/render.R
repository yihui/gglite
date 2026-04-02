#' Choose a Mark Automatically
#'
#' Inspect the types of the `x` and `y` columns referenced in `aesthetics` and
#' return a suitable mark definition (and optional coordinate override). Returns
#' `NULL` when automatic detection is not possible.
#'
#' @param data A data frame.
#' @param aesthetics A named list of aesthetic mappings (column names).
#' @param ts Whether the data originates from a time series object.
#' @return A list with elements `mark` (a layer list) and `coord` (a coordinate
#'   list or `NULL`), or `NULL`.
#' @noRd
auto_mark = function(data, aesthetics, ts = FALSE) {
  if (is.null(data) || !is.data.frame(data)) return()

  # Multi-field position encoding → line mark + parallel coordinates
  if (length(aesthetics$position) > 1)
    return(list(mark = list(type = 'line'), coord = list(type = 'parallel')))

  x_col = aesthetics$x
  y_col = aesthetics$y
  x = if (!is.null(x_col) && x_col %in% names(data)) data[[x_col]]
  y = if (!is.null(y_col) && y_col %in% names(data)) data[[y_col]]
  xt = var_type(x)
  yt = var_type(y)
  coord = NULL
  mark = if (ts && xt == 'numeric' && yt == 'numeric') {
    list(type = 'line')
  } else if (xt == 'numeric' && yt == 'numeric') {
    list(type = 'point', style = list(shape = 'point'))
  } else if (xt == 'categorical' && yt == 'numeric') {
    list(type = 'boxplot')
  } else if (xt == 'numeric' && yt == 'categorical') {
    coord = list(transform = list(list(type = 'transpose')))
    list(type = 'boxplot', encode = list(x = y_col, y = x_col))
  } else if (xt == 'categorical' && yt == 'categorical') {
    if (is.null(aesthetics$color)) list(
      type = 'cell', encode = list(color = 'count'),
      transform = list(list(type = 'group', color = 'count'))
    ) else list(type = 'cell')
  } else if (xt == 'date' && yt == 'numeric') {
    list(type = 'line')
  } else if (xt == 'numeric' && yt == 'none') {
    list(type = 'interval', transform = list(list(type = 'binX', y = 'count')))
  } else if (xt == 'categorical' && yt == 'none') {
    list(type = 'interval', transform = list(list(type = 'groupX', y = 'count')))
  }
  if (!is.null(mark)) list(mark = mark, coord = coord)
}

# ---- Configuration builder ----

#' Build G2 Spec
#'
#' Convert a `g2` chart object into a nested list matching G2's
#' `chart.options()` spec format. Data frames are annotated for column-major
#' JSON serialisation. Constructor options (width, height, container) are
#' handled separately by [chart_html()].
#'
#' @param chart A `g2` object.
#' @return A list suitable for JSON serialization.
#' @noRd
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

  # Auto-detect mark type when no layers are configured
  if (!length(marks) && !is.null(chart$data)) {
    auto = auto_mark(chart$data, chart$aesthetics, ts = isTRUE(chart$ts_origin))
    if (!is.null(auto)) {
      m = list(type = auto$mark$type)
      enc = chart$aesthetics
      if (!is.null(auto$mark$encode))
        enc = modifyList(enc, auto$mark$encode)
      if (length(enc)) m$encode = enc
      extra = auto$mark[setdiff(names(auto$mark), c('type', 'encode'))]
      if (length(extra)) m = modifyList(m, extra)
      marks = list(m)
      if (!is.null(auto$coord) && is.null(chart$coords))
        config$coordinate = auto$coord
    }
  }

  if (length(marks)) config$children = marks

  # Chart-wide config
  if (length(chart$scales)) config$scale = chart$scales
  if (!is.null(chart$coords)) config$coordinate = chart$coords
  if (length(chart$interactions)) config$interaction = chart$interactions
  if (length(chart$axes)) config$axis = chart$axes
  # Default y-axis title for time series data (use data object name)
  if (isTRUE(chart$ts_origin) && !is.null(chart$ts_name)) {
    y_ax = config$axis$y
    if (is.null(y_ax$title))
      config$axis$y = modifyList(as.list(y_ax), list(title = chart$ts_name))
  }
  if (length(chart$legends)) config$legend = chart$legends
  if (!is.null(chart$chart_title)) config$title = chart$chart_title
  if (!is.null(chart$tooltip_config)) config$tooltip = chart$tooltip_config
  if (!is.null(chart$sliders)) config$slider = chart$sliders
  if (!is.null(chart$scrollbars)) config$scrollbar = chart$scrollbars
  if (length(chart$layout)) config = modifyList(config, chart$layout)

  # Theme: merge global option with per-chart theme
  theme = modifyList(as.list(getOption('gglite.theme')), as.list(chart$theme))
  if (length(theme)) config$theme = theme

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
#' The global option `gglite.defer_render` controls whether chart rendering is
#' deferred until the container scrolls into the viewport. Set
#' `options(gglite.defer_render = TRUE)` to use the default threshold (0.5,
#' i.e., 50% of the chart visible), or supply a numeric value between 0 and 1
#' to customise it, e.g., `options(gglite.defer_render = 0.3)`. When enabled,
#' enter animations fire when the reader first sees each chart instead of on
#' page load. This is useful for demo or documentation pages. It is not
#' typically needed for regular plots.
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
  defer_opt = getOption('gglite.defer_render')
  threshold = if (isTRUE(defer_opt)) 0.5 else if (is.numeric(defer_opt)) defer_opt

  # G2 dark themes render content with light colors on a transparent canvas;
  # set a dark background so the chart is visible on light pages
  dark = isTRUE(chart$theme$type %in% c('dark', 'classicDark'))
  w = if (!is.null(width)) paste0('width:', width, 'px;') else ''
  h = if (!is.null(height)) paste0('height:', height, 'px;') else ''
  bg = if (dark) 'background-color:#141414;' else ''

  if (!is.null(threshold)) {
    # Ensure container has min-height so IntersectionObserver can trigger
    ch = if (is.null(ctor$height)) 480 else ctor$height
    mh = paste0('min-height:', ch, 'px;')
    style = paste0(w, h, bg, mh)
    spec_js = paste0('const spec = ', xfun::tojson(spec), ';\n')
    options_js = 'chart.options(spec);\n'
    render_js = paste0(
      'new IntersectionObserver((entries, obs) => {\n',
      '  if (entries[0].isIntersecting) {\n',
      '    chart.render();\n',
      '    obs.disconnect();\n',
      '  }\n',
      '}, { threshold: ', threshold,
      ' }).observe(document.getElementById("', id, '"));\n'
    )
  } else {
    style = paste0(w, h, bg)
    spec_js = ''
    options_js = paste0('chart.options(', xfun::tojson(spec), ');\n')
    render_js = 'chart.render();\n'
  }

  if (nzchar(style)) style = paste0(' style="', style, '"')

  paste0(
    '<div id="', id, '"', style, '></div>\n',
    '<script type="module">\n',
    spec_js,
    'const chart = new G2.Chart(', xfun::tojson(ctor), ');\n',
    options_js,
    render_js,
    '</script>'
  )
}

cdn_scripts = function() {
  sprintf('<script src="%s" defer></script>', c(g2_cdn(), g2_patches_cdn))
}

#' Preview a Chart in the Viewer or Browser
#'
#' @param x A `g2` object.
#' @param ... Additional arguments passed to [chart_html()].
#' @return The chart object (invisibly).
#' @export
print.g2 = function(x, ...) {
  body = chart_html(x, ...)
  html = c(
    '<!DOCTYPE html>', '<html>', '<head>',
    '<meta charset="utf-8">',
    cdn_scripts(),
    '</head>', '<body>',
    body,
    '</body>', '</html>'
  )
  #TODO: xfun >= 0.57.3 no longer needs paste()
  xfun::html_view(paste(html, collapse = '\n'))
  invisible(x)
}

#' Custom Printing in Knitr
#'
#' @param x A `g2` object.
#' @param ... Ignored.
#' @return A `knit_asis` character vector.
knit_print.g2 = function(x, ...) {
  out = paste(c(cdn_scripts(), chart_html(x, ...)), collapse = '\n')
  structure(out, class = c('knit_asis', 'html'))
}

#' @importFrom xfun record_print
#' @export
record_print.g2 = function(x, ...) {
  xfun::new_record(c(cdn_scripts(), chart_html(x, ...)), 'asis')
}

.onLoad = function(...) {
  setHook(packageEvent('knitr', 'onLoad'), function(...) {
    registerS3method('knit_print', 'g2', knit_print.g2, envir = asNamespace('knitr'))
  })
}

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
    return(list(marks = list(list(type = 'line')), coord = list(type = 'parallel')))

  x_col = aesthetics$x
  y_col = aesthetics$y
  x = if (!is.null(x_col) && x_col %in% names(data)) data[[x_col]]
  y = if (!is.null(y_col) && y_col %in% names(data)) data[[y_col]]
  xt = var_type(x)
  yt = var_type(y)
  coord = NULL

  # For categorical vs numeric: bar if categories are unique, beeswarm if
  # repeated, and overlay density when the smallest group has >= 30 values
  cat_num_marks = function(cat_var, cat_col, num_col) {
    if (!anyDuplicated(cat_var)) return(list(list(type = 'interval')))
    bee = list(type = 'beeswarm')
    freq = table(cat_var)
    if (length(freq) && min(freq) >= 30) list(
      modifyList(bee, list(encode = list(size = 2))),
      list(
      type = 'density',
      data = list(transform = list(list(
        type = 'kde', field = num_col,
        groupBy = list(cat_col), size = 20L
      ))),
      encode = list(x = cat_col, y = 'y', size = 'size'),
      tooltip = FALSE,
      style = list(opacity = 0.5, pointerEvents = 'none')
    ))
    else list(bee)
  }

  marks = if (ts && xt == 'numeric' && yt == 'numeric') {
    list(list(type = 'line'))
  } else if (xt == 'numeric' && yt == 'numeric') {
    list(list(type = 'point', style = list(shape = 'point')))
  } else if (xt == 'categorical' && yt == 'numeric') {
    cat_num_marks(x, x_col, y_col)
  } else if (xt == 'numeric' && yt == 'categorical') {
    coord = list(transform = list(list(type = 'transpose')))
    enc = list(x = y_col, y = x_col)
    lapply(cat_num_marks(y, y_col, x_col), function(m) {
      if (is.null(m$encode)) m$encode = enc
      else if (is.null(m$encode$x)) m$encode = modifyList(enc, m$encode)
      m
    })
  } else if (xt == 'categorical' && yt == 'categorical') {
    if (is.null(aesthetics$color)) list(list(
      type = 'cell', encode = list(color = 'count'),
      transform = list(list(type = 'group', color = 'count'))
    )) else list(list(type = 'cell'))
  } else if (xt == 'date' && yt == 'numeric') {
    list(list(type = 'line'))
  } else if (xt == 'numeric' && yt == 'none') {
    list(list(
      type = 'rect',
      transform = list(list(type = 'binX', y = 'count')),
      style = list(stroke = 'white')
    ))
  } else if (xt == 'categorical' && yt == 'none') {
    list(list(
      type = 'interval', transform = list(list(type = 'groupX', y = 'count'))
    ))
  }
  if (length(marks)) list(marks = marks, coord = coord)
}

# Ensure at least one mark exists, auto-adding via auto_mark() when none have
# been added explicitly. If auto_mark() returns multiple marks (e.g., beeswarm
# plus a density overlay for large groups), all are added. Returns the modified
# chart. Stops if auto-detection fails (no data or unrecognized types).
ensure_mark = function(chart) {
  auto = auto_mark(chart$data, chart$aesthetics, ts = isTRUE(chart$ts_origin))
  if (is.null(auto)) stop('add a mark first')
  for (am in auto$marks) {
    enc = chart$aesthetics
    if (!is.null(am$encode)) enc = modifyList(enc, am$encode)
    layer = modifyList(am, if (length(enc)) list(encode = enc) else list())
    chart$layers = c(chart$layers, list(layer))
  }
  if (!is.null(auto$coord) && is.null(chart$coords))
    chart$coords = auto$coord
  chart
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

  # Collect all variables referenced in the chart for data trimming
  all_vars = collect_vars(chart)

  # Data (column-major; annotate_df wraps it)
  if (!is.null(chart$data)) config$data = trim_data(chart$data, all_vars)

  # Build marks (layers)
  marks = lapply(chart$layers, function(layer) {
    m = list(type = layer$type)
    # Top-level encode shared by all marks
    enc = chart$aesthetics
    if (length(enc)) m$encode = enc
    # Layer-specific properties (may override encode, add style, transform, etc.)
    extra = layer[setdiff(names(layer), 'type')]
    if (length(extra)) m = modifyList(m, extra)
    # Trim mark-level data frames (merge chart aesthetics + layer encode +
    # labels text for vars)
    if (is.data.frame(m$data)) {
      lv = c(chart$aesthetics, layer$encode, lapply(layer$labels, `[[`, 'text'))
      m$data = trim_data(m$data, lv)
    }
    m
  })

  # Auto-detect mark type when no layers are configured
  if (!length(marks) && !is.null(chart$data)) {
    auto = auto_mark(chart$data, chart$aesthetics, ts = isTRUE(chart$ts_origin))
    if (!is.null(auto)) {
      # Drop rows with NA in aesthetic columns for auto-detected marks
      cols = unlist(chart$aesthetics[c('x', 'y')])
      cols = cols[cols %in% names(config$data)]
      if (length(cols)) {
        ok = complete.cases(config$data[cols])
        if (any(!ok)) config$data = config$data[ok, , drop = FALSE]
      }
      marks = lapply(auto$marks, function(am) {
        m = list(type = am$type)
        enc = chart$aesthetics
        if (!is.null(am$encode)) enc = modifyList(enc, am$encode)
        if (length(enc)) m$encode = enc
        extra = am[setdiff(names(am), c('type', 'encode'))]
        if (length(extra)) m = modifyList(m, extra)
        m
      })
      if (!is.null(auto$coord) && is.null(chart$coords))
        config$coordinate = auto$coord
    }
  }

  # G2 highlight interactions (legendHighlight, elementHighlight, etc.)
  # require explicit state config on marks to be visible; inject defaults when
  # the user hasn't provided any
  hi = c(
    'legendHighlight', 'elementHighlight',
    'elementHighlightByX', 'elementHighlightByColor'
  )
  if (any(hi %in% names(chart$interactions))) marks = lapply(marks, function(m) {
    if (is.null(m$state$inactive)) m$state$inactive = list(opacity = 0.5)
    m
  })

  if (length(marks)) config$children = marks

  # Chart-wide config
  if (length(chart$scales)) config$scale = chart$scales

  # Auto-detect time scale for Date/POSIXt columns
  if (is.data.frame(chart$data)) for (ch in c('x', 'y')) {
    col = chart$aesthetics[[ch]]
    if (!is.null(col) && col %in% names(chart$data) &&
        (inherits(chart$data[[col]], 'Date') ||
         inherits(chart$data[[col]], 'POSIXt')) &&
        is.null(config$scale[[ch]]$type))
      config$scale[[ch]]$type = 'time'
  }

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
  if (length(chart$chart_title)) config$title = chart$chart_title
  config$tooltip = chart$tooltip_config
  config$slider = chart$sliders
  config$scrollbar = chart$scrollbars
  if (length(chart$layout)) config = modifyList(config, chart$layout)
  if (length(chart$canvas_extra)) config = modifyList(config, chart$canvas_extra)

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

# Returns the effective renderer string for a chart ('canvas', 'svg', 'webgl').
# Per-chart setting (chart$renderer) takes precedence over the global option.
effective_renderer = function(chart) {
  chart$renderer %||% tolower(getOption('gglite.renderer') %||% 'canvas')
}

# Returns TRUE when the page should use g2.lite (global renderer option set
# to any value, or per-chart renderer is svg/webgl).
needs_lite = function(chart) {
  !is.null(getOption('gglite.renderer')) || isTRUE(chart$renderer %in% c('svg', 'webgl'))
}

cdn_scripts = function(chart = NULL) {
  sprintf('<script src="%s" defer></script>', g2_cdn(chart))
}

g2_html_page = function(body, chart = NULL) {
  paste(c(
    '<!DOCTYPE html>', '<html>', '<head>',
    '<meta charset="utf-8">',
    cdn_scripts(chart),
    '</head>', '<body>',
    body,
    '</body>', '</html>'
  ), collapse = '\n')
}

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
#' @param id Optional container element ID. When `NULL` (default), a
#'   class-based container is used instead (more git-friendly output).
#' @param width,height Optional CSS dimensions for the container.
#' @return A character string of HTML.
#' @export
chart_html = function(chart, id = NULL, width = NULL, height = NULL) {
  ctor = dropNulls(chart$options %||% list(height = 480L, autoFit = TRUE))
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
    observe_target = if (is.null(id)) 'el' else paste0('document.getElementById("', id, '")')
    render_js = paste0(
      'new IntersectionObserver((entries, obs) => {\n',
      '  if (entries[0].isIntersecting) {\n',
      '    chart.render();\n',
      '    obs.disconnect();\n',
      '  }\n',
      '}, { threshold: ', threshold,
      ' }).observe(', observe_target, ');\n'
    )
  } else {
    style = paste0(w, h, bg)
    spec_js = ''
    options_js = paste0('chart.options(', xfun::tojson(spec), ');\n')
    render_js = 'chart.render();\n'
  }

  if (nzchar(style)) style = paste0(' style="', style, '"')

  # Renderer setup: when using g2.lite (non-canvas renderer or global option
  # set), pass a raw JS renderer instantiation directly in the ctor.
  if (needs_lite(chart)) {
    r = effective_renderer(chart)
    r_ns = switch(r, svg = 'window.G.SVG', webgl = 'window.G.WebGL', canvas = 'window.G.Canvas2D')
    ctor$renderer = js(paste0('new ', r_ns, '.Renderer()'))
  }

  if (is.null(id)) {
    div = paste0('<div data-gglite-container', style, '></div>\n')
    ctor$container = js('el')
    el_js = paste0(
      'const el = document.querySelector("[data-gglite-container]");\n',
      'el.removeAttribute("data-gglite-container");\n'
    )
  } else {
    div = paste0('<div id="', id, '"', style, '></div>\n')
    ctor$container = id
    el_js = ''
  }
  ctor_js = paste0(el_js, 'const chart = new G2.Chart(', xfun::tojson(ctor), ');\n')

  paste0(div, '<script type="module">\n', spec_js, ctor_js, options_js, render_js, '</script>')
}

#' Preview a Chart in the Viewer or Browser
#'
#' @param x A `g2` object.
#' @param ... Additional arguments passed to [chart_html()].
#' @return The chart object (invisibly).
#' @export
print.g2 = function(x, ...) {
  #TODO: xfun >= 0.57.3 no longer needs paste()
  xfun::html_view(g2_html_page(chart_html(x, ...), chart = x))
  invisible(x)
}

# Document-scoped flag stored in opts_knit to include CDN scripts only once per
# knit session. opts_knit is restored between documents, so the flag resets
# automatically, which also allows Quarto (which rejects non-disk-based
# htmltools::htmlDependency sources).
.knitr.flag = 'gglite.scripts_added'

#' Custom Printing in Knitr
#'
#' @param x A `g2` object.
#' @param ... Ignored.
#' @return A `knit_asis` character vector.
knit_print.g2 = function(x, ...) {
  html = chart_html(x)
  if (!isTRUE(knitr::opts_knit$get(.knitr.flag))) {
    knitr::opts_knit$set(setNames(list(TRUE), .knitr.flag))
    html = paste(c(cdn_scripts(x), html), collapse = '\n')
  }
  structure(html, class = c('knit_asis', 'html'))
}

#' HTML Representation for Jupyter Notebooks
#'
#' Called by the `repr` package (used by IRkernel) to render g2 charts in
#' Jupyter notebooks. Returns a complete HTML page so the chart is displayed
#' inside a sandboxed output cell.
#'
#' @param obj A `g2` object.
#' @param ... Ignored.
#' @return A character string of complete HTML.
#' @noRd
repr_html.g2 = function(obj, ...) g2_html_page(chart_html(obj), chart = obj)

#' Text Representation for Jupyter Notebooks
#'
#' Returns a brief text description so IRkernel's MIME bundle includes a
#' non-empty `text/plain` entry, which is required before any rich display
#' (including HTML) is sent to the Jupyter frontend.
#'
#' @param obj A `g2` object.
#' @param ... Ignored.
#' @return A character string.
#' @noRd
repr_text.g2 = function(obj, ...) {
  n = if (is.data.frame(obj$data)) nrow(obj$data) else NULL
  marks = paste(vapply(obj$layers, `[[`, '', 'type'), collapse = ', ')
  if (!nzchar(marks)) marks = 'auto'
  n_str = if (is.null(n)) 'no data' else as.character(n)
  sprintf('G2 chart (%s; %s rows)', marks, n_str)
}

#' @importFrom xfun record_print
#' @export
record_print.g2 = function(x, ...) {
  xfun::new_record(c(cdn_scripts(x), chart_html(x, ...), ''), 'asis')
}

register_methods = function(pkgs, generics) {
  for (i in seq_along(pkgs)) local({
    pkg = pkgs[[i]]; generic = generics[[i]]
    hook = function(...) {
      registerS3method(
        generic, 'g2',
        asNamespace('gglite')[[paste0(generic, '.g2')]],
        envir = asNamespace(pkg)
      )
    }
    if (isNamespaceLoaded(pkg)) hook()
    setHook(packageEvent(pkg, 'onLoad'), hook)
  })
}

.onLoad = function(...) {
  register_methods(
    c('knitr', 'repr', 'repr'),
    c('knit_print', 'repr_html', 'repr_text')
  )
}

#' Classify a Variable's Type
#'
#' Returns `'date'`, `'categorical'`, or `'numeric'` for a given vector, or
#' `'none'` when `NULL`.
#'
#' @param x A vector (or `NULL`).
#' @return A single character string.
#' @noRd
var_type = function(x) {
  if (is.null(x)) return('none')
  if (inherits(x, 'Date') || inherits(x, 'POSIXt')) return('date')
  if (is.character(x) || is.factor(x) || is.logical(x)) return('categorical')
  'numeric'
}

#' Convert a Time Series to a Data Frame
#'
#' Converts a `ts` (or `mts`) object into a data frame suitable for plotting.
#' Univariate series produce columns `time` and `value`; multivariate series
#' are reshaped to long format with columns `time`, `series`, and `value`.
#'
#' @param x A `ts` or `mts` object.
#' @return A list with elements `data` (a data frame) and `aesthetics` (a named
#'   list of default aesthetic mappings).
#' @noRd
ts_to_df = function(x) {
  t = as.numeric(time(x))
  if (NCOL(x) > 1) {
    nms = colnames(x)
    d = data.frame(
      time = rep(t, length(nms)),
      series = factor(rep(nms, each = length(t)), levels = nms),
      value = as.numeric(x)
    )
    list(data = d, aesthetics = list(x = 'time', y = 'value', color = 'series'))
  } else {
    list(
      data = data.frame(time = t, value = as.numeric(x)),
      aesthetics = list(x = 'time', y = 'value')
    )
  }
}

#' Remove NULL Elements from a List
#' @noRd
dropNulls = function(x) x[!vapply(x, is.null, logical(1))]

#' Null-coalescing operator
#' @noRd
`%||%` = function(x, y) if (is.null(x)) y else x

#' Process a Layout Argument (padding, margin, or inset)
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
  res = setNames(as.list(value), paste0(name, sides))
  dropNulls(lapply(res, function(v) if (is.na(v)) NULL else v))
}

#' Extract Column References from a G2 Inline Data Transform Spec
#'
#' When a layer's `data` is a list with a `transform` key (rather than a data
#' frame), the transform entries can reference data columns via `field`,
#' `fields`, `groupBy`, or `by`. This helper extracts those names so they can
#' be included in the trimming allowlist.
#'
#' @param data A layer `data` value.
#' @return A list of referenced column names, or `NULL`.
#' @noRd
data_transform_vars = function(data) {
  if (!is.list(data) || is.data.frame(data) || is.null(data$transform)) return()
  lapply(data$transform, `[`, c('field', 'fields', 'groupBy', 'by'))
}

#' Collect Variable Names Used in a Chart
#'
#' Gathers all column names referenced by the chart's aesthetic mappings,
#' facet encodings, and the encode, label text, and inline transform fields of
#' any layer that reads from the chart-level data (i.e., layers that do not
#' supply their own data frame).
#'
#' @param chart A `g2` object.
#' @return A list of variable names.
#' @noRd
collect_vars = function(chart) {
  vars = c(chart$aesthetics, chart$facet$encode)
  # Include vars for layers that use the chart-level data. Layers with their
  # own data *frame* bring their own source; layers with a G2 transform spec
  # (a list) or no data at all still read from the chart-level data frame.
  vars2 = lapply(chart$layers, function(layer) {
    if (!is.data.frame(layer$data)) c(
      layer$encode, lapply(layer$labels, `[[`, 'text'),
      data_transform_vars(layer$data)
    )
  })
  unlist(c(vars, vars2))
}

#' Trim a Data Frame to Used Columns
#'
#' Restricts a data frame to only the columns listed in `vars`. If `data` is
#' wrapped in [I()], the `AsIs` class is stripped and the data is returned
#' untrimmed (all columns preserved) — this lets callers opt out of trimming
#' when variables are referenced outside the visible spec, e.g. inside inline
#' JavaScript functions.
#'
#' @param data A data frame (possibly with class `AsIs`), or any other value.
#' @param vars Character vector of column names to keep.
#' @return The (possibly trimmed) data frame, or `data` unchanged when it is
#'   not a data frame or when trimming is not applicable.
#' @noRd
trim_data = function(data, vars) {
  if (inherits(data, 'AsIs'))
    return(structure(data, class = setdiff(class(data), 'AsIs')))
  vars = unlist(vars)
  if (!is.data.frame(data) || !length(vars)) return(data)
  keep = intersect(vars, names(data))
  if (!length(keep) || length(keep) == ncol(data)) return(data)
  data[, keep, drop = FALSE]
}

#' Annotate Data Frames for Column-Major JSON
#'
#' Recursively walks a nested list and wraps any data frame found in a `data`
#' field with `list(type = 'column', value = df)` so that the G2 column-major
#' helper script can convert it client-side. If a `data` field is a single
#' string starting with `https://`, it is annotated as a `fetch` type instead,
#' which tells G2 to load the data from the URL.
#'
#' @param x A nested list.
#' @return The annotated list.
#' @noRd
annotate_df = function(x) {
  if (is.data.frame(x) || !is.list(x)) return(x)
  nms = names(x)
  if ('data' %in% nms) {
    if (is.data.frame(d <- x$data)) {
      # Convert Date/POSIXt columns to millisecond timestamps for G2
      for (col in names(d)) {
        if (inherits(d[[col]], 'Date'))
          d[[col]] = as.numeric(d[[col]]) * 86400000
        else if (inherits(d[[col]], 'POSIXt'))
          d[[col]] = as.numeric(d[[col]]) * 1000
      }
      x$data = list(type = 'column', value = d)
    } else if (is.character(d) && length(d) == 1 && grepl('^https://', d)) {
      x$data = list(type = 'fetch', value = d)
    } else if (is.null(d)) x$data = NULL
  }
  idx = setdiff(nms, '')
  idx = if (length(idx)) setdiff(idx, 'data') else seq_along(x)
  for (i in idx) {
    if (is.list(xi <- x[[i]])) x[[i]] = annotate_df(xi)
  }
  x
}

#' Extract Terms from a Formula Expression
#'
#' Recursively extracts variable names from a formula expression. The `+`
#' operator separates terms (e.g., `x1 + x2` yields `c('x1', 'x2')`);
#' any other expression is deparsed as-is.
#'
#' @param expr A language object.
#' @return A character vector of term names.
#' @noRd
extract_terms = function(expr) {
  if (is.name(expr)) return(as.character(expr))
  if (is.call(expr) && identical(expr[[1]], as.name('+')))
    return(c(extract_terms(expr[[2]]), extract_terms(expr[[3]])))
  deparse(expr)
}

#' Convert a One-Sided Formula to a Variable Name
#'
#' Converts a one-sided formula of the form `~ var` to the character string
#' `"var"`. All other values are returned unchanged. This allows users to write
#' aesthetic mappings as `color = ~ species` instead of `color = 'species'`.
#'
#' @param x A value (formula or otherwise).
#' @return A character string if `x` is a one-sided single-term formula;
#'   otherwise `x` unchanged.
#' @noRd
as_var = function(x) {
  if (!inherits(x, 'formula') || length(x) != 2) return(x)
  terms = extract_terms(x[[2]])
  if (length(terms) != 1) stop(
    "Formula '", deparse(x), "' must contain exactly one variable name ",
    "(e.g., `~ var`)"
  )
  terms
}

#' Apply as_var() to Each Element of a List
#' @noRd
as_vars = function(x) lapply(x, as_var)

#' Parse a Formula into Aesthetic and Facet Mappings
#'
#' Interprets an R formula as chart aesthetic mappings:
#' - `y ~ x` maps to `list(x = 'x', y = 'y')`
#' - `~ x` maps to `list(x = 'x')`
#' - `~ x1 + x2 + x3` maps to `list(position = c('x1', 'x2', 'x3'))`
#' - `y ~ x | z` adds faceting by `z` (columns)
#' - `y ~ x | 0 + z` adds faceting by `z` (rows)
#' - `y ~ x | z1 + z2` adds faceting by `z1` (columns) and `z2` (rows)
#'
#' @param f A formula object.
#' @return A list with `aesthetics` (named list) and `facet` (a facet list or
#'   `NULL`).
#' @noRd
parse_formula = function(f) {
  lhs = if (length(f) == 3) f[[2]]
  rhs = if (length(f) == 3) f[[3]] else f[[2]]

  # Extract conditioning (facet) variables from |
  facet_terms = NULL
  if (is.call(rhs) && identical(rhs[[1]], as.name('|'))) {
    facet_terms = extract_terms(rhs[[3]])
    rhs = rhs[[2]]
  }

  rhs_terms = extract_terms(rhs)

  # Build aesthetics
  aesthetics = list()
  if (!is.null(lhs)) aesthetics$y = deparse(lhs)
  if (length(rhs_terms) == 1) {
    aesthetics$x = rhs_terms
  } else if (length(rhs_terms) > 1) {
    aesthetics$position = rhs_terms
  }

  # Build facet: | var → column; | 0 + var → row; | var1 + var2 → both
  facet = NULL
  if (length(facet_terms)) {
    # A leading 0 shifts terms to row (y) instead of column (x)
    nms = if (facet_terms[1] == '0') 'y' else c('x', 'y')
    facet_terms = facet_terms[facet_terms != '0']
    enc = setNames(as.list(facet_terms), head(nms, length(facet_terms)))
    if (length(enc)) facet = list(type = 'facetRect', encode = enc)
  }

  list(aesthetics = aesthetics, facet = facet)
}

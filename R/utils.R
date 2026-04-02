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

#' Parse a Formula into Aesthetic and Facet Mappings
#'
#' Interprets an R formula as chart aesthetic mappings:
#' - `y ~ x` maps to `list(x = 'x', y = 'y')`
#' - `~ x` maps to `list(x = 'x')`
#' - `~ x1 + x2 + x3` maps to `list(position = c('x1', 'x2', 'x3'))`
#' - `y ~ x | z` adds faceting by `z`
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

  # Build facet
  facet = NULL
  if (length(facet_terms)) {
    enc = list()
    if (length(facet_terms) >= 1) enc$x = facet_terms[1]
    if (length(facet_terms) >= 2) enc$y = facet_terms[2]
    facet = list(type = 'facetRect', encode = enc)
  }

  list(aesthetics = aesthetics, facet = facet)
}

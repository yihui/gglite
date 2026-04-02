library(testit)

assert('var_type classifies correctly', {
  (var_type(NULL) %==% 'none')
  (var_type(1:5) %==% 'numeric')
  (var_type(c(1.0, 2.0)) %==% 'numeric')
  (var_type(c('a', 'b')) %==% 'categorical')
  (var_type(factor(c('x', 'y'))) %==% 'categorical')
  (var_type(c(TRUE, FALSE)) %==% 'categorical')
  (var_type(Sys.Date()) %==% 'date')
  (var_type(Sys.time()) %==% 'date')
})

assert('dropNulls removes NULL elements', {
  (dropNulls(list(a = 1, b = NULL, c = 3)) %==% list(a = 1, c = 3))
})

assert('process_layout rejects bad lengths', {
  (has_error(process_layout('padding', c(1, 2))))
})

assert('annotate_df() wraps data frames', {
  x = list(data = data.frame(a = 1:3), children = list(
    list(type = 'point', data = data.frame(b = 4:6))
  ))
  res = annotate_df(x)
  (res$data$type %==% 'column')
  (is.data.frame(res$data$value))
  (res$children[[1]]$data$type %==% 'column')
})

assert('extract_terms extracts + separated names', {
  (extract_terms(quote(x)) %==% 'x')
  (extract_terms(quote(x + y)) %==% c('x', 'y'))
  (extract_terms(quote(x + y + z)) %==% c('x', 'y', 'z'))
})

assert('parse_formula: y ~ x', {
  res = parse_formula(hp ~ mpg)
  (res$aesthetics$x %==% 'mpg')
  (res$aesthetics$y %==% 'hp')
  (is.null(res$facet))
})

assert('parse_formula: ~ x', {
  res = parse_formula(~ mpg)
  (res$aesthetics$x %==% 'mpg')
  (is.null(res$aesthetics$y))
})

assert('parse_formula: ~ x1 + x2 + x3', {
  res = parse_formula(~ a + b + c)
  (res$aesthetics$position %==% c('a', 'b', 'c'))
  (is.null(res$aesthetics$x))
})

assert('parse_formula: y ~ x | z', {
  res = parse_formula(hp ~ mpg | cyl)
  (res$aesthetics$x %==% 'mpg')
  (res$aesthetics$y %==% 'hp')
  (res$facet$type %==% 'facetRect')
  (res$facet$encode$x %==% 'cyl')
})

assert('parse_formula: y ~ x | z1 + z2', {
  res = parse_formula(y ~ x | a + b)
  (res$facet$encode$x %==% 'a')
  (res$facet$encode$y %==% 'b')
})

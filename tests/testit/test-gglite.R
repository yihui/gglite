library(testit)

# g2() creates a g2 object
assert('g2() returns an object of class g2', {
  chart = g2()
  (inherits(chart, 'g2'))
})

# g2() accepts data and inline aesthetics
assert('g2() accepts data and aesthetics', {
  df = data.frame(x = 1:3, y = 4:6)
  chart = g2(df, x = x, y = y)
  (identical(chart$data, df))
  (chart$aesthetics$x == 'x')
  (chart$aesthetics$y == 'y')
})

# encode() maps aesthetics
assert('encode() maps variable names to aesthetics', {
  chart = g2() |> encode(x = foo, y = bar, color = baz)
  (chart$aesthetics$x == 'foo')
  (chart$aesthetics$y == 'bar')
  (chart$aesthetics$color == 'baz')
})

# mark_*() functions add layers
assert('mark_point() and mark_line() add layers', {
  chart = g2() |> mark_point() |> mark_line()
  (length(chart$layers) == 2)
  (chart$layers[[1]]$type == 'point')
  (chart$layers[[2]]$type == 'line')
})

# build_config() produces correct spec (no constructor options)
assert('build_config() produces correct spec', {
  df = data.frame(x = c('A', 'B'), y = c(3, 7))
  chart = g2(df, x = x, y = y) |>
    mark_interval() |>
    scale_of('y', nice = TRUE)
  config = gglite:::build_config(chart)

  # spec should NOT contain constructor options
  (is.null(config$width))
  (is.null(config$height))
  # spec should contain data and marks
  (length(config$data) == 2)
  (config$data[[1]]$x == 'A')
  (config$data[[2]]$y == 7)
  (length(config$children) == 1)
  (config$children[[1]]$type == 'interval')
  (config$children[[1]]$encode$x == 'x')
  (config$children[[1]]$encode$y == 'y')
  (config$scale$y$nice == TRUE)
})

# chart_html() generates valid HTML with G2.Chart constructor args
assert('chart_html() generates correct HTML structure', {
  chart = g2(data.frame(x = 1, y = 2), x = x, y = y) |> mark_point()
  html = chart_html(chart, id = 'test-container')
  (grepl('id="test-container"', html))
  (grepl('G2.Chart(', html, fixed = TRUE))
  (grepl('"container"', html))
  (grepl('test-container', html))
  (grepl('chart.options(', html, fixed = TRUE))
  (grepl('chart.render()', html, fixed = TRUE))
  (grepl('type="module"', html))
})

# knit_print.g2() returns knit_asis HTML with defer
assert('knit_print.g2() returns knit_asis output', {
  chart = g2(data.frame(x = 1, y = 2), x = x, y = y) |> mark_point()
  out = gglite:::knit_print.g2(chart)
  (inherits(out, 'knit_asis'))
  (grepl('defer', out))
  (grepl('G2.Chart(', out, fixed = TRUE))
})

# End-to-end pipe chaining
assert('pipe chaining works end-to-end', {
  chart = g2(mtcars, x = mpg, y = hp) |>
    mark_point() |>
    scale_of('x', type = 'linear') |>
    coordinate('polar') |>
    interact('tooltip')
  (inherits(chart, 'g2'))
  (length(chart$layers) == 1)
  (chart$scales$x$type == 'linear')
  (chart$coords$type == 'polar')
  (length(chart$interactions) == 1)
})

# g2_cdn() is customizable via option
assert('g2_cdn() respects gglite.g2_cdn option', {
  old = getOption('gglite.g2_cdn')
  options(gglite.g2_cdn = 'https://example.com/g2.js')
  res = gglite:::g2_cdn()
  options(gglite.g2_cdn = old)
  (res == 'https://example.com/g2.js')
})

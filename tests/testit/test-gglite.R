library(testit)

# g2() creates a g2 object
assert('g2() returns an object of class g2', {
  chart = g2()
  (inherits(chart, 'g2'))
})

# g2() accepts data and inline aesthetics (character strings)
assert('g2() accepts data and aesthetics', {
  df = data.frame(x = 1:3, y = 4:6)
  chart = g2(df, x = 'x', y = 'y')
  (identical(chart$data, df))
  (chart$aesthetics$x == 'x')
  (chart$aesthetics$y == 'y')
})

# encode() maps aesthetics as character strings
assert('encode() maps column names to aesthetics', {
  chart = g2() |> encode(x = 'foo', y = 'bar', color = 'baz')
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

# build_config() produces correct spec with column-major data
assert('build_config() produces correct spec', {
  df = data.frame(x = c('A', 'B'), y = c(3, 7))
  chart = g2(df, x = 'x', y = 'y') |>
    mark_interval() |>
    scale_of('y', nice = TRUE)
  config = gglite:::build_config(chart)

  # spec should NOT contain constructor options
  (is.null(config$width))
  (is.null(config$height))
  # data should be annotated as column-major
  (config$data$type == 'column')
  (is.data.frame(config$data$value))
  # marks
  (length(config$children) == 1)
  (config$children[[1]]$type == 'interval')
  (config$children[[1]]$encode$x == 'x')
  (config$children[[1]]$encode$y == 'y')
  (config$scale$y$nice == TRUE)
})

# chart_html() generates valid HTML with G2.Chart constructor args
assert('chart_html() generates correct HTML structure', {
  chart = g2(data.frame(x = 1, y = 2), x = 'x', y = 'y') |> mark_point()
  html = chart_html(chart, id = 'test-container')
  (grepl('id="test-container"', html))
  (grepl('G2.Chart(', html, fixed = TRUE))
  (grepl('"container"', html))
  (grepl('test-container', html))
  (grepl('chart.options(', html, fixed = TRUE))
  (grepl('chart.render()', html, fixed = TRUE))
  (grepl('type="module"', html))
  # column-major data annotation
  (grepl('"type"', html))
  (grepl('"column"', html))
})

# transform_of() adds transforms to the last mark
assert('transform_of() adds transforms', {
  chart = g2() |> mark_interval() |> transform_of('stackY')
  (length(chart$layers[[1]]$transform) == 1)
  (chart$layers[[1]]$transform[[1]]$type == 'stackY')
})

# coord_transpose() adds transpose transform
assert('coord_transpose() adds transpose transform', {
  chart = g2() |> mark_interval() |> coord_transpose()
  (length(chart$coords$transform) == 1)
  (chart$coords$transform[[1]]$type == 'transpose')
})

# facet_rect() sets facet config
assert('facet_rect() sets faceting', {
  chart = g2() |> mark_point() |> facet_rect(x = 'Species')
  (chart$facet$type == 'facetRect')
  (chart$facet$encode$x == 'Species')
  config = gglite:::build_config(chart)
  (config$type == 'facetRect')
  (config$encode$x == 'Species')
})

# animate() sets animation on last mark
assert('animate() sets animation options', {
  chart = g2() |> mark_interval() |>
    animate(enter = list(type = 'fadeIn'))
  (chart$layers[[1]]$animate$enter$type == 'fadeIn')
})

# theme_of(), axis_of(), legend_of(), title_of() set chart options
assert('component functions set chart options', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    theme_of('dark') |>
    axis_of('x', title = 'MPG') |>
    legend_of('color', position = 'right') |>
    title_of('Cars')
  (chart$theme$type == 'dark')
  (chart$axes$x$title == 'MPG')
  (chart$legends$color$position == 'right')
  (chart$chart_title == 'Cars')
})

# End-to-end pipe chaining
assert('pipe chaining works end-to-end', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
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

# annotate_df correctly wraps data frames
assert('annotate_df() wraps data frames', {
  x = list(data = data.frame(a = 1:3), children = list(
    list(type = 'point', data = data.frame(b = 4:6))
  ))
  res = gglite:::annotate_df(x)
  (res$data$type == 'column')
  (is.data.frame(res$data$value))
  (res$children[[1]]$data$type == 'column')
})

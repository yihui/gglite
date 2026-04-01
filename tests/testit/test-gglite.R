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
  (chart$aesthetics$x %==% 'x')
  (chart$aesthetics$y %==% 'y')
})

# encode() maps aesthetics as character strings
assert('encode() maps column names to aesthetics', {
  chart = g2() |> encode(x = 'foo', y = 'bar', color = 'baz')
  (chart$aesthetics$x %==% 'foo')
  (chart$aesthetics$y %==% 'bar')
  (chart$aesthetics$color %==% 'baz')
})

# mark_*() functions add layers
assert('mark_point() and mark_line() add layers', {
  chart = g2() |> mark_point() |> mark_line()
  (length(chart$layers) %==% 2L)
  (chart$layers[[1]]$type %==% 'point')
  (chart$layers[[2]]$type %==% 'line')
})

# mark_point() defaults to solid shape; user can override
assert('mark_point() defaults to solid shape and respects user override', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |> mark_point()
  (chart$layers[[1]]$style$shape %==% 'point')
  chart2 = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point(style = list(shape = 'hollow'))
  (chart2$layers[[1]]$style$shape %==% 'hollow')
  chart3 = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point(style = list(opacity = 0.5))
  (chart3$layers[[1]]$style$shape %==% 'point')
  (chart3$layers[[1]]$style$opacity %==% 0.5)
})

# build_config() produces correct spec with column-major data
assert('build_config() produces correct spec', {
  df = data.frame(x = c('A', 'B'), y = c(3, 7))
  chart = g2(df, x = 'x', y = 'y') |>
    mark_interval() |>
    scale_('y', nice = TRUE)
  config = build_config(chart)

  # spec should NOT contain constructor options
  (is.null(config$width))
  (is.null(config$height))
  # data should be annotated as column-major
  (config$data$type %==% 'column')
  (is.data.frame(config$data$value))
  # marks
  (length(config$children) %==% 1L)
  (config$children[[1]]$type %==% 'interval')
  (config$children[[1]]$encode$x %==% 'x')
  (config$children[[1]]$encode$y %==% 'y')
  (config$scale$y$nice %==% TRUE)
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

# transform_() adds transforms to the last mark
assert('transform_() adds transforms', {
  chart = g2() |> mark_interval() |> transform_('stackY')
  (length(chart$layers[[1]]$transform) %==% 1L)
  (chart$layers[[1]]$transform[[1]]$type %==% 'stackY')
})

# coord_transpose() adds transpose transform
assert('coord_transpose() adds transpose transform', {
  chart = g2() |> mark_interval() |> coord_transpose()
  (length(chart$coords$transform) %==% 1L)
  (chart$coords$transform[[1]]$type %==% 'transpose')
})

# facet_rect() sets facet config
assert('facet_rect() sets faceting', {
  chart = g2() |> mark_point() |> facet_rect(x = 'Species')
  (chart$facet$type %==% 'facetRect')
  (chart$facet$encode$x %==% 'Species')
  config = build_config(chart)
  (config$type %==% 'facetRect')
  (config$encode$x %==% 'Species')
})

# animate() sets animation on last mark
assert('animate() sets animation options', {
  chart = g2() |> mark_interval() |>
    animate(enter = list(type = 'fadeIn'))
  (chart$layers[[1]]$animate$enter$type %==% 'fadeIn')
})

# build_config() respects global gglite.theme option
assert('build_config() merges gglite.theme option', {
  old = options(gglite.theme = list(axis = list(labelFontSize = 20)))
  chart = g2(mtcars, x = 'mpg', y = 'hp') |> mark_point()
  config = build_config(chart)
  options(old)
  (config$theme$axis$labelFontSize %==% 20)
})

assert('gglite.theme merges with per-chart theme_()', {
  old = options(gglite.theme = list(axis = list(labelFontSize = 20)))
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    theme_('dark', axis = list(titleFontSize = 18))
  config = build_config(chart)
  options(old)
  # per-chart type + titleFontSize win; labelFontSize from option preserved
  (config$theme$type %==% 'dark')
  (config$theme$axis$titleFontSize %==% 18)
  (config$theme$axis$labelFontSize %==% 20)
})

# theme_(), axis_(), legend_(), title_() set chart options
assert('component functions set chart options', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    theme_('dark') |>
    axis_('x', title = 'MPG') |>
    legend_('color', position = 'right') |>
    title_('Cars')
  (chart$theme$type %==% 'dark')
  (chart$axes$x$title %==% 'MPG')
  (chart$legends$color$position %==% 'right')
  (chart$chart_title %==% 'Cars')
})

# End-to-end pipe chaining
assert('pipe chaining works end-to-end', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    scale_('x', type = 'linear') |>
    coord_polar() |>
    interact('tooltip')
  (inherits(chart, 'g2'))
  (length(chart$layers) %==% 1L)
  (chart$scales$x$type %==% 'linear')
  (chart$coords$type %==% 'polar')
  (length(chart$interactions) %==% 1L)
})

# g2_cdn() is customizable via option
assert('g2_cdn() respects gglite.g2_cdn option', {
  old = getOption('gglite.g2_cdn')
  options(gglite.g2_cdn = 'https://example.com/g2.js')
  res = g2_cdn()
  options(gglite.g2_cdn = old)
  (res %==% 'https://example.com/g2.js')
})

# annotate_df correctly wraps data frames
assert('annotate_df() wraps data frames', {
  x = list(data = data.frame(a = 1:3), children = list(
    list(type = 'point', data = data.frame(b = 4:6))
  ))
  res = annotate_df(x)
  (res$data$type %==% 'column')
  (is.data.frame(res$data$value))
  (res$children[[1]]$data$type %==% 'column')
})

# coord_*() shortcut functions
assert('coord_polar() sets polar coordinate', {
  chart = g2() |> mark_interval() |> coord_polar()
  (chart$coords$type %==% 'polar')
})

assert('coord_theta() sets theta coordinate', {
  chart = g2() |> mark_interval() |> coord_theta(innerRadius = 0.5)
  (chart$coords$type %==% 'theta')
  (chart$coords$innerRadius %==% 0.5)
})

assert('coord_radial() sets radial coordinate', {
  chart = g2() |> mark_interval() |> coord_radial()
  (chart$coords$type %==% 'radial')
})

# theme_*() shortcut functions
assert('theme_classic() sets classic theme', {
  chart = g2() |> mark_point() |> theme_classic()
  (chart$theme$type %==% 'classic')
})

assert('theme_dark() sets dark theme', {
  chart = g2() |> mark_point() |> theme_dark()
  (chart$theme$type %==% 'dark')
})

assert('theme_academy() sets academy theme', {
  chart = g2() |> mark_point() |> theme_academy()
  (chart$theme$type %==% 'academy')
})

# g2() layout arguments (padding, margin, inset)
assert('g2() padding scalar sets layout', {
  chart = g2(padding = 20)
  (chart$layout$padding %==% 20)
})

assert('g2() padding vector sets layout sides', {
  chart = g2(padding = c(30, NA, NA, 10))
  (chart$layout$paddingTop %==% 30)
  (chart$layout$paddingLeft %==% 10)
  (is.null(chart$layout$paddingRight))
  (is.null(chart$layout$paddingBottom))
})

assert('g2() margin and inset work', {
  chart = g2(margin = 16, inset = c(5, 10, 5, 10))
  (chart$layout$margin %==% 16)
  (chart$layout$insetTop %==% 5)
  (chart$layout$insetRight %==% 10)
})

assert('process_layout rejects bad lengths', {
  (has_error(process_layout('padding', c(1, 2))))
})

# Helper wrapper functions
assert('scale_x() is shortcut for scale_(x)', {
  chart = g2() |> mark_point() |> scale_x(type = 'log')
  (chart$scales$x$type %==% 'log')
})

assert('axis_x() and axis_y() are shortcuts', {
  chart = g2() |> mark_point() |>
    axis_x(title = 'X') |> axis_y(title = 'Y')
  (chart$axes$x$title %==% 'X')
  (chart$axes$y$title %==% 'Y')
})

assert('legend_color() is shortcut for legend_(color)', {
  chart = g2() |> mark_point() |> legend_color(position = 'right')
  (chart$legends$color$position %==% 'right')
})

assert('slider_x() and scrollbar_y() are shortcuts', {
  chart = g2() |> mark_point() |> slider_x() |> scrollbar_y()
  (isTRUE(chart$sliders$x))
  (isTRUE(chart$scrollbars$y))
})

# build_config() auto-sets shape scale range to fix legend mismatch
assert('build_config() sets shape scale range when shape maps to column', {
  chart = g2(iris, x = 'Sepal.Width', y = 'Sepal.Length',
    shape = 'Species') |> mark_point()
  config = build_config(chart)
  # 3 unique Species -> range of length 3
  (length(config$scale$shape$range) %==% 3L)
  (config$scale$shape$range %==% c('point', 'plus', 'diamond'))
})

assert('build_config() does not override user-set shape range', {
  chart = g2(iris, x = 'Sepal.Width', y = 'Sepal.Length',
    shape = 'Species') |> mark_point() |>
    scale_shape(range = c('circle', 'square', 'triangle'))
  config = build_config(chart)
  (config$scale$shape$range %==% c('circle', 'square', 'triangle'))
})

assert('build_config includes layout options', {
  chart = g2(data.frame(x = 1, y = 2), x = 'x', y = 'y',
    padding = 20, inset = c(5, NA, 5, NA)) |>
    mark_point()
  config = build_config(chart)
  (config$padding %==% 20)
  (config$insetTop %==% 5)
  (config$insetBottom %==% 5)
  (is.null(config$insetRight))
})

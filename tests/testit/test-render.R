library(testit)

assert('build_config() produces correct spec', {
  df = data.frame(x = c('A', 'B'), y = c(3, 7))
  chart = g2(df, x = 'x', y = 'y') |>
    mark_interval() |>
    scale_('y', nice = TRUE)
  config = build_config(chart)
  (is.null(config$width))
  (is.null(config$height))
  (config$data$type %==% 'column')
  (is.data.frame(config$data$value))
  (length(config$children) %==% 1L)
  (config$children[[1]]$type %==% 'interval')
  (config$children[[1]]$encode$x %==% 'x')
  (config$children[[1]]$encode$y %==% 'y')
  # scale after mark goes to mark level in the spec
  (config$children[[1]]$scale$y$nice %==% TRUE)
})

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
  (grepl('"type"', html))
  (grepl('"column"', html))
})

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
  (config$theme$type %==% 'dark')
  (config$theme$axis$titleFontSize %==% 18)
  (config$theme$axis$labelFontSize %==% 20)
})

assert('build_config includes layout options', {
  chart = g2(data.frame(x = 1, y = 2), x = 'x', y = 'y') |>
    canvas(padding = 20, inset = c(5, NA, 5, NA)) |>
    mark_point()
  config = build_config(chart)
  (config$padding %==% 20)
  (config$insetTop %==% 5)
  (config$insetBottom %==% 5)
  (is.null(config$insetRight))
})

# ---- canvas() and renderer ----

assert('canvas() sets dimensions in options', {
  chart = g2() |> canvas(width = 600, height = 400)
  (chart$options$width %==% 600)
  (chart$options$height %==% 400)
  (is.null(chart$options$autoFit))
})

assert('canvas() default height and autoFit when width is NULL', {
  chart = g2() |> canvas()
  (chart$options$height %==% 480)
  (chart$options$autoFit %==% TRUE)
  (is.null(chart$options$width))
})

assert('canvas() sets renderer (case-insensitive)', {
  chart = g2() |> canvas(renderer = 'SVG')
  (chart$renderer %==% 'svg')
  chart2 = g2() |> canvas(renderer = 'WebGL')
  (chart2$renderer %==% 'webgl')
})

assert('canvas() passes extra args to canvas_extra', {
  chart = g2() |> canvas(clip = TRUE)
  (isTRUE(chart$canvas_extra$clip))
})

assert('chart_html() injects renderer JS for svg', {
  chart = g2() |> canvas(renderer = 'svg') |> mark_point()
  html = chart_html(chart)
  (grepl('window.G.SVG.Renderer', html, fixed = TRUE))
})

assert('chart_html() injects renderer JS for webgl', {
  chart = g2() |> canvas(renderer = 'webgl') |> mark_point()
  html = chart_html(chart)
  (grepl('window.G.WebGL.Renderer', html, fixed = TRUE))
})

assert('chart_html() no renderer JS for default canvas', {
  chart = g2() |> mark_point()
  html = chart_html(chart)
  (!grepl('window.G', html, fixed = TRUE))
})

assert('effective_renderer() defaults to canvas', {
  chart = g2()
  (effective_renderer(chart) %==% 'canvas')
})

assert('effective_renderer() respects per-chart setting', {
  chart = g2() |> canvas(renderer = 'webgl')
  (effective_renderer(chart) %==% 'webgl')
})

assert('effective_renderer() respects global option', {
  old = options(gglite.renderer = 'svg')
  chart = g2()
  r = effective_renderer(chart)
  options(old)
  (r %==% 'svg')
})

assert('effective_renderer() per-chart overrides global', {
  old = options(gglite.renderer = 'svg')
  chart = g2() |> canvas(renderer = 'webgl')
  r = effective_renderer(chart)
  options(old)
  (r %==% 'webgl')
})

assert('cdn_scripts() returns g2.min.js for canvas', {
  scripts = cdn_scripts(g2())
  (any(grepl('g2.min.js', scripts, fixed = TRUE)))
  (!any(grepl('g2.lite.min.js', scripts, fixed = TRUE)))
})

assert('cdn_scripts() returns g2.lite.min.js for svg', {
  scripts = cdn_scripts(g2() |> canvas(renderer = 'svg'))
  (any(grepl('g2.lite.min.js', scripts, fixed = TRUE)))
  (any(grepl('@antv/g-svg', scripts, fixed = TRUE)))
  (!any(grepl('g2.min.js"', scripts, fixed = TRUE)))
})

assert('cdn_scripts() returns g2.lite.min.js when global renderer is set', {
  old = options(gglite.renderer = 'canvas')
  scripts = cdn_scripts(g2())
  options(old)
  (any(grepl('g2.lite.min.js', scripts, fixed = TRUE)))
  (any(grepl('@antv/g-canvas', scripts, fixed = TRUE)))
})

assert('build_config includes canvas_extra options', {
  chart = g2(data.frame(x = 1, y = 2), x = 'x', y = 'y') |>
    canvas(clip = TRUE) |>
    mark_point()
  config = build_config(chart)
  (isTRUE(config$clip))
})

# ---- Auto mark detection ----

assert('auto_mark returns NULL for non-data-frame or missing aesthetics', {
  (is.null(auto_mark(NULL, list(x = 'a'))))
  (is.null(auto_mark(mtcars, list())))
})

assert('auto_mark: numeric x + numeric y -> point', {
  res = auto_mark(mtcars, list(x = 'mpg', y = 'hp'))
  (res$marks[[1]]$type %==% 'point')
  (is.null(res$coord))
})

assert('auto_mark: categorical x + numeric y, unique -> interval', {
  df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
  res = auto_mark(df, list(x = 'x', y = 'y'))
  (length(res$marks) %==% 1L)
  (res$marks[[1]]$type %==% 'interval')
})

assert('auto_mark: categorical x + numeric y, repeated -> beeswarm', {
  res = auto_mark(iris, list(x = 'Species', y = 'Sepal.Width'))
  (res$marks[[1]]$type %==% 'beeswarm')
  (is.null(res$coord))
})

assert('auto_mark: categorical x + numeric y, large groups -> beeswarm + density', {
  df = data.frame(x = rep(c('A', 'B'), each = 30), y = rnorm(60))
  res = auto_mark(df, list(x = 'x', y = 'y'))
  (length(res$marks) %==% 2L)
  (res$marks[[1]]$type %==% 'beeswarm')
  (res$marks[[2]]$type %==% 'density')
  (!is.null(res$marks[[2]]$data$transform))
})

assert('auto_mark: numeric x + categorical y -> transpose', {
  res = auto_mark(iris, list(x = 'Sepal.Width', y = 'Species'))
  (res$marks[[1]]$type %==% 'beeswarm')
  (res$coord$transform[[1]]$type %==% 'transpose')
})

assert('auto_mark: categorical x + categorical y -> cell with group count', {
  df = data.frame(a = c('x', 'y'), b = c('m', 'n'))
  res = auto_mark(df, list(x = 'a', y = 'b'))
  (res$marks[[1]]$type %==% 'cell')
  (res$marks[[1]]$encode$color %==% 'count')
  (res$marks[[1]]$transform[[1]]$type %==% 'group')
})

assert('auto_mark: cat x + cat y with existing color -> plain cell', {
  df = data.frame(a = c('x', 'y'), b = c('m', 'n'))
  res = auto_mark(df, list(x = 'a', y = 'b', color = 'a'))
  (res$marks[[1]]$type %==% 'cell')
  (is.null(res$marks[[1]]$encode))
  (is.null(res$marks[[1]]$transform))
})

assert('auto_mark: date x + numeric y -> line', {
  df = data.frame(d = Sys.Date() + 1:5, v = 1:5)
  res = auto_mark(df, list(x = 'd', y = 'v'))
  (res$marks[[1]]$type %==% 'line')
})

assert('auto_mark: numeric x + no y -> rect with binX and white stroke', {
  res = auto_mark(mtcars, list(x = 'mpg'))
  (res$marks[[1]]$type %==% 'rect')
  (res$marks[[1]]$transform[[1]]$type %==% 'binX')
  (res$marks[[1]]$style$stroke %==% 'white')
})

assert('auto_mark: categorical x + no y -> interval with groupX', {
  res = auto_mark(iris, list(x = 'Species'))
  (res$marks[[1]]$type %==% 'interval')
  (res$marks[[1]]$transform[[1]]$type %==% 'groupX')
})

assert('auto_mark: position encoding -> line + parallel', {
  res = auto_mark(iris, list(position = names(iris)[1:4]))
  (res$marks[[1]]$type %==% 'line')
  (res$coord$type %==% 'parallel')
})

assert('build_config uses auto mark when no layers are set', {
  chart = g2(mtcars, x = 'mpg', y = 'hp')
  config = build_config(chart)
  (length(config$children) %==% 1L)
  (config$children[[1]]$type %==% 'point')
  (config$children[[1]]$encode$x %==% 'mpg')
})

assert('build_config auto mark does not override explicit layers', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |> mark_line()
  config = build_config(chart)
  (length(config$children) %==% 1L)
  (config$children[[1]]$type %==% 'line')
})

assert('build_config auto mark adds transpose for numeric x + cat y', {
  chart = g2(iris, x = 'Sepal.Width', y = 'Species')
  config = build_config(chart)
  (config$children[[1]]$type %==% 'beeswarm')
  (config$coordinate$transform[[1]]$type %==% 'transpose')
  (config$children[[1]]$encode$x %==% 'Species')
  (config$children[[1]]$encode$y %==% 'Sepal.Width')
})

assert('auto_mark: ts numeric x + numeric y -> line', {
  df = data.frame(time = 1:10, value = rnorm(10))
  res = auto_mark(df, list(x = 'time', y = 'value'), ts = TRUE)
  (res$marks[[1]]$type %==% 'line')
  (is.null(res$coord))
})

assert('auto_mark: ts = FALSE still gives point for numeric + numeric', {
  df = data.frame(time = 1:10, value = rnorm(10))
  res = auto_mark(df, list(x = 'time', y = 'value'), ts = FALSE)
  (res$marks[[1]]$type %==% 'point')
})

assert('build_config auto-detects line for univariate ts', {
  chart = g2(sunspot.year)
  config = build_config(chart)
  (length(config$children) %==% 1L)
  (config$children[[1]]$type %==% 'line')
  (config$children[[1]]$encode$x %==% 'time')
  (config$children[[1]]$encode$y %==% 'value')
})

assert('build_config auto-detects line for multivariate ts', {
  chart = g2(EuStockMarkets)
  config = build_config(chart)
  (length(config$children) %==% 1L)
  (config$children[[1]]$type %==% 'line')
  (config$children[[1]]$encode$x %==% 'time')
  (config$children[[1]]$encode$y %==% 'value')
  (config$children[[1]]$encode$color %==% 'series')
})

assert('g2() ts conversion sets default aesthetics', {
  chart = g2(sunspot.year)
  (is.data.frame(chart$data))
  (chart$aesthetics$x %==% 'time')
  (chart$aesthetics$y %==% 'value')
  (isTRUE(chart$ts_origin))
})

assert('g2() ts aesthetics can be overridden', {
  chart = g2(sunspot.year, size = 'value')
  (chart$aesthetics$x %==% 'time')
  (chart$aesthetics$y %==% 'value')
  (chart$aesthetics$size %==% 'value')
})

assert('build_config sets default y-axis title from ts name', {
  chart = g2(sunspot.year)
  config = build_config(chart)
  (config$axis$y$title %==% 'sunspot.year')
})

assert('build_config ts y-axis title not set for non-ts data', {
  chart = g2(mtcars, x = 'mpg', y = 'hp')
  config = build_config(chart)
  (is.null(config$axis$y$title))
})

assert('build_config ts y-axis title does not override user title', {
  chart = g2(sunspot.year) |> axis_y(title = 'My Title')
  config = build_config(chart)
  (config$axis$y$title %==% 'My Title')
})

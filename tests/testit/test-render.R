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
  (config$scale$y$nice %==% TRUE)
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
  chart = g2(data.frame(x = 1, y = 2), x = 'x', y = 'y',
    padding = 20, inset = c(5, NA, 5, NA)) |>
    mark_point()
  config = build_config(chart)
  (config$padding %==% 20)
  (config$insetTop %==% 5)
  (config$insetBottom %==% 5)
  (is.null(config$insetRight))
})

# ---- Auto mark detection ----

assert('auto_mark returns NULL for non-data-frame or missing aesthetics', {
  (is.null(auto_mark(NULL, list(x = 'a'))))
  (is.null(auto_mark(mtcars, list())))
})

assert('auto_mark: numeric x + numeric y -> point', {
  res = auto_mark(mtcars, list(x = 'mpg', y = 'hp'))
  (res$mark$type %==% 'point')
  (is.null(res$coord))
})

assert('auto_mark: categorical x + numeric y -> boxplot', {
  res = auto_mark(iris, list(x = 'Species', y = 'Sepal.Width'))
  (res$mark$type %==% 'boxplot')
  (is.null(res$coord))
})

assert('auto_mark: numeric x + categorical y -> boxplot + transpose', {
  res = auto_mark(iris, list(x = 'Sepal.Width', y = 'Species'))
  (res$mark$type %==% 'boxplot')
  (res$coord$transform[[1]]$type %==% 'transpose')
})

assert('auto_mark: categorical x + categorical y -> cell with group count', {
  df = data.frame(a = c('x', 'y'), b = c('m', 'n'))
  res = auto_mark(df, list(x = 'a', y = 'b'))
  (res$mark$type %==% 'cell')
  (res$mark$encode$color %==% 'count')
  (res$mark$transform[[1]]$type %==% 'group')
})

assert('auto_mark: cat x + cat y with existing color -> plain cell', {
  df = data.frame(a = c('x', 'y'), b = c('m', 'n'))
  res = auto_mark(df, list(x = 'a', y = 'b', color = 'a'))
  (res$mark$type %==% 'cell')
  (is.null(res$mark$encode))
  (is.null(res$mark$transform))
})

assert('auto_mark: date x + numeric y -> line', {
  df = data.frame(d = Sys.Date() + 1:5, v = 1:5)
  res = auto_mark(df, list(x = 'd', y = 'v'))
  (res$mark$type %==% 'line')
})

assert('auto_mark: numeric x + no y -> interval with binX', {
  res = auto_mark(mtcars, list(x = 'mpg'))
  (res$mark$type %==% 'interval')
  (res$mark$transform[[1]]$type %==% 'binX')
})

assert('auto_mark: categorical x + no y -> interval with groupX', {
  res = auto_mark(iris, list(x = 'Species'))
  (res$mark$type %==% 'interval')
  (res$mark$transform[[1]]$type %==% 'groupX')
})

assert('auto_mark: position encoding -> line + parallel', {
  res = auto_mark(iris, list(position = names(iris)[1:4]))
  (res$mark$type %==% 'line')
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
  (config$children[[1]]$type %==% 'boxplot')
  (config$coordinate$transform[[1]]$type %==% 'transpose')
  (config$children[[1]]$encode$x %==% 'Species')
  (config$children[[1]]$encode$y %==% 'Sepal.Width')
})

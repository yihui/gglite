library(testit)

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

assert('theme_classic_dark() sets classicDark theme', {
  chart = g2() |> mark_point() |> theme_classic_dark()
  (chart$theme$type %==% 'classicDark')
})

assert('dark theme auto-sets viewFill in build_config', {
  chart = g2(mtcars, hp ~ mpg) |> theme_dark()
  config = build_config(chart)
  (config$theme$view$viewFill %==% '#141414')
})

assert('classicDark theme auto-sets viewFill in build_config', {
  chart = g2(mtcars, hp ~ mpg) |> theme_classic_dark()
  config = build_config(chart)
  (config$theme$view$viewFill %==% '#141414')
})

assert('light theme does not set viewFill', {
  chart = g2(mtcars, hp ~ mpg) |> theme_light()
  config = build_config(chart)
  (is.null(config$theme$view))
})

assert('dark theme propagates to facet children', {
  chart = g2(mtcars, hp ~ mpg | cyl) |> theme_dark()
  config = build_config(chart)
  (config$children[[1]]$theme$type %==% 'dark')
  (config$children[[1]]$theme$view$viewFill %==% '#141414')
})

assert('classicDark theme propagates to facet children', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width | Species) |> theme_classic_dark()
  config = build_config(chart)
  (config$children[[1]]$theme$type %==% 'classicDark')
  (config$children[[1]]$theme$view$viewFill %==% '#141414')
})

assert('light theme does not propagate viewFill to facet children', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width | Species) |> theme_light()
  config = build_config(chart)
  (is.null(config$children[[1]]$theme$view))
})

assert('dark theme injects border colors on facet children', {
  chart = g2(mtcars, hp ~ mpg | cyl) |> theme_dark()
  config = build_config(chart)
  (config$children[[1]]$style$mainStroke %==% 'rgba(255,255,255,0.25)')
  (config$children[[1]]$style$viewStroke %==% 'rgba(255,255,255,0.25)')
})

assert('classicDark theme injects border colors on facet children', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width | Species) |> theme_classic_dark()
  config = build_config(chart)
  (config$children[[1]]$style$mainStroke %==% 'rgba(255,255,255,0.25)')
  (config$children[[1]]$style$viewStroke %==% 'rgba(255,255,255,0.25)')
})

assert('light theme does not inject border colors on facet children', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width | Species)
  config = build_config(chart)
  (is.null(config$children[[1]]$style$mainStroke))
  (is.null(config$children[[1]]$style$viewStroke))
})

assert('user-provided mainStroke is not overwritten by dark theme', {
  chart = g2(mtcars, hp ~ mpg | cyl) |> theme_dark() |>
    mark_point(style = list(mainStroke = '#ff0000'))
  config = build_config(chart)
  (config$children[[1]]$style$mainStroke %==% '#ff0000')
})


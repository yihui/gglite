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

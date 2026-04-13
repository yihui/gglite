library(testit)

assert('component functions set chart options', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    theme_('dark') |>
    axis_('x', title = 'MPG') |>
    legend_('color', position = 'right') |>
    header('Cars')
  (chart$theme$type %==% 'dark')
  # axis after mark goes to mark level
  (chart$layers[[1]]$axis$x$title %==% 'MPG')
  (chart$legends$color$position %==% 'right')
  (chart$chart_title %==% 'Cars')
})

assert('axis_x() and axis_y() are shortcuts', {
  chart = g2() |> mark_point() |>
    axis_x(title = 'X') |> axis_y(title = 'Y')
  # after mark, axis goes to mark level
  (chart$layers[[1]]$axis$x$title %==% 'X')
  (chart$layers[[1]]$axis$y$title %==% 'Y')
})

assert('legend_color() is shortcut for legend_(color)', {
  chart = g2() |> mark_point() |> legend_color(position = 'right')
  (chart$legends$color$position %==% 'right')
})

assert('slider_x() and scroll_y() are shortcuts', {
  chart = g2() |> mark_point() |> slider_x() |> scroll_y()
  (isTRUE(chart$sliders$x))
  (isTRUE(chart$scrollbars$y))
})

assert('labels() S3 dispatch works with |>', {
  df = data.frame(x = c('A', 'B'), y = c(3, 7))
  chart = g2(df, y ~ x) |> mark_interval() |> labels(text = ~ y)
  (length(chart$layers[[1]]$labels) %==% 1L)
})

assert('labels.g2() deferred modifier works with +', {
  df = data.frame(x = c('A', 'B'), y = c(3, 7))
  chart = g2(df, y ~ x) |> mark_interval()
  result = chart + labels.g2(text = ~ y)
  (length(result$layers[[1]]$labels) %==% 1L)
})

assert('style_view() sets theme view fills', {
  chart = g2() |> style_view(padding = '#ffc9c9', plot = '#a5d8ff')
  (chart$theme$view$viewFill %==% '#ffc9c9')
  (chart$theme$view$plotFill %==% '#a5d8ff')
})

assert('style_view() sets container bg', {
  chart = g2() |> style_view(margin = '#fff3bf')
  (chart$bg %==% '#fff3bf')
})


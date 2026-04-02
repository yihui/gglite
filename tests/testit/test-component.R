library(testit)

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

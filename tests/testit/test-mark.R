library(testit)

assert('mark_point() and mark_line() add layers', {
  chart = g2() |> mark_point() |> mark_line()
  (length(chart$layers) %==% 2L)
  (chart$layers[[1]]$type %==% 'point')
  (chart$layers[[2]]$type %==% 'line')
})

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

library(testit)

assert('scale_x() is shortcut for scale_(x)', {
  chart = g2() |> mark_point() |> scale_x(type = 'log')
  (chart$scales$x$type %==% 'log')
})

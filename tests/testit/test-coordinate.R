library(testit)

assert('coord_transpose() adds transpose transform', {
  chart = g2() |> mark_interval() |> coord_transpose()
  (length(chart$coords$transform) %==% 1L)
  (chart$coords$transform[[1]]$type %==% 'transpose')
})

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

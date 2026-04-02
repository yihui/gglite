library(testit)

assert('animate() sets animation options', {
  chart = g2() |> mark_interval() |>
    animate(enter = list(type = 'fadeIn'))
  (chart$layers[[1]]$animate$enter$type %==% 'fadeIn')
})

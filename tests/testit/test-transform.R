library(testit)

assert('transform_() adds transforms', {
  chart = g2() |> mark_interval() |> transform_('stackY')
  (length(chart$layers[[1]]$transform) %==% 1L)
  (chart$layers[[1]]$transform[[1]]$type %==% 'stackY')
})

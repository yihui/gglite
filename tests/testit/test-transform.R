library(testit)

assert('transform() adds transforms to g2 chart', {
  chart = g2() |> mark_interval() |> transform('stackY')
  (length(chart$layers[[1]]$transform) %==% 1L)
  (chart$layers[[1]]$transform[[1]]$type %==% 'stackY')
})

assert('transform() with + operator works', {
  chart = g2() + mark_interval() + transform('stackY')
  (length(chart$layers[[1]]$transform) %==% 1L)
  (chart$layers[[1]]$transform[[1]]$type %==% 'stackY')
})

assert('transform() dispatches to base R for data frames', {
  df = data.frame(x = 1:3, y = 4:6)
  result = transform(df, z = x + y)
  (inherits(result, 'data.frame'))
  (result$z %==% c(5L, 7L, 9L))
})

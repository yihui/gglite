library(testit)

assert('transform() S3 dispatch works', {
  chart = g2() |> mark_interval() |> transform('dodgeX')
  (chart$layers[[1]]$transform[[1]]$type %==% 'dodgeX')
})

assert('transform.g2() deferred modifier works with +', {
  chart = g2() |> mark_interval()
  result = chart + transform.g2('stackY')
  (result$layers[[1]]$transform[[1]]$type %==% 'stackY')
})

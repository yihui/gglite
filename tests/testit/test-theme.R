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

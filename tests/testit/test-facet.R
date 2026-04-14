library(testit)

assert('facet_rect() sets faceting', {
  chart = g2() |> mark_point() |> facet_rect(x = 'Species')
  (chart$facet$type %==% 'facetRect')
  (chart$facet$encode$x %==% 'Species')
  config = build_config(chart)
  (config$type %==% 'facetRect')
  (config$encode$x %==% 'Species')
})

assert('facet_rect() unnamed one-sided formula sets x', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width) |>
    facet_rect(~ Species)
  (chart$facet$encode$x %==% 'Species')
  (is.null(chart$facet$encode$y))
})

assert('facet_rect() unnamed two-sided formula y ~ x sets x and y', {
  df = data.frame(x = 1, y = 1, sex = 'M', species = 'A')
  chart = g2(df, y ~ x) |> facet_rect(sex ~ species)
  (chart$facet$encode$x %==% 'species')
  (chart$facet$encode$y %==% 'sex')
})

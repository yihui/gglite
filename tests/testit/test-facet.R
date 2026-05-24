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

assert('row_facet_height() returns nrows * 200 for row-faceted charts', {
  df = data.frame(x = 1:6, y = 1:6, g = rep(c('a', 'b', 'c'), 2))
  chart = g2(df, y ~ x) |> facet_rect(y = 'g') |> mark_point()
  (row_facet_height(chart) %==% 600L)
})

assert('row_facet_height() returns NULL for column-only facets', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width) |> facet_rect(~ Species)
  (is.null(row_facet_height(chart)))
})

assert('row_facet_height() returns NULL when no data', {
  chart = g2() |> mark_point() |> facet_rect(y = 'grp')
  (is.null(row_facet_height(chart)))
})

assert('row_facet_height() returns NULL for single row value', {
  df = data.frame(x = 1:3, y = 1:3, g = rep('a', 3))
  chart = g2(df, y ~ x) |> facet_rect(y = 'g') |> mark_point()
  (is.null(row_facet_height(chart)))
})

assert('chart_html() auto-scales height for row-faceted charts', {
  df = data.frame(x = 1:4, y = 1:4, g = rep(c('a', 'b'), 2))
  chart = g2(df, y ~ x) |> facet_rect(y = 'g') |> mark_point()
  html = chart_html(chart)
  (grepl('"height": 400', html, fixed = TRUE))
})

assert('chart_html() respects explicit canvas() height over row facet auto-scaling', {
  df = data.frame(x = 1:4, y = 1:4, g = rep(c('a', 'b'), 2))
  chart = g2(df, y ~ x) |> canvas(height = 300) |>
    facet_rect(y = 'g') |> mark_point()
  html = chart_html(chart)
  (grepl('"height": 300', html, fixed = TRUE))
  (!grepl('"height": 400', html, fixed = TRUE))
})

library(testit)

assert('g2() returns an object of class g2', {
  chart = g2()
  (inherits(chart, 'g2'))
})

assert('g2() accepts data and aesthetics', {
  df = data.frame(x = 1:3, y = 4:6)
  chart = g2(df, x = 'x', y = 'y')
  (identical(chart$data, df))
  (chart$aesthetics$x %==% 'x')
  (chart$aesthetics$y %==% 'y')
})

assert('encode() maps column names to aesthetics', {
  chart = g2() |> encode(x = 'foo', y = 'bar', color = 'baz')
  (chart$aesthetics$x %==% 'foo')
  (chart$aesthetics$y %==% 'bar')
  (chart$aesthetics$color %==% 'baz')
})

assert('pipe chaining works end-to-end', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    scale_('x', type = 'linear') |>
    coord_polar() |>
    interact('tooltip')
  (inherits(chart, 'g2'))
  (length(chart$layers) %==% 1L)
  (chart$scales$x$type %==% 'linear')
  (chart$coords$type %==% 'polar')
  (length(chart$interactions) %==% 1L)
})

assert('g2_cdn() respects gglite.g2_cdn option', {
  old = getOption('gglite.g2_cdn')
  options(gglite.g2_cdn = 'https://example.com/g2.js')
  res = g2_cdn()
  options(gglite.g2_cdn = old)
  (res %==% 'https://example.com/g2.js')
})

assert('g2() padding scalar sets layout', {
  chart = g2(padding = 20)
  (chart$layout$padding %==% 20)
})

assert('g2() padding vector sets layout sides', {
  chart = g2(padding = c(30, NA, NA, 10))
  (chart$layout$paddingTop %==% 30)
  (chart$layout$paddingLeft %==% 10)
  (is.null(chart$layout$paddingRight))
  (is.null(chart$layout$paddingBottom))
})

assert('g2() margin and inset work', {
  chart = g2(margin = 16, inset = c(5, 10, 5, 10))
  (chart$layout$margin %==% 16)
  (chart$layout$insetTop %==% 5)
  (chart$layout$insetRight %==% 10)
})

# ---- Formula interface ----

assert('g2() formula y ~ x sets x and y', {
  chart = g2(mtcars, hp ~ mpg)
  (chart$aesthetics$x %==% 'mpg')
  (chart$aesthetics$y %==% 'hp')
})

assert('g2() formula ~ x sets x only', {
  chart = g2(mtcars, ~ mpg)
  (chart$aesthetics$x %==% 'mpg')
  (is.null(chart$aesthetics$y))
})

assert('g2() formula with extra aesthetics', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width, color = 'Species')
  (chart$aesthetics$x %==% 'Sepal.Width')
  (chart$aesthetics$y %==% 'Sepal.Length')
  (chart$aesthetics$color %==% 'Species')
})

assert('g2() formula with faceting y ~ x | z', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width | Species)
  (chart$aesthetics$x %==% 'Sepal.Width')
  (chart$aesthetics$y %==% 'Sepal.Length')
  (chart$facet$type %==% 'facetRect')
  (chart$facet$encode$x %==% 'Species')
})

assert('g2() formula with two facet variables', {
  df = data.frame(x = 1, y = 2, a = 'A', b = 'B')
  chart = g2(df, y ~ x | a + b)
  (chart$facet$encode$x %==% 'a')
  (chart$facet$encode$y %==% 'b')
})

assert('g2() formula ~ x1 + x2 + x3 sets position encoding', {
  chart = g2(iris, ~ Sepal.Length + Sepal.Width + Petal.Length)
  (chart$aesthetics$position %==%
    c('Sepal.Length', 'Sepal.Width', 'Petal.Length'))
  (is.null(chart$aesthetics$x))
})

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

assert('encode() accepts formula aesthetics', {
  chart = g2() |> encode(x = ~ mpg, y = ~ hp, color = ~ cyl)
  (chart$aesthetics$x %==% 'mpg')
  (chart$aesthetics$y %==% 'hp')
  (chart$aesthetics$color %==% 'cyl')
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

assert('g2() formula aesthetics: color = ~ Species', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species)
  (chart$aesthetics$x %==% 'Sepal.Width')
  (chart$aesthetics$y %==% 'Sepal.Length')
  (chart$aesthetics$color %==% 'Species')
})

assert('g2() all named formula aesthetics (no positional formula)', {
  chart = g2(mtcars, x = ~ mpg, y = ~ hp, color = ~ cyl)
  (chart$aesthetics$x %==% 'mpg')
  (chart$aesthetics$y %==% 'hp')
  (chart$aesthetics$color %==% 'cyl')
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

assert('as_var() rejects multi-term one-sided formula', {
  (has_error(g2(mtcars, hp ~ mpg, color = ~ a + b)))
})

assert('g2() title argument sets chart title', {
  chart = g2(mtcars, x = 'mpg', y = 'hp', title = 'My Title')
  (chart$chart_title$title %==% 'My Title')
})

assert('g2() title and subtitle arguments set title and subtitle', {
  chart = g2(mtcars, x = 'mpg', y = 'hp',
    title = 'Title', subtitle = 'Subtitle')
  (chart$chart_title$title %==% 'Title')
  (chart$chart_title$subtitle %==% 'Subtitle')
})

# ---- interact() uses named list (object) format ----

assert('interact() stores interaction as named list', {
  chart = g2() |> interact('legendHighlight')
  (is.list(chart$interactions))
  (!is.null(names(chart$interactions)))
  (chart$interactions$legendHighlight %==% TRUE)
})

assert('interact() with options stores them correctly', {
  chart = g2() |> interact('tooltip', shared = TRUE)
  (is.list(chart$interactions$tooltip))
  (chart$interactions$tooltip$shared %==% TRUE)
})

assert('multiple interact() calls merge into one named list', {
  chart = g2() |> interact('tooltip') |>
    interact('legendFilter') |> interact('brushHighlight')
  (length(chart$interactions) %==% 3L)
  (chart$interactions$tooltip %==% TRUE)
  (chart$interactions$legendFilter %==% TRUE)
  (chart$interactions$brushHighlight %==% TRUE)
})

assert('build_config injects inactive state for legendHighlight', {
  chart = g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Species') |>
    interact('legendHighlight')
  config = build_config(chart)
  (config$children[[1]]$state$inactive$opacity %==% 0.5)
})

assert('build_config does not overwrite user-defined inactive state', {
  chart = g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Species') |>
    mark_point(state = list(inactive = list(opacity = 0.3))) |>
    interact('legendHighlight')
  config = build_config(chart)
  (config$children[[1]]$state$inactive$opacity %==% 0.3)
})

# ---- legend title fix ----

assert('legend_ transforms title string to showTitle + titleText', {
  chart = g2() |> legend_color(position = 'right', title = 'My Title')
  leg = chart$legends$color
  (leg$showTitle %==% TRUE)
  (leg$titleText %==% 'My Title')
})

assert('legend_ without title does not set showTitle', {
  chart = g2() |> legend_color(position = 'right')
  (is.null(chart$legends$color$showTitle))
})

# ---- Date/POSIXt conversion ----

assert('annotate_df converts Date columns to millisecond timestamps', {
  df = data.frame(d = as.Date('2024-01-01'), v = 1)
  config = list(data = df)
  res = annotate_df(config)
  (res$data$type %==% 'column')
  (is.numeric(res$data$value$d))
  # 2024-01-01 UTC = 1704067200000 ms
  (res$data$value$d %==% (as.numeric(as.Date('2024-01-01')) * 86400000))
})

assert('annotate_df converts POSIXt columns to millisecond timestamps', {
  t = as.POSIXct('2024-01-01 12:00:00', tz = 'UTC')
  df = data.frame(t = t, v = 1)
  config = list(data = df)
  res = annotate_df(config)
  (is.numeric(res$data$value$t))
  (res$data$value$t %==% (as.numeric(t) * 1000))
})

# ---- Auto time scale detection ----

assert('build_config auto-sets time scale for Date columns', {
  df = data.frame(d = Sys.Date() + 0:2, v = 1:3)
  chart = g2(df, x = 'd', y = 'v')
  config = build_config(chart)
  (config$scale$x$type %==% 'time')
})

# ---- Auto mark inference for categorical vs numeric ----

assert('auto_mark: unique categories produce bar chart (interval)', {
  df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
  auto = auto_mark(df, list(x = 'x', y = 'y'))
  (length(auto$marks) %==% 1L)
  (auto$marks[[1]]$type %==% 'interval')
})

assert('auto_mark: repeated categories produce beeswarm', {
  df = data.frame(x = rep(c('A', 'B'), each = 5), y = rnorm(10))
  auto = auto_mark(df, list(x = 'x', y = 'y'))
  (length(auto$marks) %==% 1L)
  (auto$marks[[1]]$type %==% 'beeswarm')
})

assert('auto_mark: large groups produce beeswarm + density', {
  df = data.frame(x = rep(c('A', 'B'), each = 30), y = rnorm(60))
  auto = auto_mark(df, list(x = 'x', y = 'y'))
  (length(auto$marks) %==% 2L)
  (auto$marks[[1]]$type %==% 'beeswarm')
  (auto$marks[[2]]$type %==% 'density')
  (!is.null(auto$marks[[2]]$data$transform))
})

assert('auto_mark: no density when smallest group has fewer than 30', {
  df = data.frame(x = c(rep('A', 29), rep('B', 40)), y = rnorm(69))
  auto = auto_mark(df, list(x = 'x', y = 'y'))
  (length(auto$marks) %==% 1L)
  (auto$marks[[1]]$type %==% 'beeswarm')
})

assert('auto_mark: numeric x categorical (transposed) unique -> interval', {
  df = data.frame(x = c(3, 7, 2), y = c('A', 'B', 'C'))
  auto = auto_mark(df, list(x = 'x', y = 'y'))
  (length(auto$marks) %==% 1L)
  (auto$marks[[1]]$type %==% 'interval')
  (!is.null(auto$coord))
})

assert('auto_mark: numeric x categorical (transposed) repeated -> beeswarm', {
  df = data.frame(x = rnorm(10), y = rep(c('A', 'B'), each = 5))
  auto = auto_mark(df, list(x = 'x', y = 'y'))
  (length(auto$marks) %==% 1L)
  (auto$marks[[1]]$type %==% 'beeswarm')
  (!is.null(auto$coord))
})

assert('build_config generates multiple children for beeswarm + density', {
  df = data.frame(x = rep(c('A', 'B'), each = 30), y = rnorm(60))
  chart = g2(df, x = 'x', y = 'y')
  config = build_config(chart)
  (length(config$children) %==% 2L)
  (config$children[[1]]$type %==% 'beeswarm')
  (config$children[[2]]$type %==% 'density')
})

# ---- + operator (ggplot2-style syntax) ----

assert('+ operator works with mark_point()', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') + mark_point()
  (inherits(chart, 'g2'))
  (length(chart$layers) %==% 1L)
  (chart$layers[[1]]$type %==% 'point')
})

assert('+ and |> produce identical results', {
  pipe_chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    scale_x(type = 'log') |>
    theme_('dark') |>
    interact('tooltip')
  plus_chart = g2(mtcars, x = 'mpg', y = 'hp') +
    mark_point() +
    scale_x(type = 'log') +
    theme_('dark') +
    interact('tooltip')
  (pipe_chart$layers %==% plus_chart$layers)
  (pipe_chart$scales %==% plus_chart$scales)
  (pipe_chart$theme %==% plus_chart$theme)
  (pipe_chart$interactions %==% plus_chart$interactions)
})

assert('+ works with encode()', {
  chart = g2(mtcars) + encode(x = 'mpg', y = 'hp')
  (chart$aesthetics$x %==% 'mpg')
  (chart$aesthetics$y %==% 'hp')
})

assert('+ works with transform_()', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') +
    mark_interval() + transform_('stackY')
  (length(chart$layers[[1]]$transform) %==% 1L)
  (chart$layers[[1]]$transform[[1]]$type %==% 'stackY')
})

assert('+ works with coord, facet, axis, legend, title, tooltip', {
  chart = g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Species') +
    coord_polar() +
    facet_rect(x = 'Species') +
    axis_x(title = 'Width') +
    legend_color(position = 'right') +
    title_('Iris') +
    tooltip_(FALSE)
  (chart$coords$type %==% 'polar')
  (chart$facet$type %==% 'facetRect')
  (chart$axes$x$title %==% 'Width')
  (chart$legends$color$position %==% 'right')
  (chart$chart_title %==% 'Iris')
  (chart$tooltip_config %==% FALSE)
})

assert('facet_rect() accepts formula variables', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width) |>
    facet_rect(x = ~ Species)
  (chart$facet$type %==% 'facetRect')
  (chart$facet$encode$x %==% 'Species')
})

assert('facet_circle() accepts formula variables', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width) |>
    facet_circle(position = ~ Species)
  (chart$facet$type %==% 'facetCircle')
  (chart$facet$encode$position %==% 'Species')
})

assert('labels_() accepts formula for text', {
  df = data.frame(x = c('A', 'B'), y = c(1, 2))
  chart = g2(df, y ~ x) |>
    mark_interval() |>
    labels_(text = ~ y)
  (chart$layers[[1]]$labels[[1]]$text %==% 'y')
})

assert('+ works with animate, labels_, style_mark', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') +
    mark_point() +
    animate(enter = list(type = 'fadeIn')) +
    labels_(text = 'hp') +
    style_mark(fill = 'red')
  (chart$layers[[1]]$animate$enter$type %==% 'fadeIn')
  (length(chart$layers[[1]]$labels) %==% 1L)
  (chart$layers[[1]]$style$fill %==% 'red')
})

assert('+ works with slider and scrollbar', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') +
    slider_x() + scrollbar_y()
  (chart$sliders$x %==% TRUE)
  (chart$scrollbars$y %==% TRUE)
})

assert('modifier without chart returns g2_mod', {
  mod = mark_point()
  (inherits(mod, 'g2_mod'))
  (is.function(mod))
})

assert('+ with non-modifier gives informative error', {
  (has_error(g2(mtcars) + 42))
})

assert('+ works with parent functions (mark_, scale_, coord_)', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') +
    mark_('point') + scale_('x', type = 'log') + coord_('polar')
  (chart$layers[[1]]$type %==% 'point')
  (chart$scales$x$type %==% 'log')
  (chart$coords$type %==% 'polar')
})

assert('+ works with theme wrappers', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') + theme_dark()
  (chart$theme$type %==% 'dark')
})

assert('+ works with coord_transpose', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') +
    mark_interval() + coord_transpose()
  (chart$coords$transform[[1]]$type %==% 'transpose')
})

# ---- mixing |> and + operators ----

assert('mixing + and |> produces same result as pure |>', {
  ref = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |> scale_x(type = 'log') |> theme_dark()
  # + first, then |>
  c1 = g2(mtcars, x = 'mpg', y = 'hp') + mark_point() |>
    scale_x(type = 'log') |> theme_dark()
  # |> first, then +
  c2 = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() + scale_x(type = 'log') |> theme_dark()
  # interleaved: + mark |> scale + theme
  c3 = g2(mtcars, x = 'mpg', y = 'hp') +
    mark_point() |> scale_x(type = 'log') + theme_dark()
  (c1$layers %==% ref$layers)
  (c1$scales %==% ref$scales)
  (c1$theme %==% ref$theme)
  (c2$layers %==% ref$layers)
  (c2$scales %==% ref$scales)
  (c2$theme %==% ref$theme)
  (c3$layers %==% ref$layers)
  (c3$scales %==% ref$scales)
  (c3$theme %==% ref$theme)
})

# ---- knit_print.g2 ----

assert('cdn_scripts returns two <script> tags', {
  s = cdn_scripts()
  (length(s) %==% 2L)
  (all(grepl('^<script src=".+" defer></script>$', s)))
})

assert('chart_html uses data-gglite-container when id is NULL', {
  html = chart_html(g2(mtcars, x = 'mpg', y = 'hp') |> mark_point())
  (grepl('<div data-gglite-container', html, fixed = TRUE))
  (!grepl('<div id=', html, fixed = TRUE))
  (grepl('querySelector("[data-gglite-container]")', html, fixed = TRUE))
  (grepl('removeAttribute("data-gglite-container")', html, fixed = TRUE))
  (!grepl('Object.assign', html, fixed = TRUE))
})

assert('chart_html uses id-based container when id is provided', {
  html = chart_html(g2(mtcars, x = 'mpg', y = 'hp') |> mark_point(), id = 'my-chart')
  (grepl('<div id="my-chart"', html, fixed = TRUE))
  (!grepl('querySelector', html, fixed = TRUE))
})

assert('knit_print.g2 returns a knit_asis object containing chart HTML', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width)
  out = knit_print.g2(chart)
  (inherits(out, 'knit_asis'))
  (grepl('<div', out))
})

assert('knit_print.g2 does not pass knitr chunk options to chart_html', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width)
  # knitr passes options = list(...) in ...; chart_html must not receive it
  (!has_error(knit_print.g2(chart, options = list(echo = TRUE))))
})

assert('knitr dispatches knit_print to knit_print.g2', {
  loadNamespace('knitr')
  chart = g2(iris, Sepal.Length ~ Sepal.Width)
  out = knitr::knit_print(chart)
  (inherits(out, 'knit_asis'))
})

# ---- context-sensitive scale_ / axis_ ----

assert('scale_y after second-or-later mark applies to that mark', {
  chart = g2(mtcars, x = 'mpg') |>
    mark_interval(encode = list(y = 'hp')) |>
    mark_line(encode = list(y = 'wt')) |>
    scale_y(independent = TRUE)
  (chart$layers[[2]]$scale$y %==% list(independent = TRUE))
  (is.null(chart$layers[[1]]$scale))
  (is.null(chart$scales$y))
})

assert('scale_y after a single mark applies at chart level', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    scale_y(type = 'log')
  (chart$scales$y %==% list(type = 'log'))
  (is.null(chart$layers[[1]]$scale))
})

assert('scale_y before marks always applies at chart level', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    scale_y(type = 'log') |>
    mark_point()
  (chart$scales$y %==% list(type = 'log'))
  (is.null(chart$layers[[1]]$scale))
})

assert('axis_y after second-or-later mark applies to that mark', {
  chart = g2(mtcars, x = 'mpg') |>
    mark_interval(encode = list(y = 'hp')) |>
    mark_line(encode = list(y = 'wt')) |>
    axis_y(position = 'right', grid = FALSE)
  (chart$layers[[2]]$axis$y$position %==% 'right')
  (isFALSE(chart$layers[[2]]$axis$y$grid))
  (is.null(chart$axes$y))
})

assert('axis_y after a single mark applies at chart level', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    axis_y(position = 'right')
  (chart$axes$y$position %==% 'right')
  (is.null(chart$layers[[1]]$axis))
})

assert('axis_y FALSE after single mark hides chart-level axis', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    axis_y(FALSE)
  (isFALSE(chart$axes$y))
})

assert('coord_ resets last_op so scale/axis become chart-level', {
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_point() |>
    mark_line() |>
    coord_polar() |>
    scale_x(padding = 0.5)
  (chart$scales$x %==% list(padding = 0.5))
  (is.null(chart$layers[[1]]$scale))
  (is.null(chart$layers[[2]]$scale))
})

assert('scale_y and axis_y target the last of multiple marks', {
  chart = g2(mtcars, x = 'mpg') |>
    mark_interval(encode = list(y = 'hp')) |>
    mark_line(encode = list(y = 'wt')) |>
    scale_y(independent = TRUE) |>
    axis_y(position = 'right', grid = FALSE)
  (is.null(chart$layers[[1]]$scale))
  (is.null(chart$layers[[1]]$axis))
  (chart$layers[[2]]$scale$y %==% list(independent = TRUE))
  (chart$layers[[2]]$axis$y$position %==% 'right')
})

assert('dual-axis chart builds valid config', {
  air = aggregate(cbind(Temp, Wind) ~ Month, data = airquality, FUN = mean)
  air$Month = month.abb[air$Month]
  chart = g2(air, x = 'Month') |>
    mark_interval(encode = list(y = 'Temp')) |>
    style_mark(fill = '#85C5A6', fillOpacity = 0.7) |>
    axis_y(title = 'Temperature (°F)', titleFill = '#85C5A6') |>
    mark_line(encode = list(y = 'Wind')) |>
    style_mark(stroke = 'steelblue', lineWidth = 2) |>
    scale_y(independent = TRUE) |>
    axis_y(
      position = 'right', grid = FALSE,
      title = 'Wind Speed (mph)', titleFill = 'steelblue'
    )
  cfg = build_config(chart)
  (length(cfg$children) %==% 2L)
  (cfg$children[[1]]$encode$y %==% 'Temp')
  (cfg$children[[2]]$encode$y %==% 'Wind')
  (isTRUE(cfg$children[[2]]$scale$y$independent))
  (cfg$children[[2]]$axis$y$position %==% 'right')
})

# ---- Data trimming ----

assert('collect_vars returns chart-level aesthetics', {
  chart = g2(iris, x = 'Sepal.Length', y = 'Sepal.Width', color = 'Species')
  vars = collect_vars(chart)
  ('Sepal.Length' %in% vars)
  ('Sepal.Width' %in% vars)
  ('Species' %in% vars)
  (!('Petal.Length' %in% vars))
})

assert('collect_vars includes facet encode vars', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width | Species)
  vars = collect_vars(chart)
  ('Species' %in% vars)
})

assert('collect_vars includes layer encode vars when layer has no own data', {
  chart = g2(mtcars, x = 'mpg') |>
    mark_interval(encode = list(y = 'hp')) |>
    mark_line(encode = list(y = 'wt'))
  vars = collect_vars(chart)
  ('mpg' %in% vars)
  ('hp' %in% vars)
  ('wt' %in% vars)
})

assert('collect_vars excludes layer encode vars when layer has own data', {
  df2 = data.frame(x = 1:3, z = 4:6)
  chart = g2(mtcars, x = 'mpg', y = 'hp') |>
    mark_line(data = df2, encode = list(y = 'z'))
  vars = collect_vars(chart)
  ('mpg' %in% vars)
  ('hp' %in% vars)
  (!('z' %in% vars))
})

assert('collect_vars includes layer encode vars when layer data is a transform spec', {
  chart = g2(iris, ~ Sepal.Width, color = ~ Species) |> mark_density()
  vars = collect_vars(chart)
  # KDE field + groupBy come from the transform spec inside layer$data
  ('Sepal.Width' %in% vars)
  ('Species' %in% vars)
  ('.x' %in% vars)
})

assert('collect_vars includes labels text vars for layers without own data', {
  chart = g2(iris, Sepal.Length ~ Sepal.Width) |>
    mark_point() |>
    labels_(text = ~ Petal.Length)
  vars = collect_vars(chart)
  ('Sepal.Length' %in% vars)
  ('Sepal.Width' %in% vars)
  ('Petal.Length' %in% vars)
  (!('Petal.Width' %in% vars))
})

assert('collect_vars excludes labels text vars when layer has own data', {
  df2 = data.frame(x = 1:3, y = 4:6, label = c('a', 'b', 'c'))
  chart = g2(mtcars, hp ~ mpg) |>
    mark_point(data = df2, encode = list(x = 'x', y = 'y')) |>
    labels_(text = ~ label)
  vars = collect_vars(chart)
  ('mpg' %in% vars)
  ('hp' %in% vars)
  (!('label' %in% vars))
})

assert('trim_data removes unused columns', {
  df = iris
  result = trim_data(df, c('Sepal.Length', 'Species'))
  (ncol(result) %==% 2L)
  (names(result) %==% c('Sepal.Length', 'Species'))
})

assert('trim_data keeps all columns when vars is empty', {
  df = iris
  result = trim_data(df, character(0))
  (ncol(result) %==% ncol(df))
})

assert('trim_data does not trim when vars match all columns', {
  df = iris[, 1:2]
  result = trim_data(df, c('Sepal.Length', 'Sepal.Width'))
  (ncol(result) %==% 2L)
})

assert('trim_data does not trim I()-wrapped data frames', {
  df = I(iris)
  result = trim_data(df, c('Sepal.Length'))
  (ncol(result) %==% ncol(iris))
  (!inherits(result, 'AsIs'))
})

assert('build_config trims chart-level data to used columns', {
  chart = g2(iris, x = 'Sepal.Length', y = 'Sepal.Width') |> mark_point()
  cfg = build_config(chart)
  (names(cfg$data$value) %==% c('Sepal.Length', 'Sepal.Width'))
})

assert('build_config keeps all columns when data is I()-wrapped', {
  chart = g2(I(iris), x = 'Sepal.Length', y = 'Sepal.Width') |> mark_point()
  cfg = build_config(chart)
  (length(names(cfg$data$value)) %==% ncol(iris))
})

assert('build_config trims mark-level data frames', {
  df = data.frame(a = 1:3, b = 4:6, c = 7:9)
  chart = g2() |> mark_point(data = df, encode = list(x = 'a', y = 'b'))
  cfg = build_config(chart)
  mark_data = cfg$children[[1]]$data$value
  (names(mark_data) %==% c('a', 'b'))
})

assert('build_config preserves labels text column in mark-level data', {
  df = data.frame(x = 1:3, y = 4:6, lbl = c('p', 'q', 'r'), z = 7:9)
  chart = g2() |>
    mark_point(data = df, encode = list(x = 'x', y = 'y')) |>
    labels_(text = ~ lbl)
  cfg = build_config(chart)
  mark_data = cfg$children[[1]]$data$value
  ('x' %in% names(mark_data))
  ('y' %in% names(mark_data))
  ('lbl' %in% names(mark_data))
  (!('z' %in% names(mark_data)))
})

assert('build_config does not trim I()-wrapped mark-level data', {
  df = data.frame(a = 1:3, b = 4:6, c = 7:9)
  chart = g2() |> mark_point(data = I(df), encode = list(x = 'a', y = 'b'))
  cfg = build_config(chart)
  mark_data = cfg$children[[1]]$data$value
  (length(names(mark_data)) %==% 3L)
})

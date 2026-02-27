library(testit)

# g2() creates a g2 object with defaults
assert("g2() returns an object of class g2", {
  chart <- g2()
  inherits(chart, "g2")
  is.null(chart$data)
  (chart$options$width == 640)
  (chart$options$height == 480)
  (length(chart$layers) == 0)
  (length(chart$aesthetics) == 0)
})

# g2() accepts data and inline aesthetics
assert("g2() accepts data and aesthetics", {
  df <- data.frame(x = 1:3, y = 4:6)
  chart <- g2(df, x = x, y = y)
  identical(chart$data, df)
  (chart$aesthetics$x == "x")
  (chart$aesthetics$y == "y")
})

# g2() accepts custom dimensions
assert("g2() accepts custom width and height", {
  chart <- g2(width = 800, height = 600)
  (chart$options$width == 800)
  (chart$options$height == 600)
})

# encode() maps aesthetics
assert("encode() maps variable names to aesthetics", {
  chart <- g2() |> encode(x = foo, y = bar, color = baz)
  (chart$aesthetics$x == "foo")
  (chart$aesthetics$y == "bar")
  (chart$aesthetics$color == "baz")
})

# encode() merges with existing aesthetics
assert("encode() merges with existing aesthetics", {
  chart <- g2() |> encode(x = a) |> encode(y = b)
  (chart$aesthetics$x == "a")
  (chart$aesthetics$y == "b")
})

# mark_*() functions add layers
assert("mark_point() and mark_line() add layers", {
  chart <- g2() |> mark_point() |> mark_line()
  (length(chart$layers) == 2)
  (chart$layers[[1]]$type == "point")
  (chart$layers[[2]]$type == "line")
})

assert("mark_interval() adds an interval layer", {
  chart <- g2() |> mark_interval()
  (chart$layers[[1]]$type == "interval")
})

assert("mark_area() adds an area layer", {
  chart <- g2() |> mark_area()
  (chart$layers[[1]]$type == "area")
})

assert("mark_*() passes extra options through", {
  chart <- g2() |> mark_point(style = list(fill = "red"))
  identical(chart$layers[[1]]$style, list(fill = "red"))
})

# scale_of() configures scales
assert("scale_of() stores scale configuration", {
  chart <- g2() |> scale_of("x", type = "log")
  (chart$scales$x$type == "log")
})

# coordinate() sets coordinate system
assert("coordinate() sets the coordinate type", {
  chart <- g2() |> coordinate("polar")
  (chart$coords$type == "polar")
})

# interact() adds interactions
assert("interact() adds interaction entries", {
  chart <- g2() |> interact("tooltip") |> interact("elementHighlight")
  (length(chart$interactions) == 2)
  (chart$interactions[[1]]$type == "tooltip")
  (chart$interactions[[2]]$type == "elementHighlight")
})

# gglite:::build_config() converts chart to config list
assert("gglite:::build_config() produces correct config", {
  df <- data.frame(x = c("A", "B"), y = c(3, 7))
  chart <- g2(df, x = x, y = y, width = 400, height = 300) |>
    mark_interval() |>
    scale_of("y", nice = TRUE)
  config <- gglite:::build_config(chart)

  (config$width == 400)
  (config$height == 300)
  (length(config$data) == 2)
  (config$data[[1]]$x == "A")
  (config$data[[2]]$y == 7)
  (length(config$children) == 1)
  (config$children[[1]]$type == "interval")
  (config$children[[1]]$encode$x == "x")
  (config$children[[1]]$encode$y == "y")
  (config$scale$y$nice == TRUE)
})

assert("gglite:::build_config() handles empty chart", {
  config <- gglite:::build_config(g2())
  is.null(config$data)
  is.null(config$children)
})

# chart_html() generates HTML
assert("chart_html() returns valid HTML string", {
  chart <- g2(data.frame(x = 1, y = 2), x = x, y = y) |> mark_point()
  html <- chart_html(chart, id = "test-container")
  grepl('id="test-container"', html)
  grepl("G2.Chart", html)
  grepl("chart.options", html)
  grepl("chart.render", html)
})

assert("chart_html() auto-generates an id", {
  chart <- g2() |> mark_point()
  html <- chart_html(chart)
  grepl('id="gglite-', html)
})

# knit_print.g2() returns knit_asis HTML
assert("knit_print.g2() returns knit_asis output", {
  chart <- g2(data.frame(x = 1, y = 2), x = x, y = y) |> mark_point()
  out <- gglite:::knit_print.g2(chart)
  inherits(out, "knit_asis")
  grepl("g2.min.js", out)
  grepl("G2.Chart", out)
})

# End-to-end pipe chaining
assert("pipe chaining works end-to-end", {
  chart <- g2(mtcars, x = mpg, y = hp) |>
    mark_point() |>
    scale_of("x", type = "linear") |>
    coordinate("transpose") |>
    interact("tooltip")
  inherits(chart, "g2")
  (length(chart$layers) == 1)
  (chart$scales$x$type == "linear")
  (chart$coords$type == "transpose")
  (length(chart$interactions) == 1)
})

#' Add a Geometry Layer (Mark)
#'
#' Generic function to add a mark (geometry layer) to the chart. Use the
#' specific `mark_*()` wrappers for convenience.
#'
#' @param chart A `g2` object.
#' @param type Character string for the G2 mark type.
#' @param ... Mark-level options passed to G2, such as `data`, `encode`,
#'   `transform`, `style`, `animate`, `labels`, `tooltip`, `axis`, `legend`.
#' @return The modified `g2` object.
#' @keywords internal
mark = function(chart, type, ...) {
  layer = list(type = type)
  opts = list(...)
  if (length(opts)) layer = modifyList(layer, opts)
  chart$layers = c(chart$layers, list(layer))
  chart
}

# ---- Basic marks ----

#' Add an Interval Mark (Bar / Column Chart)
#'
#' @inheritParams mark
#' @export
#' @examples
#' # Bar chart
#' g2(data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2)), x = 'x', y = 'y') |>
#'   mark_interval()
#'
#' # Stacked bar chart (using transform)
#' df = data.frame(
#'   x = rep(c('A', 'B'), each = 2), y = c(3, 2, 5, 4),
#'   color = rep(c('a', 'b'), 2)
#' )
#' g2(df, x = 'x', y = 'y', color = 'color') |>
#'   mark_interval() |> transform_of('stackY')
mark_interval = function(chart, ...) mark(chart, 'interval', ...)

#' Add a Line Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(data.frame(x = 1:5, y = c(3, 1, 4, 1, 5)), x = 'x', y = 'y') |>
#'   mark_line()
mark_line = function(chart, ...) mark(chart, 'line', ...)

#' Add a Point Mark (Scatter Plot)
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp', color = 'cyl') |> mark_point()
mark_point = function(chart, ...) mark(chart, 'point', ...)

#' Add an Area Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(data.frame(x = 1:5, y = c(3, 1, 4, 1, 5)), x = 'x', y = 'y') |>
#'   mark_area()
mark_area = function(chart, ...) mark(chart, 'area', ...)

#' Add a Rect Mark
#'
#' Draws rectangles. Commonly used with a `bin` transform for 2-D histograms.
#'
#' @inheritParams mark
#' @export
#' @examples
#' # 2-D histogram using bin transform
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_rect(
#'     transform = list(list(type = 'bin', thresholdsX = 10, thresholdsY = 10))
#'   )
mark_rect = function(chart, ...) mark(chart, 'rect', ...)

#' Add a Cell Mark
#'
#' Draws rectangular cells, commonly used for heatmaps and calendar charts.
#'
#' @inheritParams mark
#' @export
#' @examples
#' df = expand.grid(x = LETTERS[1:4], y = LETTERS[1:4])
#' df$value = seq_len(nrow(df))
#' g2(df, x = 'x', y = 'y', color = 'value') |> mark_cell()
mark_cell = function(chart, ...) mark(chart, 'cell', ...)

#' Add a Text Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
#' g2(df, x = 'x', y = 'y') |>
#'   mark_interval() |>
#'   mark_text(encode = list(text = 'y'))
mark_text = function(chart, ...) mark(chart, 'text', ...)

#' Add a Path Mark
#'
#' Connects points in data order (unlike line, which sorts by x).
#'
#' @inheritParams mark
#' @export
#' @examples
#' # A spiral path
#' n = 100
#' t = seq(0, 4 * pi, length.out = n)
#' df = data.frame(x = t * cos(t), y = t * sin(t))
#' g2(df, x = 'x', y = 'y') |> mark_path()
mark_path = function(chart, ...) mark(chart, 'path', ...)

#' Add a Polygon Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' df = data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
#' g2(df, x = 'x', y = 'y') |> mark_polygon()
mark_polygon = function(chart, ...) mark(chart, 'polygon', ...)

#' Add an Image Mark
#'
#' Places images at data coordinates. Requires an `src` encoding for image URLs.
#'
#' @inheritParams mark
#' @export
#' @examples
#' df = data.frame(x = 1:2, y = 1:2)
#' g2(df, x = 'x', y = 'y') |>
#'   mark_image(style = list(
#'     src = 'https://gw.alipayobjects.com/mdn/rms_dfc253/afts/img/A*SZGfRaFPkIoAAAAAAAAAAAAAARQnAQ'
#'   ))
mark_image = function(chart, ...) mark(chart, 'image', ...)

#' Add a Link Mark
#'
#' Draws links (lines) between pairs of points.
#'
#' @inheritParams mark
#' @export
#' @examples
#' df = data.frame(x = c(0, 1), y = c(0, 0), x1 = c(1, 2), y1 = c(1, 1))
#' g2(df) |>
#'   mark_link(encode = list(x = c('x', 'x1'), y = c('y', 'y1')))
mark_link = function(chart, ...) mark(chart, 'link', ...)

# ---- Reference / annotation marks ----

#' Add a Vertical Reference Line (lineX)
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   mark_line_x(data = list(list(x = 20)),
#'     style = list(stroke = 'red', lineDash = c(4, 4)))
mark_line_x = function(chart, ...) mark(chart, 'lineX', ...)

#' Add a Horizontal Reference Line (lineY)
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   mark_line_y(data = list(list(y = 150)),
#'     style = list(stroke = 'red', lineDash = c(4, 4)))
mark_line_y = function(chart, ...) mark(chart, 'lineY', ...)

#' Add a Range Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   mark_range(
#'     data = list(list(x = c(15, 25), y = c(100, 200))),
#'     style = list(fill = 'steelblue', fillOpacity = 0.15)
#'   )
mark_range = function(chart, ...) mark(chart, 'range', ...)

#' Add a Horizontal Range (rangeX)
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   mark_range_x(data = list(list(x = c(15, 25))),
#'     style = list(fill = 'steelblue', fillOpacity = 0.15))
mark_range_x = function(chart, ...) mark(chart, 'rangeX', ...)

#' Add a Vertical Range (rangeY)
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(mtcars, x = 'mpg', y = 'hp') |>
#'   mark_point() |>
#'   mark_range_y(data = list(list(y = c(100, 200))),
#'     style = list(fill = 'orange', fillOpacity = 0.15))
mark_range_y = function(chart, ...) mark(chart, 'rangeY', ...)

#' Add a Connector Mark
#'
#' Draws a connector line with optional labels between two data points.
#'
#' @inheritParams mark
#' @export
#' @examples
#' df = data.frame(x = c('A', 'B'), y = c(3, 7))
#' g2(df, x = 'x', y = 'y') |>
#'   mark_interval() |>
#'   mark_connector(
#'     data = list(list(x = 'A', x1 = 'B')),
#'     encode = list(x = 'x', x1 = 'x1'),
#'     labels = list(list(text = '+133%'))
#'   )
mark_connector = function(chart, ...) mark(chart, 'connector', ...)

# ---- Statistical / composite marks ----

#' Add a Box Mark
#'
#' Draws pre-computed box elements (for custom box plots).
#'
#' @inheritParams mark
#' @export
mark_box = function(chart, ...) mark(chart, 'box', ...)

#' Add a Box Plot Mark
#'
#' A composite mark that automatically computes box plot statistics (median,
#' quartiles, whiskers) from raw data.
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(iris, x = 'Species', y = 'Sepal.Width') |> mark_boxplot()
mark_boxplot = function(chart, ...) mark(chart, 'boxplot', ...)

#' Add a Density Mark
#'
#' A composite mark for kernel density estimation visualization.
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length') |>
#'   mark_density(
#'     transform = list(list(type = 'kde')),
#'     style = list(fill = 'steelblue', fillOpacity = 0.5)
#'   )
mark_density = function(chart, ...) mark(chart, 'density', ...)

#' Add a Heatmap Mark
#'
#' A composite mark for rendering heatmaps from point data.
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2(iris, x = 'Sepal.Width', y = 'Sepal.Length', color = 'Petal.Length') |>
#'   mark_heatmap()
mark_heatmap = function(chart, ...) mark(chart, 'heatmap', ...)

#' Add a Vector Mark
#'
#' Draws arrows or vectors. Useful for wind or flow field visualizations.
#'
#' @inheritParams mark
#' @export
mark_vector = function(chart, ...) mark(chart, 'vector', ...)

# ---- Graph marks ----

#' Add a Node Mark
#'
#' Used in graph visualizations together with [mark_edge()].
#'
#' @inheritParams mark
#' @export
mark_node = function(chart, ...) mark(chart, 'node', ...)

#' Add an Edge Mark
#'
#' Used in graph visualizations together with [mark_node()].
#'
#' @inheritParams mark
#' @export
mark_edge = function(chart, ...) mark(chart, 'edge', ...)

# ---- Layout marks (complex / composite) ----

#' Add a Sankey Mark
#'
#' Draws a Sankey diagram. Data should have `source`, `target`, and `value`
#' columns.
#'
#' @inheritParams mark
#' @export
#' @examples
#' df = data.frame(
#'   source = c('A', 'A', 'B'), target = c('B', 'C', 'C'),
#'   value = c(5, 3, 2)
#' )
#' g2(df) |>
#'   mark_sankey(
#'     encode = list(source = 'source', target = 'target', value = 'value'),
#'     layout = list(nodeAlign = 'center')
#'   )
mark_sankey = function(chart, ...) mark(chart, 'sankey', ...)

#' Add a Chord Mark
#'
#' Draws a chord diagram. Data should have `source`, `target`, and `value`
#' columns.
#'
#' @inheritParams mark
#' @export
#' @examples
#' df = data.frame(
#'   source = c('A', 'A', 'B'), target = c('B', 'C', 'C'),
#'   value = c(5, 3, 2)
#' )
#' g2(df) |>
#'   mark_chord(
#'     encode = list(source = 'source', target = 'target', value = 'value')
#'   )
mark_chord = function(chart, ...) mark(chart, 'chord', ...)

#' Add a Treemap Mark
#'
#' Draws a treemap layout. Data should be hierarchical (a nested list with
#' `name`, `value`, and optionally `children` fields).
#'
#' @inheritParams mark
#' @export
#' @examples
#' tree_data = list(
#'   name = 'root', children = list(
#'     list(name = 'A', value = 10),
#'     list(name = 'B', value = 20),
#'     list(name = 'C', value = 15)
#'   )
#' )
#' g2() |>
#'   mark_treemap(
#'     data = list(value = tree_data),
#'     encode = list(value = 'value')
#'   )
mark_treemap = function(chart, ...) mark(chart, 'treemap', ...)

#' Add a Pack (Circle Packing) Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' tree_data = list(
#'   name = 'root', children = list(
#'     list(name = 'A', value = 10),
#'     list(name = 'B', value = 20),
#'     list(name = 'C', value = 15)
#'   )
#' )
#' g2() |>
#'   mark_pack(
#'     data = list(value = tree_data),
#'     encode = list(value = 'value', color = 'name')
#'   )
mark_pack = function(chart, ...) mark(chart, 'pack', ...)

#' Add a Force Graph Mark
#'
#' Draws a force-directed graph layout.
#'
#' @inheritParams mark
#' @export
mark_force_graph = function(chart, ...) mark(chart, 'forceGraph', ...)

#' Add a Tree Mark
#'
#' Draws a tree layout.
#'
#' @inheritParams mark
#' @export
mark_tree = function(chart, ...) mark(chart, 'tree', ...)

#' Add a Word Cloud Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' df = data.frame(
#'   text = c('�码', 'Data', 'Science', 'R', 'G2', 'Chart'),
#'   value = c(30, 25, 20, 15, 10, 5)
#' )
#' g2(df) |>
#'   mark_word_cloud(encode = list(text = 'text', value = 'value', color = 'text'))
mark_word_cloud = function(chart, ...) mark(chart, 'wordCloud', ...)

#' Add a Gauge Mark
#'
#' @inheritParams mark
#' @export
mark_gauge = function(chart, ...) mark(chart, 'gauge', ...)

#' Add a Liquid Mark
#'
#' Draws a liquid fill gauge.
#'
#' @inheritParams mark
#' @export
#' @examples
#' g2() |>
#'   mark_liquid(data = list(list(value = 0.3)),
#'     encode = list(y = 'value'),
#'     style = list(textContent = '30%'))
mark_liquid = function(chart, ...) mark(chart, 'liquid', ...)

#' Add a Shape Mark
#'
#' A custom mark whose rendering is controlled by a JavaScript render function.
#'
#' @inheritParams mark
#' @export
mark_shape = function(chart, ...) mark(chart, 'shape', ...)

#' Add a Partition (Sunburst) Mark
#'
#' @inheritParams mark
#' @export
#' @examples
#' tree_data = list(
#'   name = 'root', children = list(
#'     list(name = 'A', value = 10, children = list(
#'       list(name = 'A1', value = 5), list(name = 'A2', value = 5)
#'     )),
#'     list(name = 'B', value = 20)
#'   )
#' )
#' g2() |>
#'   mark_partition(
#'     data = list(value = tree_data),
#'     encode = list(value = 'value'),
#'     coordinate = list(type = 'theta', innerRadius = 0.3)
#'   )
mark_partition = function(chart, ...) mark(chart, 'partition', ...)

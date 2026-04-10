#' Add a Geometry Layer (Mark)
#'
#' Generic function to add a mark (geometry layer) to the chart. Use the
#' specific `mark_*()` wrappers for convenience.
#'
#' @param chart A `g2` object.
#' @param type Character string for the G2 mark type.
#' @param ... Mark-level options passed to G2, such as `data`, `encode`,
#'   `transform`, `style`, `animate`, `labels`, `tooltip`, `axis`, `legend`.
#'   When `data` is a data frame, only columns referenced by the chart or
#'   mark-level encodings are kept. Wrap it in [I()] to preserve all columns.
#' @return The modified `g2` object.
mark_ = function(chart = NULL, type, ...) {
  mod = check_chart(mark_, chart, c(if (!missing(type)) list(type), list(...)))
  if (!is.null(mod)) return(mod)
  layer = list(type = type)
  opts = list(...)
  if (length(opts)) layer = modifyList(layer, opts)
  chart$layers = c(chart$layers, list(layer))
  chart$last_op = 'mark'
  chart
}

# ---- Basic marks ----

#' @details `mark_interval()`: Add an interval mark for bar and column charts.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Bar chart
#' df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
#' g2(df, y ~ x) |> mark_interval()
#'
#' # Stacked bar chart
#' df = data.frame(
#'   x = rep(c('A', 'B'), each = 2), y = c(3, 2, 5, 4),
#'   color = rep(c('a', 'b'), 2)
#' )
#' g2(df, y ~ x, color = ~ color) |>
#'   mark_interval() |> transform_('stackY')
mark_interval = function(chart = NULL, ...) mark_(chart, 'interval', ...)

#' @details `mark_line()`: Add a line mark (connects points sorted by x).
#' @rdname mark_
#' @export
#' @examples
#'
#' # Line and area chart
#' df = data.frame(x = 1:5, y = c(3, 1, 4, 1, 5))
#' p = g2(df, y ~ x)
#' p |> mark_line()
mark_line = function(chart = NULL, ...) mark_(chart, 'line', ...)

#' @details `mark_point()`: Add a point mark (scatter plot).
#' @rdname mark_
#' @export
#' @examples
#'
#' # Scatter plot
#' g2(mtcars, hp ~ mpg, color = ~ cyl) |> mark_point()
mark_point = function(chart = NULL, ...) {
  opts = modifyList(list(style = list(shape = 'point')), list(...))
  do.call(mark_, c(list(chart, 'point'), opts))
}

#' @details `mark_area()`: Add an area mark (filled region under the line).
#' @rdname mark_
#' @export
#' @examples
#'
#' # Area chart
#' p |> mark_area()
mark_area = function(chart = NULL, ...) mark_(chart, 'area', ...)

#' @details `mark_rect()`: Draw rectangles. Commonly used with a `bin`
#'   transform for 2-D histograms.
#' @rdname mark_
#' @export
#' @examples
#'
#' # 2-D histogram using bin transform
#' g2(mtcars, hp ~ mpg) |>
#'   mark_rect(
#'     transform = list(list(type = 'bin', thresholdsX = 10, thresholdsY = 10))
#'   )
mark_rect = function(chart = NULL, ...) mark_(chart, 'rect', ...)

#' @details `mark_cell()`: Draw rectangular cells, commonly used for heatmaps
#'   and calendar charts.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Heatmap cells
#' df = expand.grid(x = LETTERS[1:4], y = LETTERS[1:4])
#' df$value = seq_len(nrow(df))
#' g2(df, y ~ x, color = ~ value) |> mark_cell()
mark_cell = function(chart = NULL, ...) mark_(chart, 'cell', ...)

#' @details `mark_text()`: Place text labels at data coordinates.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Bar chart with text labels
#' df = data.frame(x = c('A', 'B', 'C'), y = c(3, 7, 2))
#' g2(df, y ~ x) |>
#'   mark_interval() |>
#'   mark_text(encode = list(text = 'y'))
mark_text = function(chart = NULL, ...) mark_(chart, 'text', ...)

#' @details `mark_path()`: Connect points in data order (unlike line, which
#'   sorts by x).
#' @rdname mark_
#' @export
#' @examples
#'
#' # Spiral path (points connected in data order)
#' n = 100
#' t = seq(0, 4 * pi, length.out = n)
#' df = data.frame(x = t * cos(t), y = t * sin(t))
#' g2(df, y ~ x) |> mark_path()
mark_path = function(chart = NULL, ...) mark_(chart, 'path', ...)

#' @details `mark_polygon()`: Draw filled polygon shapes.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Triangle polygon
#' df = data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
#' g2(df, y ~ x) |> mark_polygon()
mark_polygon = function(chart = NULL, ...) mark_(chart, 'polygon', ...)

#' @details `mark_image()`: Place images at data coordinates. Requires an
#'   `src` encoding for image URLs.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Image mark at two data points
#' df = data.frame(x = 1:2, y = 1:2)
#' g2(df, y ~ x) |>
#'   mark_image(style = list(
#'     src = 'https://gw.alipayobjects.com/mdn/rms_dfc253/afts/img/A*SZGfRaFPkIoAAAAAAAAAAAAAARQnAQ'
#'   ))
mark_image = function(chart = NULL, ...) mark_(chart, 'image', ...)

#' @details `mark_link()`: Draw links (lines) between pairs of points.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Link mark connecting pairs of points
#' df = data.frame(x = c(0, 1), y = c(0, 0), x1 = c(1, 2), y1 = c(1, 1))
#' g2(df) |>
#'   mark_link(encode = list(x = c('x', 'x1'), y = c('y', 'y1')))
mark_link = function(chart = NULL, ...) mark_(chart, 'link', ...)

# ---- Reference / annotation marks ----

#' @details `mark_line_x()`: Draw a vertical reference line at a given x
#'   position.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Scatter plot with reference lines and shaded regions
#' p = g2(mtcars, hp ~ mpg) |> mark_point()
#' p |> mark_line_x(data = list(list(x = 20)),
#'   style = list(stroke = 'red', lineDash = c(4, 4)))
mark_line_x = function(chart = NULL, ...) mark_(chart, 'lineX', ...)

#' @details `mark_line_y()`: Draw a horizontal reference line at a given y
#'   position.
#' @rdname mark_
#' @export
#' @examples
#' p |> mark_line_y(data = list(list(y = 150)),
#'   style = list(stroke = 'red', lineDash = c(4, 4)))
mark_line_y = function(chart = NULL, ...) mark_(chart, 'lineY', ...)

#' @details `mark_range()`: Shade a rectangular region defined by x and y
#'   intervals.
#' @rdname mark_
#' @export
#' @examples
#' p |> mark_range(
#'   data = list(list(x = c(15, 25), y = c(100, 200))),
#'   style = list(fill = 'steelblue', fillOpacity = 0.15)
#' )
mark_range = function(chart = NULL, ...) mark_(chart, 'range', ...)

#' @details `mark_range_x()`: Shade a vertical band defined by an x interval.
#' @rdname mark_
#' @export
#' @examples
#' p |> mark_range_x(data = list(list(x = c(15, 25))),
#'   style = list(fill = 'steelblue', fillOpacity = 0.15))
mark_range_x = function(chart = NULL, ...) mark_(chart, 'rangeX', ...)

#' @details `mark_range_y()`: Shade a horizontal band defined by a y interval.
#' @rdname mark_
#' @export
#' @examples
#' p |> mark_range_y(data = list(list(y = c(100, 200))),
#'   style = list(fill = 'orange', fillOpacity = 0.15))
mark_range_y = function(chart = NULL, ...) mark_(chart, 'rangeY', ...)

#' @details `mark_connector()`: Draw a connector line with optional labels
#'   between two data points.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Bar chart with connector annotation
#' df = data.frame(x = c('A', 'B'), y = c(3, 7))
#' g2(df, y ~ x) |>
#'   mark_interval() |>
#'   mark_connector(
#'     data = list(list(x = 'A', x1 = 'B')),
#'     encode = list(x = 'x', x1 = 'x1'),
#'     labels = list(list(text = '+133%'))
#'   )
mark_connector = function(chart = NULL, ...) mark_(chart, 'connector', ...)

# ---- Statistical / composite marks ----

#' @details `mark_box()`: Draw pre-computed box elements (for custom box
#'   plots).
#' @rdname mark_
#' @export
mark_box = function(chart = NULL, ...) mark_(chart, 'box', ...)

#' @details `mark_boxplot()`: A composite mark that automatically computes box
#'   plot statistics (median, quartiles, whiskers) from raw data.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Box plot and beeswarm
#' p = g2(iris, Sepal.Width ~ Species)
#' p |> mark_boxplot()
mark_boxplot = function(chart = NULL, ...) mark_(chart, 'boxplot', ...)

#' @details `mark_beeswarm()`: Display individual data points using force
#'   simulation to avoid overlapping. Particularly useful for visualizing
#'   distributions within categories.
#' @rdname mark_
#' @export
#' @examples
#' p |> mark_beeswarm()
mark_beeswarm = function(chart, ...) mark_(chart, 'beeswarm', ...)

#' @details `mark_density()`: Visualize probability density using kernel
#'   density estimation (KDE). When the chart has a numeric `x` aesthetic, the
#'   KDE transform and encodings are configured automatically: the density is
#'   computed for the `x` column, and if `color` is also mapped, separate
#'   density curves are drawn for each group. Explicit `data`/`encode` in `...`
#'   bypass this auto-configuration.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Density plot by species
#' g2(iris, ~ Sepal.Width, color = ~ Species) |> mark_density()
mark_density = function(chart = NULL, ...) {
  mod = check_chart(mark_density, chart, list(...))
  if (!is.null(mod)) return(mod)
  opts = list(...)
  # Auto-configure KDE when no explicit data/encode provided
  if (is.null(opts$data) && is.null(opts$encode) && is.data.frame(chart$data)) {
    aes = chart$aesthetics
    field = aes$x
    color = aes$color
    if (!is.null(field) && is.numeric(chart$data[[field]])) {
      # Use color variable as the x-axis band; fall back to field name
      chart$data$.x = if (!is.null(color)) chart$data[[color]] else field
      group_by = list('.x')
      if (!is.null(color)) group_by = c(group_by, list(color))
      layer = list(
        type = 'density',
        data = list(transform = list(list(
          type = 'kde', field = field,
          groupBy = group_by, size = 20L
        ))),
        encode = list(
          x = '.x', y = 'y', size = 'size',
          color = color
        ),
        tooltip = FALSE
      )
      chart$aesthetics$x = NULL
      # Default axis titles: x = grouping variable, y = numeric field
      if (is.null(chart$axes$x$title))
        chart$axes$x$title = if (!is.null(color)) color else field
      if (is.null(chart$axes$y$title)) chart$axes$y$title = field
      if (length(opts)) layer = modifyList(layer, opts)
      chart$layers = c(chart$layers, list(layer))
      return(chart)
    }
  }
  mark_(chart, 'density', ...)
}

#' @details `mark_heatmap()`: A composite mark for rendering heatmaps from
#'   point data.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Heatmap from point data
#' g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Petal.Length) |>
#'   mark_heatmap()
mark_heatmap = function(chart = NULL, ...) mark_(chart, 'heatmap', ...)

#' @details `mark_vector()`: Draw arrows or vectors. Useful for wind or flow
#'   field visualizations.
#' @rdname mark_
#' @export
mark_vector = function(chart = NULL, ...) mark_(chart, 'vector', ...)

# ---- Graph marks ----

#' @details `mark_node()`: Used in graph visualizations together with
#'   [mark_edge()].
#' @rdname mark_
#' @export
mark_node = function(chart = NULL, ...) mark_(chart, 'node', ...)

#' @details `mark_edge()`: Used in graph visualizations together with
#'   [mark_node()].
#' @rdname mark_
#' @export
mark_edge = function(chart = NULL, ...) mark_(chart, 'edge', ...)

# ---- Layout marks (complex / composite) ----

#' @details `mark_sankey()`: Draw a Sankey diagram. Data should have
#'   `source`, `target`, and `value` columns.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Sankey and chord diagrams
#' df = data.frame(
#'   source = c('A', 'A', 'B'), target = c('B', 'C', 'C'),
#'   value = c(5, 3, 2)
#' )
#' g2(df) |>
#'   mark_sankey(
#'     encode = list(source = 'source', target = 'target', value = 'value'),
#'     layout = list(nodeAlign = 'center')
#'   )
mark_sankey = function(chart = NULL, ...) mark_(chart, 'sankey', ...)

#' @details `mark_chord()`: Draw a chord diagram. Data should have `source`,
#'   `target`, and `value` columns.
#' @rdname mark_
#' @export
#' @examples
#' g2(df) |>
#'   mark_chord(
#'     encode = list(source = 'source', target = 'target', value = 'value')
#'   )
mark_chord = function(chart = NULL, ...) mark_(chart, 'chord', ...)

#' @details `mark_treemap()`: Draw a treemap layout. Data should be
#'   hierarchical (a nested list with `name`, `value`, and optionally
#'   `children` fields).
#' @rdname mark_
#' @export
#' @examples
#'
#' # Treemap and circle packing
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
mark_treemap = function(chart = NULL, ...) mark_(chart, 'treemap', ...)

#' @details `mark_pack()`: Draw a circle packing layout. Data format is the
#'   same as `mark_treemap()`.
#' @rdname mark_
#' @export
#' @examples
#' g2() |>
#'   mark_pack(
#'     data = list(value = tree_data),
#'     encode = list(value = 'value', color = 'name')
#'   )
mark_pack = function(chart = NULL, ...) mark_(chart, 'pack', ...)

#' @details `mark_force_graph()`: Draw a force-directed graph layout.
#' @rdname mark_
#' @export
mark_force_graph = function(chart = NULL, ...) mark_(chart, 'forceGraph', ...)

#' @details `mark_tree()`: Draw a tree layout.
#' @rdname mark_
#' @export
mark_tree = function(chart = NULL, ...) mark_(chart, 'tree', ...)

#' @details `mark_word_cloud()`: Draw a word cloud.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Word cloud
#' df = data.frame(
#'   text = c('Hello', 'Data', 'Science', 'R', 'G2', 'Chart'),
#'   value = c(30, 25, 20, 15, 10, 5)
#' )
#' g2(df) |>
#'   mark_word_cloud(encode = list(text = 'text', value = 'value', color = 'text'))
mark_word_cloud = function(chart = NULL, ...) mark_(chart, 'wordCloud', ...)

#' @details `mark_gauge()`: Draw a gauge (speedometer) chart.
#' @rdname mark_
#' @export
mark_gauge = function(chart = NULL, ...) mark_(chart, 'gauge', ...)

#' @details `mark_liquid()`: Draw a liquid fill gauge.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Liquid fill gauge
#' g2() |>
#'   mark_liquid(data = list(list(value = 0.3)),
#'     encode = list(y = 'value'),
#'     style = list(textContent = '30%'))
mark_liquid = function(chart = NULL, ...) mark_(chart, 'liquid', ...)

#' @details `mark_shape()`: A custom mark whose rendering is controlled by a
#'   JavaScript render function.
#' @rdname mark_
#' @export
mark_shape = function(chart = NULL, ...) mark_(chart, 'shape', ...)

#' @details `mark_sunburst()`: A composite mark for sunburst (radial partition)
#'   visualization of hierarchical data. This wraps the partition mark with
#'   polar coordinates. The data should be a nested tree structure wrapped in a
#'   list, e.g., `data = list(type = 'inline', value = list(tree))`.
#' @rdname mark_
#' @export
#' @examples
#'
#' # Sunburst chart (hierarchical radial partition)
#' tree = list(name = 'root', children = list(
#'   list(name = 'A', value = 10, children = list(
#'     list(name = 'A1', value = 5), list(name = 'A2', value = 5)
#'   )),
#'   list(name = 'B', value = 20)
#' ))
#' g2() |> mark_sunburst(
#'   data = list(type = 'inline', value = list(tree)),
#'   encode = list(value = 'value')
#' )
mark_sunburst = function(chart = NULL, ...) {
  mod = check_chart(mark_sunburst, chart, list(...))
  if (!is.null(mod)) return(mod)
  opts = list(...)
  # Default to polar coordinates for sunburst layout
  if (is.null(opts$coordinate))
    opts$coordinate = list(type = 'polar')
  if (is.null(opts$axis)) opts$axis = FALSE
  if (is.null(opts$legend)) opts$legend = FALSE
  layer = list(type = 'partition')
  if (length(opts)) layer = modifyList(layer, opts)
  chart$layers = c(chart$layers, list(layer))
  chart
}

#' @details `mark_partition()`: A composite mark for hierarchical icicle chart
#'   visualization. For a radial (sunburst) layout, use [mark_sunburst()]
#'   instead. The data should be a nested tree structure wrapped in a list,
#'   e.g., `data = list(type = 'inline', value = list(tree))`.
#' @rdname mark_
#' @export
mark_partition = function(chart = NULL, ...) mark_(chart, 'partition', ...)

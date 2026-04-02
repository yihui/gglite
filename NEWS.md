# gglite 0.1

A lightweight R interface to the
[AntV G2](https://g2.antv.antgroup.com/) JavaScript visualization library with
a ggplot2-style API. Create interactive charts using the Grammar of Graphics
with support for 35+ geometry types (marks), scales, coordinates, themes,
transforms, facets, animations, and chart components (axes, legends, titles,
tooltips, labels, sliders, and scrollbars). Renders in R Markdown, litedown,
Shiny, and standalone HTML previews.

- `g2()` now accepts `ts` and `mts` (time series) objects. Univariate series
  are converted to a data frame with `time` and `value` columns; multivariate
  series are reshaped to long format with `time`, `series`, and `value`
  columns. The auto-mark feature detects time series data and draws line charts
  automatically (one line per series for multivariate data).

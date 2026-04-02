# gglite 0.1

- The formula syntax in `g2()` now supports `| 0 + var` for row faceting (e.g.,
  `y ~ x | 0 + var`), in addition to the existing `| var` for column faceting.

A lightweight R interface to the
[AntV G2](https://g2.antv.antgroup.com/) JavaScript visualization library with
a ggplot2-style API. Create interactive charts using the Grammar of Graphics
with support for 35+ geometry types (marks), scales, coordinates, themes,
transforms, facets, animations, and chart components (axes, legends, titles,
tooltips, labels, sliders, and scrollbars). Renders in R Markdown, litedown,
Shiny, and standalone HTML previews.

# gglite 0.1

A lightweight R interface to the
[AntV G2](https://g2.antv.antgroup.com/) JavaScript visualization library with
a ggplot2-style API. Create interactive charts using the Grammar of Graphics
with support for 35+ geometry types (marks), scales, coordinates, themes,
transforms, facets, animations, and chart components (axes, legends, titles,
tooltips, labels, sliders, and scrollbars). Renders in R Markdown, litedown,
Shiny, and standalone HTML previews.

- Removed unnecessary explicit `mark_point()` and `mark_boxplot()` calls from
  examples, vignettes, and roxygen documentation where the auto-mark feature
  can infer the correct mark type automatically. Marks under section headings
  that explicitly reference the mark type (e.g., "Point Mark") are preserved.

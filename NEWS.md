# gglite 0.1

- Renamed `labels_()` to `label()` and `tooltip_()` to `tooltip()`.

- Renamed `theme_classicDark()` to `theme_classic_dark()`.

- Renamed `scrollbar_x()` / `scrollbar_y()` to `scroll_x()` / `scroll_y()`.

- Un-exported the generic parent functions `axis_()`, `legend_()`, `mark_()`,
  `scale_()`, `slider_()`, and `theme_()`. The specific wrapper functions
  (e.g., `axis_x()`, `mark_point()`, `scale_x()`) are still exported.

A lightweight R interface to the
[AntV G2](https://g2.antv.antgroup.com/) JavaScript visualization library with
a ggplot2-style API. Create interactive charts using the Grammar of Graphics
with support for 35+ geometry types (marks), scales, coordinates, themes,
transforms, facets, animations, and chart components (axes, legends, titles,
tooltips, labels, sliders, and scrollbars). Renders in R Markdown, litedown,
Shiny, and standalone HTML previews.

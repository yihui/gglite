# gglite 0.1

- Updated all documentation (examples, vignettes, README) to use the new
  renamed API functions: `scale_x()`, `scale_y()`, `scale_color()`,
  `scale_size()` (replacing `scale_of()`), `axis_x()`, `axis_y()` (replacing
  `axis_of()`), `legend_color()` (replacing `legend_of()`), `slider_x()`,
  `slider_y()` (replacing `slider_of()`), `scrollbar_x()`, `scrollbar_y()`
  (replacing `scrollbar_of()`), `transform_()`, `theme_()`, `title_()`,
  `tooltip_()`, `labels_()`, and removed `padding_of()` in favor of the
  `padding` argument in `g2()`.

A lightweight R interface to the
[AntV G2](https://g2.antv.antgroup.com/) JavaScript visualization library with
a ggplot2-style API. Create interactive charts using the Grammar of Graphics
with support for 35+ geometry types (marks), scales, coordinates, themes,
transforms, facets, animations, and chart components (axes, legends, titles,
tooltips, labels, sliders, and scrollbars). Renders in R Markdown, litedown,
Shiny, and standalone HTML previews.

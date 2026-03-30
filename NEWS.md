# gglite 0.1

- Added `playback` argument to `g2()`. When `TRUE`, the chart's initial render
  is deferred until the container is scrolled into the viewport (via
  `IntersectionObserver`), so enter animations play when the reader first sees
  the chart rather than on page load.

- Updated `examples/animations.Rmd` to use `playback = TRUE` and longer
  animation durations so the animations are clearly visible on the demo page.

A lightweight R interface to the
[AntV G2](https://g2.antv.antgroup.com/) JavaScript visualization library with
a ggplot2-style API. Create interactive charts using the Grammar of Graphics
with support for 35+ geometry types (marks), scales, coordinates, themes,
transforms, facets, animations, and chart components (axes, legends, titles,
tooltips, labels, sliders, and scrollbars). Renders in R Markdown, litedown,
Shiny, and standalone HTML previews.

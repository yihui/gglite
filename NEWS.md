# gglite 0.1

- Dark and classicDark themes now apply a dark background (`#141414`) to the
  chart container, so the dark-themed content is visible on light pages.

- Fixed coordinate examples: the parallel coordinate legend is moved to the
  bottom to avoid overlap; the radar example uses multi-series data for a
  visible chart; the helix example uses `mark_interval()` (bars) instead of
  `mark_line()` (which was nearly invisible).

- Added examples for all working marks to the marks example page: heatmap,
  image, word cloud, sankey, chord, treemap, pack, gauge, force graph, and
  tree. Non-working marks (density with KDE, partition) are included with
  `eval = FALSE` and a note for future reference.

- Deferred rendering (`gglite.defer_render`) now sets `min-height` on the
  container div so the `IntersectionObserver` can fire reliably.

A lightweight R interface to the
[AntV G2](https://g2.antv.antgroup.com/) JavaScript visualization library with
a ggplot2-style API. Create interactive charts using the Grammar of Graphics
with support for 35+ geometry types (marks), scales, coordinates, themes,
transforms, facets, animations, and chart components (axes, legends, titles,
tooltips, labels, sliders, and scrollbars). Renders in R Markdown, litedown,
Shiny, and standalone HTML previews.

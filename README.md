# gglite

> **Experimental** — this package is still under active development. The API may
> change and it has been hurting my brain badly. Feedback welcome!

An R package for interactive data visualization using the [AntV
G2](https://g2.antv.antgroup.com/) JavaScript library, with a [Grammar of
Graphics](https://www.springer.com/gp/book/9780387245447)-style API inspired by
ggplot2. The goal is to have Grammar of Graphics *and* interactivity *and* keep
everything lightweight ({gglite} has only one R package dependency).

## Install

Not on CRAN yet, but r-universe.dev is an awesome service in many ways:

``` r
install.packages('gglite', repos = 'https://yihui.r-universe.dev')
```

You can also try the package without installing in the package playground:
<https://pkg.yihui.org/gglite/playground/> (it also works on your mobile
devices, thanks to webR). See the package site (<https://pkg.yihui.org/gglite/>)
for examples and documentation.

R Markdown, [litedown](#0), Quarto, Jupyter, Shiny, and standalone HTML previews
(in the browser) are all supported.

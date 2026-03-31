#' Shiny Output for a G2 Chart
#'
#' Create a placeholder in the UI for a G2 chart rendered by [renderG2()].
#' The required JavaScript dependencies (G2 library, column-major data helper,
#' and output binding) are automatically attached to the page.
#'
#' @param outputId Output variable name to read the chart from.
#' @param width,height CSS dimensions for the chart container.
#' @return A Shiny UI element.
#' @examples
#' \dontrun{
#' library(shiny)
#' ui = fluidPage(g2Output('chart1'))
#' server = function(input, output, session) {
#'   output$chart1 = renderG2({
#'     g2(mtcars, x = 'mpg', y = 'hp') |> mark_point()
#'   })
#' }
#' shinyApp(ui, server)
#' }
#' @export
g2Output = function(outputId, width = '100%', height = '480px') {
  dep_g2 = htmltools::htmlDependency(
    'g2', '5',
    src = c(href = 'https://unpkg.com/@antv/g2@5/dist'),
    script = 'g2.min.js'
  )
  dep_col = htmltools::htmlDependency(
    'g2-column', '1',
    src = c(href = 'https://cdn.jsdelivr.net/npm/@xiee/utils/js'),
    script = 'g2-column.min.js'
  )
  dep_binding = htmltools::htmlDependency(
    'g2-binding', as.character(utils::packageVersion('gglite')),
    src = system.file('www', package = 'gglite'),
    script = 'g2-binding.js'
  )
  shiny::tagList(
    dep_g2, dep_col, dep_binding,
    shiny::div(
      id = outputId, class = 'gglite-output',
      style = paste0('width:', width, ';height:', height)
    )
  )
}

#' Render a G2 Chart in Shiny
#'
#' Create a reactive G2 chart for use with [g2Output()].
#' Assign it to an output slot: `output$ID = renderG2({...})`.
#'
#' @param expr An expression that returns a `g2` object.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Whether `expr` is already quoted.
#' @return A render function for use with [g2Output()].
#' @export
renderG2 = function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr = substitute(expr)
  expr_env = env
  func = function() eval(expr, envir = expr_env)
  shiny::markRenderFunction(g2Output, function() {
    chart = func()
    list(ctor = chart$options, spec = xfun::tojson(build_config(chart)))
  })
}

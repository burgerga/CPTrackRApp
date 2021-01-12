#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#' @param shiny.maxRequestSize This is a number which specifies the maximum web request size, which 
#' serves as a size limit for file uploads. If unset, the maximum request size defaults to 1024 MiB.
#'
#' @export
run_app <- function(shiny.maxRequestSize = 1024*1024^2,
  ...
) {
  options(shiny.maxRequestSize = shiny.maxRequestSize)
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(...)
  )
}

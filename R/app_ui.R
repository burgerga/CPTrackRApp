#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      h1("CPTrackR App"),
      sidebarLayout(
        sidebarPanel(
         mod_sqlite_file_ui("sqlfile", "Choose sqlite file"),
         width = 3),  
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Tracking Preview",
                               plotOutput("trackplot", height = "80vh")
                               ),
                      tabPanel("Pipeline",
                               verbatimTextOutput("pipeline"),
                               tags$head(tags$style("#pipeline{overflow-y:scroll; max-height: 80vh;}")))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CPTrackRApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


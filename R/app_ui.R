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
      div(id = "toprow",
        div(id = "topleft", div(titlePanel("CPTrackR App"))),
        div(id = "topmid", div(mod_sqlite_file_ui("sqlfile", "Choose sqlite file"))),
        div(id = "topright", tableOutput("exp_info"))
      ),
      tabsetPanel(type = "tabs",
                  tabPanel("SQLite Explorer", 
                           selectInput('tables', "Choose table", c()),
                           DT::dataTableOutput("content"),
                           conditionalPanel(condition="input.tables!=''",
                                            downloadButton('downloadData', 'Download'))
                  ),
                  tabPanel("Tracking",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput('group_input', "Choose group", c()),
                               downloadButton('dl_fixed_tracks', 'Download all fixed tracks')
                             ),
                             mainPanel(
                               plotOutput("trackplot", height = "80vh") %>% 
                                 shinycssloaders::withSpinner()
                             )
                           )
                           ),
                  tabPanel("Pipeline",
                           verbatimTextOutput("pipeline"),
                           tags$head(tags$style("#pipeline{overflow-y:scroll; max-height: 80vh;}")))
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


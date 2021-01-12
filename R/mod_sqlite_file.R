sqlite_exts <- c('db', 'sqlite', 'sqlite3') 

#' sqlite_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sqlite_file_ui <- function(id, label = "Sqlite file"){
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label,
              multiple = FALSE,
              accept = c("application/vnd.sqlite3", 
                         "application/x-sqlite3",
                         ".db", ".sqlite", ".sqlite3"))
  )
}
    
#' sqlite_file Server Function
#'
#' @noRd 
mod_sqlite_file_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
      pool <- reactiveVal(label = "pool")
      close_pool <- function(pool) {
        if(!is.null(pool) && DBI::dbIsValid(pool)) {
          pool::poolClose(pool)
        }
        return(invisible(NULL))
      }
      
      observeEvent(input$file, {
        file <- input$file
        validate(
          need(tools::file_ext(file$name) %in% sqlite_exts, 
               "Wrong File Format try again!"))
       
        pool(close_pool(pool()))
        pool(pool::dbPool(
          drv = RSQLite::SQLite(),
          dbname = file$datapath
        ))
      })  
      
      onStop(function() {
        isolate(close_pool(pool()))
      })
      
      return(pool)
    }
  )
}
 

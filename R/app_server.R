#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
 sqlpool <- mod_sqlite_file_server("sqlfile")
 #output$tbl <- renderTable({
  # req(sqlpool())
   #RSQLite::dbReadTable(sqlpool(), "Experiment_Properties")
 #})
 
 sqltables <- reactive({
   req(sqlpool())
   DBI::dbListTables(sqlpool())
 })
 
    
 observe({
    x <- sqltables()
    updateSelectInput(session, "tables",
                      label = "Select table",
                      choices = x,
                      selected = head(x, 1)
    )
 })
 
 sqltable_data <- reactive({
   req(input$tables)
   sqlpool() %>% tbl(input$tables) %>% collect()
 })
 
 observeEvent(input$tables, {
   req(input$tables)
   output$content <- DT::renderDataTable(sqltable_data(),
                                         options = list(scrollX = TRUE))
 })
 
 output$downloadData <- downloadHandler(
    filename = function() {
       paste0(input$tables, ".csv")
    },
    content = function(file) {
       write.csv(sqltable_data(), file)
    },
    contentType = "text/csv"
 )
 
 pipeline <- reactive({
   req(sqlpool())
   get_cp_pipeline(sqlpool())
 })
 
 lut <- reactive({
   req(sqlpool())
   create_tracking_lut(sqlpool(), groups = 1) 
 })
 
 object_data <- reactive({
    req(sqlpool())
    group_id_col <- get_experiment_properties(sqlpool(), "group_id") %>% pull(value)
    get_object_data_with_groups(sqlpool()) %>%
       filter(.data[[group_id_col]] == 1) %>%
       collect()
 })
 
 output$trackplot <- renderPlot({
    plot_data <- object_data() %>% left_join(lut())
    p <- ggplot(plot_data, aes(Nuclei_Location_Center_X, Nuclei_Location_Center_Y, 
                          group = uid, color = as.factor(uid))) + 
       geom_path() + 
       guides(color = F) +
       coord_fixed() + 
      theme_bw(base_size = 15)
    p
 })
 
 output$pipeline <- renderText(pipeline())
 
}

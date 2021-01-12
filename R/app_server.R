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
                      selected = utils::head(x, 1)
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
       utils::write.csv(sqltable_data(), file)
    },
    contentType = "text/csv"
 )
 
 pipeline <- reactive({
   req(sqlpool())
   get_cp_pipeline(sqlpool())
 })
 
 lut <- reactive({
   req(sqlpool())
   create_tracking_lut(sqlpool(), groups = sel_group()) 
 })
 
 object_data <- reactive({
    req(sqlpool())
    group_id_col <- get_experiment_properties(sqlpool(), "group_id") %>% pull(value)
    get_object_data_with_groups(sqlpool()) %>%
       filter(.data[[group_id_col]] == !!sel_group()) %>%
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
 
 output$exp_info <- renderTable({
   req(sqlpool())
   
   tibble::tribble(
   ~key, ~value,
   "CellProfiler", get_cp_version(sqlpool()),
   "Run Timestamp", get_cp_time(sqlpool()) 
 )}, colnames = F)
 
 cp_groups <- reactive({
    req(sqlpool())
    get_sql_group_metadata_col(sqlpool()) %>%
       mutate(choice = paste(.[[1]], .[[2]], sep = ": "))
 })
 
 observe({
    x <- cp_groups()$choice
    updateSelectInput(session, "group_input",
                      label = "Select group",
                      choices = x,
                      selected = utils::head(x, 1)
    )
 })
 
 sel_group <- reactive({
    req(input$group_input)
    cp_groups() %>% filter(choice == input$group_input) %>% pull(1)
 })
 
 fixed_tracking_data <- reactive({
   
 })
 
 output$dl_fixed_tracks <- downloadHandler(
    filename = function() {
       paste0("fixed_tracks", ".csv")
    },
    content = function(file) {
      req(sqlpool())
      withProgress(message = 'Preparing download', value = 0, {
        parts <- 4
        incProgress(1/parts, detail = "Collecting object data")
        data <- get_object_data_with_groups(sqlpool()) %>% collect()
        incProgress(2/parts, detail = "Creating lookup table")
        lut <- create_tracking_lut(sqlpool())
        incProgress(3/parts, detail = "Joining data")
        fixed <- data %>% left_join(lut)
        incProgress(4/parts, detail = "Writing data")
        utils::write.csv(fixed, file)
      })
    },
    contentType = "text/csv"
 )
 
}

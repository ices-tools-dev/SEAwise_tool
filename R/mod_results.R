#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(titlePanel(title = textOutput(ns("region_title"))), uiOutput(ns("subregion_dropdown"), inline = T)),
    tabsetPanel(
      tabPanel("Social and Economic Effects", 
               mod_wp2_ui(ns("wp2"))),
      tabPanel("Ecological effects on Fisheries",
               mod_wp3_ui(ns("wp3"))),
      tabPanel("Ecological consequences of Fisheries",
               mod_wp4_ui(ns("wp4"))),
      #tabPanel("Spatial Management Impacts"),
      tabPanel("Management Strategy and Trade-off Evaluation",
               mod_wp6_ui(ns("wp6"))),
    )
  )
}
    
#' results Server Functions
#'
#' @noRd 
mod_results_server <- function(id, case_study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    display_region <- reactive ({
      switch(case_study(),
                        "baltic_sea" = "Baltic Sea", 
                        "western_waters" = "Western Waters",
                        "greater_north_sea" = "Greater North Sea", 
                        "mediterranean" = "Mediterranean")
    })
    
    subregion <- reactive({
      if(case_study() == "mediterranean") {
        region_choices <- c("GSA 17-19" = "central_mediterranean", "GSA 20" = "eastern_mediterranean")
      } else if(case_study() == "western_waters") {
        region_choices <- c("Celtic Sea" = "celtic_seas", "Bay of Biscay" = "bay_of_biscay")
      }
    })
    
    output$subregion_dropdown <- renderUI({
      if(case_study() %in% c("mediterranean", "western_waters")) {
      selectInput(ns("select_subregion"), "", choices = subregion())  
      }
    })
    
    local_case <- reactive({
      if(case_study() %in% c("greater_north_sea", "baltic_sea")){
        case_study()
        } else {
        req(input$select_subregion)
        input$select_subregion
      }
    })
    
    output$region_title <- renderText(display_region())
    mod_wp2_server("wp2", local_case)
    mod_wp3_server("wp3", local_case)
    mod_wp4_server("wp4", local_case)
    mod_wp6_server("wp6", local_case)
  })
}
    
## To be copied in the UI
# mod_results_ui("results_baltic")
    
## To be copied in the server
# mod_results_server("results_baltic")

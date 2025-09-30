#' scenarios UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scenarios_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width = 3,  actionButton(ns("climate_modal"), "Climate scenarios", width = "250px")),
    column(width = 3,  actionButton(ns("mgmt_modal"), "Management scenarios", width = "250px")),
  )
}
    
#' scenarios Server Functions
#'
#' @noRd 
mod_scenarios_server <- function(id, local_case){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    climate_scenario_text <- reactive({
      req(!is.null(local_case()))
      scenario_texts[[local_case()]]$climate
    })

    observeEvent(input$climate_modal, {
      req(!is.null(climate_scenario_text()))
      showModal(
        modalDialog(size = "m",
          title = "Climate Scenarios",
          HTML(climate_scenario_text()),,
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })

    mgmt_scenario_text <- reactive({
      req(!is.null(local_case()))
      texts <- scenario_texts[[local_case()]]$mgmt_generic
      texts <- paste0(texts,
                                   "<br>","<br>",
                                   "<h3><b>In this case study region, the following scenarios were considered:</b></h3>",
                                   scenario_texts[[local_case()]]$mgmt_specific)
    })
    
    observeEvent(input$mgmt_modal, {
      req(!is.null(mgmt_scenario_text()))
      showModal(
        modalDialog(size = "xl",
          title = "Management Scenarios",
          HTML(mgmt_scenario_text()),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}
    
## To be copied in the UI
# mod_scenarios_ui("scenarios_1")
    
## To be copied in the server
# mod_scenarios_server("scenarios_1")

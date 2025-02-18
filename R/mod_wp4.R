#' wp4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wp4_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("wp4_ui"))
    
    
  )
}

#' wp4 Server Functions
#'
#' @noRd 
mod_wp4_server <- function(id, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data <- reactive({
      switch(ecoregion(),
             "baltic_sea" = NULL, 
             "bay_of_biscay" = NULL, 
             "celtic_seas" = NULL, 
             "greater_north_sea" = WP4_NS, 
             "mediterranean" = WP4_med,
             "central_mediterranean" = WP4_med,
             "eastern_mediterranean" = WP4_med,
      )
    })
    
    output$wp4_ui <- renderUI({
      req(data(), ecoregion())
      tabsetPanel(
        tabPanel("Relative Benthic State",
                 mod_rbs_ui(ns("rbs_1"))),
        tabPanel("Litter",
                 mod_litter_ui(ns("litter_1"))),
        tabPanel("Bycatch",
                 mod_bycatch_ui(ns("bycatch_1"))),
        if(ecoregion() %in% c("greater_north_sea", "western_waters")){
          tabPanel("Ecosystem Depletion Risk",
                   mod_ecosystem_risk_ui(ns("ecosystem_risk_1")))
        }
      )
    })
    
    map_parameters <- reactive({
      req(!is.null(data()))
      data()$map_parameters
    })
    
    mod_bycatch_server("bycatch_1", data()$bycatch, map_parameters, ecoregion)
    
    mod_litter_server("litter_1", data()$litter, map_parameters, ecoregion)
    
    mod_rbs_server("rbs_1", data()$rbs, map_parameters, ecoregion)
    
    mod_ecosystem_risk_server("ecosystem_risk_1", data()$ecosystem)
    
  })
}

## To be copied in the UI
# mod_wp4_ui("wp4_1")

## To be copied in the server
# mod_wp4_server("wp4_1")

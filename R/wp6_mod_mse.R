#' mse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mse_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(width = 2,
                                  # shiny::selectInput(inputId = ns("mse_ecoregion"),
                                  #                    label = "Select ecoregion", 
                                  #                    choices = c("All Ecoregions" = "all_ecoregions", 
                                  #                                "Greater North Sea" = "North Sea",
                                  #                                "Celtic Sea",
                                  #                                "Bay of Biscay", 
                                  #                                "Baltic Sea",
                                  #                                "Central Mediterranean", "Eastern Mediterranean"), 
                                  #                    selected = "all_ecoregions"),
                                         selectInput(inputId = ns("mse_plot_id"),
                                                            label = "Select plot for display", 
                                                            choices = c("Changes at Regional Level" = "regional_change",
                                                                        "SSB" = "SSB",
                                                                        "F/Fmsy" = "F_ratio",
                                                                        "Average age" = "mean_age",
                                                                        "p(SSB < Blim)" = "ssb_blim",
                                                                        "Fleet Landings" = "fleet_landings",
                                                                        "Value of Fleet Landings" = "fleet_landings_value",
                                                                        "Stock Landings" = "stock_landings",
                                                                        "Value of Stock Landings" = "stock_landings_value"),
                                                     selected = "regional_change"
                                         )),
      mainPanel(
        card(full_screen = T, 
             card_body(plotOutput(ns("mse_plot"), height = "900px"), max_height_full_screen =  "100%", fill = T)) 
      )
    )
  )
}
    
#' mse Server Functions
#'
#' @noRd 
mod_mse_server <- function(id, case_study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$mse_plot <- renderPlot({
      ecoregion <- switch(case_study(),
                          "baltic_sea" = "Baltic Sea", 
                          "bay_of_biscay" = "Bay of Biscay", 
                          "celtic_seas" = "Celtic Sea", 
                          "greater_north_sea" = "North Sea", 
                          "central_mediterranean" = "Central Mediterranean",
                          "eastern_mediterranean" = "Eastern Mediterranean",
      )
      if (input$mse_plot_id %in% c("SSB", "F_ratio", "mean_age", "ssb_blim")) {
        
        filtered_stocks <- stock %>% dplyr::filter(!stock_name %in% constant_cpue)
        plot_mse_generic(df = filtered_stocks, input = input$mse_plot_id, list_params = mse_plot_params, ecoregion)
        
      } else if (input$mse_plot_id %in% c("fleet_landings", "fleet_landings_value")) {
        
        plot_mse_generic(df = total_landings_fleet, input = input$mse_plot_id, list_params = mse_plot_params, ecoregion)
        
      } else if (input$mse_plot_id %in% c("stock_landings", "stock_landings_value")) {
        
        plot_mse_generic(df = total_landings_stock, input = input$mse_plot_id, list_params = mse_plot_params, ecoregion)
        
      } else if (input$mse_plot_id == "regional_change") {
        
        plot_mse_indicators_regional(df = tab_stock, ecoregion)
        
      }
      
    })
    
  })
}

## To be copied in the UI
# mod_mse_ui("mse_1")

## To be copied in the server
# mod_mse_server("mse_1")

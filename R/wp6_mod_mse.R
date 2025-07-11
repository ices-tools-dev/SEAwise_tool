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
                                      card(full_screen = T, 
    layout_sidebar(sidebar = sidebar(selectInput(inputId = ns("mse_plot_id"),
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
                                                     selected = "regional_change")
                                     ),
                                           card_body(plotOutput(ns("mse_plot"), height = "70vh"), 
                                                     max_height_full_screen =  "100%", fill = T)),
    card(card_header("Figure Information"),
         uiOutput(ns("caption"))) 
      
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
        
        filtered_stocks %>% filter(!HCR == "PGY") %>% plot_mse_generic(input = input$mse_plot_id, list_params = mse_plot_params, ecoregion)
        # plot_mse_generic(df = filtered_stocks, input = input$mse_plot_id, list_params = mse_plot_params, ecoregion)
        
      } else if (input$mse_plot_id %in% c("fleet_landings", "fleet_landings_value")) {
        
        total_landings_fleet %>% filter(!HCR == "PGY") %>% plot_mse_generic(input = input$mse_plot_id, list_params = mse_plot_params, ecoregion)
        # plot_mse_generic(df = total_landings_fleet, input = input$mse_plot_id, list_params = mse_plot_params, ecoregion)
        
      } else if (input$mse_plot_id %in% c("stock_landings", "stock_landings_value")) {
        
        total_landings_stock %>% filter(!HCR == "PGY") %>% plot_mse_generic(input = input$mse_plot_id, list_params = mse_plot_params, ecoregion)
        # plot_mse_generic(df = total_landings_stock, input = input$mse_plot_id, list_params = mse_plot_params, ecoregion)
        
      } else if (input$mse_plot_id == "regional_change") {
        
        tab_stock %>% filter(!HCR == "PGY") %>% plot_mse_indicators_regional(ecoregion)
        #plot_mse_indicators_regional(df = tab_stock, ecoregion)
        
      }
      
    })
    
    output$caption <- renderUI({
      validate(
        need(!is.null(figure_texts[[case_study()]]), message = "")
      )
      text <- paste(select_text(figure_texts, ecoregion = case_study(), "mse", "caption"))
      HTML(text)
    })
  })
}

## To be copied in the UI
# mod_mse_ui("mse_1")

## To be copied in the server
# mod_mse_server("mse_1")

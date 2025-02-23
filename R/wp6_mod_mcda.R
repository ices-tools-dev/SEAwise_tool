#' mcda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card card_header card_body layout_sidebar sidebar accordion accordion_panel
#' @importFrom DT DTOutput
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_to_title
mod_mcda_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_sidebar(sidebar = sidebar("Set Weightings", 
                                     sliderInput(ns("stocks"), "Set Stocks utility weighting", value = 5, min = 0, max = 10, step = 1),
                                     sliderInput(ns("biodiversity"), "Set Biodiversity utility weighting", value = 5, min = 0, max = 10, step = 1),
                                     sliderInput(ns("habitats"), "Set habitats utility weighting", value = 5, min = 0, max = 10, step = 1),
                                     sliderInput(ns("community"), "Set community utility weighting", value = 5, min = 0, max = 10, step = 1, ticks = T),
                                     sliderInput(ns("revenue"), "Set revenue utility weighting", value = 5, min = 0, max = 10),
                                     sliderInput(ns("well-being"), "Set well-being Weighting", value = 5, min = 0, max = 10)
                                     ),
    accordion(open = c("Standardised User Weightings","Table of Utilities","Histogram of Utilities"),
      # accordion_panel("Standardised User Weightings",
      #   card(DTOutput(ns("weightings_table")), min_height = 150, height = "10vh")
      # ),
      accordion_panel("Table of Utilities",
        card(DTOutput(ns("scenario_table")), height = 400)
      ),
      accordion_panel("Histogram of Utilities",
        card(plotOutput(ns("hist")), height = 600)  
      )
    )
    
    )
  )
}

#' mcda Server Functions
#'
#' @noRd 
mod_mcda_server <- function(id, case_study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # macro_weight <- reactive({sum(input$fisheries_weighting, input$socioeconomic_weighting, input$ecological_weighting)})
    
    weightings <- reactive({
      req(input$stocks, input$biodiversity, input$habitats, input$community, input$revenue, input$"well-being")
     
      dat <- c(input$stocks, input$biodiversity, input$habitats, input$community, input$revenue, input$"well-being")  
      scaling <- 100/max(dat)
      dat <- round(dat*scaling, digits = 1)
        
      names(dat) <- str_to_title(c("stocks", "biodiversity", "habitats", "community", "revenue", "well-being"))
      return(dat)
    })
    
    output$weightings_table <- renderDataTable({
      req(!is.null(weightings()))
      
      dat <- weightings() %>% bind_rows()
      DT::datatable(data = dat, 
                    options = list(dom ="",
                                   ordering = FALSE),
                    rownames = FALSE, )
    })
    
    
    mcda_utilities <- reactive({
      
      mcda <- calculate_utilities(mcda_data, quantile = 0.5)
      mcda <- aggregate_utilities(mcda,
                                  criteria = c('f'), 
                                  label = "combined_f", 
                                  method = "sum", 
                                  set_weight = 1, 
                                  sub_criteria = TRUE, 
                                  weighted = TRUE)
      
      mcda <- aggregate_utilities(mcda,
                                  criteria = c('ssb'), 
                                  label = "combined_ssb", 
                                  method = "sum", 
                                  set_weight = 1, 
                                  sub_criteria = TRUE, 
                                  weighted = TRUE)
      
      # Combine criteria into macro-criteria
      
      mcda <- aggregate_utilities(mcda, criteria = c('combined_f', 'combined_ssb'), label = "stocks", method = "sum", set_weight = 1, sub_criteria = FALSE, weighted = TRUE)
      mcda <- aggregate_utilities(mcda, criteria = c('mml', 'apex_pred'), label = "biodiversity", method = "sum", set_weight = 1, sub_criteria = FALSE, weighted = TRUE)
      mcda <- aggregate_utilities(mcda, criteria = c('rbs'), label = "habitats", method = "sum", set_weight = 1, sub_criteria = FALSE, weighted = TRUE)
      mcda <- aggregate_utilities(mcda, criteria = c('employment', 'rsl', 'wage'), label = "community", method = "sum", set_weight = 1, sub_criteria = FALSE, weighted = TRUE)
      mcda <- aggregate_utilities(mcda, criteria = c('gva'), label = "revenue", method = "sum", set_weight = 1, sub_criteria = FALSE, weighted = TRUE)
      mcda <- aggregate_utilities(mcda, criteria = c('co2'), label = "well-being", method = "sum", set_weight = 1, sub_criteria = FALSE, weighted = TRUE)
      
    })
    
    mcda_data_outputs <- reactive({
      req(mcda_utilities(), weightings())
      # criteria <- c("ssb", "f", "rbs", "mml", "apex_pred", "employment", "wage", "gva", "rsl", "co2")
      
      # mcda <- set_weights(mcda_utilities(),
      #                     criteria = criteria,
      #                     new_weights = weightings())
      # 
      
      criteria <- c("stocks", "biodiversity", "habitats", "community", "revenue", "well-being")
    
      mcda <- set_weights(mcda_utilities(),
                          criteria = criteria,
                          new_weights = c(input$stocks, input$biodiversity, input$habitats, input$community, input$revenue, input$"well-being"))

      # 
      mcda  <- run_maut(mcda,
                        criteria = c("stocks", "biodiversity","habitats", "community", "revenue", "well-being")
      )
      # note that the total utility is the result of the passed criteria only. keep that in mind for visualization
      
      # transfer the results on a df

      df <- generate_df(mcda,
                       criteria = c("combined_f", "combined_ssb", "rbs", "mml", "apex_pred", "employment", "wage", "gva", "rsl", "co2",
                                    "stocks", "biodiversity","habitats", "community", "revenue", "well-being"),
                       sub_criteria = c('ssb', 'f'))
      return(list(results = df,
                  mcda_object = mcda))
      
    })
    
    output$scenario_table <- renderDataTable({
      req(mcda_data_outputs())
      
      scenario_utilities <- mcda_data_outputs()$results
      scenario_utilities[,-1] <- round(scenario_utilities[,-1], digits = 3)
      scenario_utilities <- scenario_utilities[order(scenario_utilities$total_utility, decreasing = T),]
      DT::datatable(data = scenario_utilities,
                    options = list(dom ="",
                                   ordering = TRUE))
    })
    
    output$hist <- renderPlot({
      req(mcda_data_outputs())
      
      plot_histogram(mcda_data_outputs()$results,
                     criteria = c("stocks", "biodiversity","habitats", "community", "revenue", "well-being"),
                     weights = mcda_data_outputs()$mcda_object$weights,
                     parents = mcda_data_outputs()$mcda_object$parents,
                     subcriteria = FALSE,
      )
      
    })
  })
}

## To be copied in the UI
# mod_mcda_ui("mcda_1")

## To be copied in the server
# mod_mcda_server("mcda_1")

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
mod_mcda_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_sidebar(sidebar = sidebar("Set Weightings", 
                                     accordion(multiple = TRUE, open = c("Macro level Weightings","Indicator Weightings"),
                                               accordion_panel("Macro level Weightings", 
                                                               
                                                               numericInput(ns("fisheries_weighting"), "Set Fisheries utility weighting", value = 1, min = 0, max = 100, step = 1),
                                                               numericInput(ns("socioeconomic_weighting"), "Set Socio-economic utility weighting", value = 1, min = 0, max = 100),
                                                               numericInput(ns("ecological_weighting"), "Set Ecological Weighting", value = 1, min = 0, max = 100)
                                               ),
                                               accordion_panel("Indicator Weightings",
                                                               numericInput(ns("f_weight"), "Fishing level (F)",  value = 1, min = 0, max = 100),
                                                               numericInput(ns("ssb_weight"), "Spawning Stock Biomass (SSB)",  value = 1, min = 0, max = 100),
                                                               numericInput(ns("employment_weight"), "Employment (FTE)",  value = 1, min = 0, max = 100),
                                                               numericInput(ns("gva_weight"), "Gross Value Add (GVA)",  value = 1, min = 0, max = 100),
                                                               numericInput(ns("wage_weight"), "Wages",  value = 1, min = 0, max = 100),
                                                               numericInput(ns("co2_weight"), "Carbon Dioxide Emissions",  value = 1, min = 0, max = 100)),
                                               
                                               accordion_panel("Advanced Settings",
                                                               accordion_panel("Stock Weightings", uiOutput(ns("f_sub_inputs"))),
                                                               accordion_panel("Socio-economic Sub-Weightings", uiOutput(ns("soceco_sub_inputs"))),
                                                               accordion_panel("Ecological Sub-Weightings", uiOutput(ns("ecological_sub_inputs"))))
                                     )
    ),
    card(DTOutput(ns("weightings_table")), height = 120),
    card(DTOutput(ns("scenario_table")), height = 400),
    card(plotOutput(ns("hist")), height = 600)
    )
  )
}

#' mcda Server Functions
#'
#' @noRd 
mod_mcda_server <- function(id, case_study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    macro_weight <- reactive({sum(input$fisheries_weighting, input$socioeconomic_weighting, input$ecological_weighting)})
    
    weightings <- reactive({
      req(input$fisheries_weighting, input$socioeconomic_weighting, macro_weight(), input$f_weight, input$ssb_weight, input$employment_weight, input$gva_weight, input$wage_weight, input$co2_weight)
      fisheries_weights <- c(input$f_weight, input$ssb_weight) * (input$fisheries_weighting/macro_weight())
      socioeconomic_weights <- c(input$employment_weight, input$gva_weight, input$wage_weight, input$co2_weight) * (input$socioeconomic_weighting/macro_weight())
      #ecological_weights <- c(input$f_weight, input$ssb_weight) * (input$fisheries_weighting/macro_weight())
      dat <- c(fisheries_weights, socioeconomic_weights)
      scaling <- 100/max(dat)
      dat <- matrix(data = dat*scaling, nrow = 1) %>% 
        as.data.frame() %>% 
        round(digits = 1)
      names(dat) <- c("Fishing Level", "Spawning Stock Biomass (SSB)", "Employment (FTE)", "Gross Value Add (GVA)", "Wages", "Carbon Dioxide Emissions")
      return(dat)
    })
    
    output$weightings_table <- renderDataTable({
      req(!is.null(weightings()))
      DT::datatable(data = weightings(), 
                    options = list(dom ="",
                                   ordering = FALSE),
                    rownames = FALSE, )
    })
    
    
    mcda_utilities <- reactive({
      
      mcda <- calculate_utilities(mcda, q = 0.5)
    })
    
    mcda_data_ouputs <- reactive({
      
      #Insert setting of weights here
      
      
      
      #aggregate utility
      mcda <- aggregate_utilities(mcda_utilities(),
                                  criteria = c('f'), 
                                  label = "combined_f", 
                                  method = "add", 
                                  set_weight = 1, 
                                  sub_criteria = TRUE, 
                                  weighted = TRUE)
      
      mcda <- aggregate_utilities(mcda,
                                  criteria = c('ssb'), 
                                  label = "combined_ssb", 
                                  method = "add", 
                                  set_weight = 1, 
                                  sub_criteria = TRUE, 
                                  weighted = TRUE)
      
      mcda  <- run_maut(mcda,
                        criteria = c("combined_f", "combined_ssb","employment", "wage", "gva", "rsl", "co2"))
      
      df <- generate_matrix(mcda,
                            criteria = c("combined_f", "combined_ssb","employment", "wage", "gva", "rsl", "co2"),
                            sub_criteria = c('ssb', 'f'))
    })
    
    output$scenario_table <- renderDataTable({
      top_scenarios <- mcda_data_ouputs()
      top_scenarios[,-1] <- round(top_scenarios[,-1], digits = 3)
      tops = top_scenarios(dataframe = top_scenarios, 
                           criterion = 'total_utility',
                           num = 10,
                           ascending = FALSE)
      
    })
    output$hist <- renderPlot({
      plot_histogram(mcda_data_ouputs(),
                     criteria = c('combined_ssb', 'combined_f', 'employment', 'wage', 'gva', 'rsl', 'co2'))
    })
    
    
    output$f_sub_inputs <- renderUI({
      #read data input for selected region
      
      stocks_data <- read.csv("dev/MCDA_SHARED/MCDA_SHARED/MCDA_stock_med.csv")
      stocks <- unique(stocks_data$sub_criteria)
      n <- length(stocks)
      
      sliders <- lapply(seq_len(n), function(i) {
        numericInput(
          inputId = ns(paste0("slider", i)),
          label = stocks[i],
          min = 0,
          max = 100,
          value = 1, # Rounded value for display
          step = 0.01
        )
      })
      do.call(tagList, sliders)
      
    })
    
    output$soceco_sub_inputs <- renderUI({
      #read data input for selected region
      
      soceco_data <- read.csv("dev/MCDA_SHARED/MCDA_SHARED/MCDA_socioeco_mml_med.csv")
      stocks <- unique(stocks_data$sub_criteria)
      n <- length(stocks)
      
      sliders <- lapply(seq_len(n), function(i) {
        numericInput(
          inputId = ns(paste0("slider", i)),
          label = stocks[i],
          min = 0,
          max = 100,
          value = 1, # Rounded value for display
          step = 0.01
        )
      })
      do.call(tagList, sliders)
      
    })
    
    output$ecological_sub_inputs <- renderUI({
      #read data input for selected region
      
      stocks_data <- read.csv("dev/MCDA_SHARED/MCDA_SHARED/MCDA_stock_med.csv")
      stocks <- unique(stocks_data$sub_criteria)
      n <- length(stocks)
      
      sliders <- lapply(seq_len(n), function(i) {
        numericInput(
          inputId = ns(paste0("slider", i)),
          label = stocks[i],
          min = 0,
          max = 100,
          value = 1, # Rounded value for display
          step = 0.01
        )
      })
      do.call(tagList, sliders)
      
    })
    
    
    
  })
}

## To be copied in the UI
# mod_mcda_ui("mcda_1")

## To be copied in the server
# mod_mcda_server("mcda_1")

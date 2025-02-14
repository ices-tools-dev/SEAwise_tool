#' wp3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom dplyr mutate 
mod_wp3_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(height = "70vh", full_screen = TRUE, max_height = "120%",
      layout_sidebar(sidebar = sidebar(
        uiOutput(ns("stock_selector")),
        uiOutput(ns("scenario_selector"))),
      fluidRow(column(6, card(plotOutput(ns("cat")), full_screen = TRUE)),
               column(6, card(plotOutput(ns("rec")), full_screen = TRUE))),
      fluidRow(column(6, card(plotOutput(ns("fish")), full_screen = TRUE)),
               column(6, card(plotOutput(ns("ssb"))), full_screen = TRUE)),
      
        
      ))
  )
}

#' wp3 Server Functions
#'
#' @noRd 
mod_wp3_server <- function(id, case_study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    case_study_data <- reactive({
      if (case_study() %in% c("central_mediterranean", "eastern_mediterranean")){
        selected_region <- "mediterranean"
      } else {
        selected_region <- case_study()
      }
      data <- stock_productivity[[selected_region]]$data
      refs <- stock_productivity[[selected_region]]$refs 
      list(data = data,
           refs = refs)
    })
    
    output$stock_selector <- renderUI({
      stocks <- unique(case_study_data()$data$stock)
      if(case_study() == "eastern_mediterranean") {
        stocks <- stocks[stocks %in% c("ars.gsa18-20", "ara.gsa.18-20", "mut.gsa.20")]
      } else if (case_study() == "central_mediterranean") {
        stocks <- stocks[stocks != "mut.gsa.20"]
        }
      selectInput(ns("stock_selection"), label = "Select Stock", choices = stocks)
    })
    
    stock_data <- reactive({
      req(input$stock_selection)
      req(case_study_data())
      
      data <- case_study_data()$data %>% 
        filter(stock == input$stock_selection)
      refs <- case_study_data()$refs %>% 
        filter(stock == input$stock_selection)
      list(data = data,
           refs = refs)
    })
    
    output$scenario_selector <- renderUI({
      req(input$stock_selection)
      req(stock_data())
      scenarios <- unique(stock_data()$data$scenario)
      if("Baseline" %in% scenarios){
        selected <- "Baseline"
      } else {
        selected <- scenarios[1]
      }
      checkboxGroupInput(ns("scenario"), label = "Select Scenario", choices = scenarios, selected = selected)
    }) %>% bindEvent(input$stock_selection)
    
    stock_data_filtered <- reactive({
      req(stock_data())
      req(input$scenario)
      
      stock_prod <- list(data = stock_data()$data %>% dplyr::filter(scenario == input$scenario),
                         refs = stock_data()$refs)
      return(stock_prod)
    })
    
    output$fish <- renderPlot({
      req(stock_data_filtered())
      
      plot_dat <- stock_data_filtered()$data %>% 
        mutate(indicator_lower = tolower(indicator)) %>% 
                 dplyr::filter(indicator_lower =="f")
      plot_refs <- stock_data_filtered()$refs
      plot_f(data = plot_dat, refs = plot_refs, region = case_study())
    })
    
    output$ssb <- renderPlot({
      req(stock_data_filtered())
      plot_dat <- stock_data_filtered()$data %>% 
        mutate(indicator_lower = tolower(indicator)) %>% 
                 dplyr::filter(indicator_lower =="ssb")
      plot_refs <- stock_data_filtered()$refs
      plot_ssb(data = plot_dat, refs = plot_refs, region = case_study())
    })
    
    output$rec <- renderPlot({
      req(stock_data_filtered())
      plot_dat <- stock_data_filtered()$data %>% 
        mutate(indicator_lower = tolower(indicator)) %>% 
                 dplyr::filter(indicator_lower %in% c("rec", "recruitment"))
      plot_refs <- stock_data_filtered()$refs
      plot_recruitment(data = plot_dat, refs = plot_refs, region = case_study())
    })
    
    output$cat <- renderPlot({
      req(stock_data_filtered())
      plot_dat <- stock_data_filtered()$data %>% 
        mutate(indicator_lower = tolower(indicator)) %>% 
                 dplyr::filter(indicator_lower=="catch")
      plot_refs <- stock_data_filtered()$refs
      plot_catch(data = plot_dat, refs = plot_refs, region = case_study())
    })
    
  })
}
    
## To be copied in the UI
# mod_wp3_ui("wp3_1")
    
## To be copied in the server
# mod_wp3_server("wp3_1")

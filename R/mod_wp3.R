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
      layout_sidebar(sidebar = sidebar(
        uiOutput(ns("stock_selector")),
        uiOutput(ns("scenario_panel"))),
      fluidRow(column(6, card(plotOutput(ns("cat")), height = "40vh",full_screen = TRUE)),
               column(6, card(plotOutput(ns("rec")), height = "40vh",full_screen = TRUE))),
      fluidRow(column(6, card(plotOutput(ns("fish")), height = "40vh",full_screen = TRUE)),
               column(6, card(plotOutput(ns("ssb")), height = "40vh",full_screen = TRUE))
      )),
      card(card_header("Figure Information"),
           uiOutput(ns("caption")))
      
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
      if(case_study() == "greater_north_sea") {
        flbeia <- unique(case_study_data()$data$flbeia$stock)
        sms <- unique(case_study_data()$data$sms$sp.name)
        selectInput(ns("stock_selection"), 
                    label = "Select Stock", 
                    choices = list("FLBEIA model" = flbeia,
                                   "SMS model" = sms))
      } else {
        
        stocks <- unique(case_study_data()$data$stock)
        if(case_study() == "eastern_mediterranean") {
          stocks <- stocks[stocks %in% c("ars.gsa18-20", "ara.gsa.18-20", "mut.gsa.20")]
        } else if (case_study() == "central_mediterranean") {
          stocks <- stocks[stocks != "mut.gsa.20"]
        }
        selectInput(ns("stock_selection"), label = "Select Stock", choices = stocks)
      }
    })
    
    stock_data <- reactive({
      req(input$stock_selection)
      req(case_study_data())
      
      if(case_study() == "greater_north_sea") {
        flbeia <- unique(case_study_data()$data$flbeia$stock)
        sms <- unique(case_study_data()$data$sms$sp.name)
        
        if(input$stock_selection %in% flbeia) {
          data <- case_study_data()$data$flbeia %>% 
            filter(stock == input$stock_selection)
          refs <- case_study_data()$refs %>% 
            filter(stock == input$stock_selection)
        } else {
          data <- case_study_data()$data$sms %>% 
            filter(sp.name == input$stock_selection)
          refs <- NULL
        }
        list(data = data,
             refs = refs)
      } else {
        
      data <- case_study_data()$data %>% 
        filter(stock == input$stock_selection)
      refs <- case_study_data()$refs %>% 
        filter(stock == input$stock_selection)
      list(data = data,
           refs = refs)
      }
    })
    
    output$scenario_panel <- renderUI({
      req(input$stock_selection)
      req(stock_data())
      climate_scenarios <- unique(stock_data()$data$climate_scenario)
      management_scenarios <- unique(stock_data()$data$management_scenario)
      tagList(
        checkboxGroupInput(ns("management_scenario"), label = "Select Management Scenario", choices = management_scenarios, selected = management_scenarios[1]),
        checkboxGroupInput(ns("climate_scenario"), label = "Select Climate Scenario", choices = climate_scenarios, selected = climate_scenarios)
      )
    })
    
    stock_data_filtered <- reactive({
      req(stock_data())
      req(input$management_scenario)
      req(input$climate_scenario)
      
      if(case_study() == "greater_north_sea" && 
         input$stock_selection %in% unique(case_study_data()$data$sms$sp.name)) {
      
        scenario_combi <- expand.grid(input$management_scenario,
                    input$climate_scenario)
        scenario_combi <- scenario_combi %>%
          mutate(combined_scenarios = stringr::str_c(Var1, Var2, sep = "_"))
        stock_prod <- list(data = stock_data()$data %>% 
                             dplyr::filter(scenario %in% scenario_combi$combined_scenarios),
                           refs = stock_data()$refs)
      } else {
      
        stock_prod <- list(data = stock_data()$data %>% 
                             dplyr::filter(climate_scenario %in% input$climate_scenario),
                           refs = stock_data()$refs)
      }
      return(stock_prod)
    })
    
    output$fish <- renderPlot({
      req(stock_data_filtered())
      
      plot_dat <- stock_data_filtered()$data %>% 
        mutate(indicator_lower = tolower(indicator)) %>% 
                 dplyr::filter(indicator_lower =="f")
      
      if(case_study() == "greater_north_sea" && 
         input$stock_selection %in% unique(case_study_data()$data$sms$sp.name)) {
        ggplot(plot_dat, aes(x = year, y = value, color = scenario))+geom_line()+
          labs(x='Year', y='Fishing pressure F')
          #theme(legend.position = 'top')
      } else {
        plot_refs <- stock_data_filtered()$refs
        plot_f(data = plot_dat, refs = plot_refs, region = case_study())
        
      }
      
    })
    
    output$ssb <- renderPlot({
      req(stock_data_filtered())
      
      plot_dat <- stock_data_filtered()$data %>% 
        mutate(indicator_lower = tolower(indicator)) %>% 
                 dplyr::filter(indicator_lower =="ssb")
      if(case_study() == "greater_north_sea" && 
         input$stock_selection %in% unique(case_study_data()$data$sms$sp.name)) {
        ggplot(plot_dat, aes(x = year, y = value, color = scenario))+geom_line()+
          labs(x='Year', y='SSB in 1000 t')
      } else {
      plot_refs <- stock_data_filtered()$refs
      plot_ssb(data = plot_dat, refs = plot_refs, region = case_study())
      }
    })
    
    output$rec <- renderPlot({
      req(stock_data_filtered())
      
      plot_dat <- stock_data_filtered()$data %>% 
        mutate(indicator_lower = tolower(indicator)) %>% 
                 dplyr::filter(indicator_lower %in% c("rec", "recruitment"))
      if(case_study() == "greater_north_sea" && 
         input$stock_selection %in% unique(case_study_data()$data$sms$sp.name)) {
        ggplot(plot_dat, aes(x = year, y = value, color = scenario))+geom_line()+
          labs(x='Year', y='Recruitment in millions')
      } else {
      plot_refs <- stock_data_filtered()$refs
      plot_recruitment(data = plot_dat, refs = plot_refs, region = case_study())
      }
    })
    
    output$cat <- renderPlot({
      req(stock_data_filtered())
      
      plot_dat <- stock_data_filtered()$data %>% 
        mutate(indicator_lower = tolower(indicator)) %>% 
                 dplyr::filter(indicator_lower=="catch")
      if(case_study() == "greater_north_sea" && 
         input$stock_selection %in% unique(case_study_data()$data$sms$sp.name)) {
        ggplot(plot_dat, aes(x = year, y = value, color = scenario))+geom_line()+
          labs(x='Year', y='Catches in 1000 t') 
      } else {
      plot_refs <- stock_data_filtered()$refs
      plot_catch(data = plot_dat, refs = plot_refs, region = case_study())
      }
    })
    
    output$caption <- renderUI({
      validate(
        need(!is.null(figure_texts[[case_study()]]), message = "")
      )
      text <- paste(select_text(figure_texts, ecoregion = case_study(), "wp3", "caption"))
      HTML(text)
    })
  })
}
    
## To be copied in the UI
# mod_wp3_ui("wp3_1")
    
## To be copied in the server
# mod_wp3_server("wp3_1")

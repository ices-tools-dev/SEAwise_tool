#' wp2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wp2_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Fleet Characteristics",
               mod_fleet_histograms_ui(ns("fleet_histograms_1")),
      ),
      tabPanel("Communities",
               mod_socioeconomics_ui(ns("socioeconomics_1")),
      ),
      tabPanel("Carbon Emissions",
               mod_carbon_ui(ns("carbon_1")),
      ),
      tabPanel("Fuel use and cost",
               mod_fish_fuel_ui(ns("fish_fuel_1")),
      ),
      tabPanel("Meal Provision",
               mod_adult_portions_ui(ns("adult_portions_1"))
      ),
      tabPanel("Climate and Management impacts",
               mod_wp2_projections_ui(ns("wp2_projections_1"))
      )
    )
  )
}

#' wp2 Server Functions
#'
#' @noRd 
mod_wp2_server <- function(id, case_study) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data <- reactive({
      req(!is.null(case_study()))
      switch(case_study(),
             "bay_of_biscay" = readRDS("data/wp2/BoB_data.rds"), 
             "celtic_seas" = readRDS("data/wp2/CS_data.rds"), 
             "greater_north_sea" = readRDS("data/wp2/NS_data.rds"), 
             "central_mediterranean" = readRDS("data/wp2/CMed_data.rds"),
             "mediterranean" = readRDS("data/wp2/CMed_data.rds"),
             "eastern_mediterranean" = readRDS("data/wp2/EMed_data.rds"))
    })
    
    fleet_data <- reactive(data()$fleet_data)
    portion_data <- reactive(data()$adult_portions)
    social_data <- reactive(data()$socioeco_data)
  carbon_data <- reactive(data()$carbon_data)
  fuel_data <- reactive(data()$fish_fuel_data)
  projection_data <- reactive(data()$projection_data)
  
  mod_fleet_histograms_server("fleet_histograms_1", fleet_data = fleet_data, ecoregion = case_study)
  
  mod_adult_portions_server("adult_portions_1", portion_data = portion_data, ecoregion = case_study)
  
  mod_socioeconomics_server("socioeconomics_1", social_data = social_data, ecoregion = case_study)
  
  mod_carbon_server("carbon_1", carbon_data = carbon_data, ecoregion = case_study)
  
  mod_fish_fuel_server("fish_fuel_1", fuel_data = fuel_data, ecoregion = case_study)
  
  mod_wp2_projections_server("wp2_projections_1", projection_data = projection_data, ecoregion = case_study)
  
  })
}

## To be copied in the UI
# mod_wp2_ui("wp2_1")

## To be copied in the server
# mod_wp2_server("wp2_1")

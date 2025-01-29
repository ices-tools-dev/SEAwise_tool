#' fish_fuel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom ggpmisc stat_fit_glance
#' @importFrom dplyr filter
mod_fish_fuel_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    uiOutput(ns("country_selector")), 
    card("",plotOutput(ns("fuel_plot")))
  )
}
    
#' fish_fuel Server Functions
#'
#' @noRd 
mod_fish_fuel_server <- function(id, ecoregion, fuel_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$country_selector <- renderUI({
      selectInput(ns("country_input"), "Select Country", choices = unique(fuel_data()$country))
    })
    
    output$fuel_plot <- renderPlot({
      req(fuel_data(), ecoregion(), input$country_input)
      
      dat <- filter(fuel_data(), country == input$country_input)
      
      #if(!ecoregion() %in% c("Central Mediterranean", "Eastern Mediterranean")){
        formula<-y~x
      plot <- ggplot(dat, aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
        geom_point() +
        geom_smooth(method='lm',se=T)+
        ggtitle(paste(input$country_input,sep=" ")) +
        stat_fit_glance(method = 'lm',
                        method.args = list(formula = formula),
                        geom = 'text',
                        aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
                        label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
        expand_limits(y = 2)
        
      if(ecoregion() == "Greater North Sea"){
          plot + facet_wrap(spec~.,scale="free")
        } else {
          plot + facet_wrap(Stock~.,scale="free")
        }
      #}
    }) 
  })
}
    
## To be copied in the UI
# mod_fish_fuel_ui("fish_fuel_1")
    
## To be copied in the server
# mod_fish_fuel_server("fish_fuel_1")

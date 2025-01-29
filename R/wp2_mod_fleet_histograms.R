#' fleet_histograms UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2 
mod_fleet_histograms_ui <- function(id){
  ns <- NS(id)
  tagList(
    card("",plotOutput(ns("fleet_histograms")))
  )
}
    
#' fleet_histograms Server Functions
#'
#' @noRd 
mod_fleet_histograms_server <- function(id, fleet_data, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$fleet_histograms <- renderPlot({
      req(fleet_data(), ecoregion())
      
      dat <- fleet_data()
      colnames(dat) <- tolower(colnames(dat))
      ggplot(data=dat, aes(x=year, y=value, fill=fleet)) + 
        geom_bar(stat="identity", position=position_dodge())+
        facet_wrap(country + variable~.,scales="free_y",drop=FALSE,ncol=6)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    })
    
  })
}
    
## To be copied in the UI
# mod_fleet_histograms_ui("fleet_histograms_1")
    
## To be copied in the server
# mod_fleet_histograms_server("fleet_histograms_1")

#' carbon UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_carbon_ui <- function(id){
  ns <- NS(id)
  tagList(
    card("", 
      plotOutput(ns("carbon_plot"))
         )
  )
}
    
#' carbon Server Functions
#'
#' @noRd 
mod_carbon_server <- function(id, carbon_data, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$carbon_plot <- renderPlot({
      req(carbon_data(), ecoregion())
      
      dat <- carbon_data()
      colnames(dat) <- tolower(colnames(dat))
      
      ggplot(data=data.frame(dat), aes(x=year, y=value, fill=fleet)) + 
        geom_bar(stat="identity", position=position_dodge())+
        facet_wrap(country~ variable,scales="free_y",drop=FALSE,ncol=3)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(fill='Year')
    })
  })
}

## To be copied in the UI
# mod_carbon_ui("carbon_1")

## To be copied in the server
# mod_carbon_server("carbon_1")

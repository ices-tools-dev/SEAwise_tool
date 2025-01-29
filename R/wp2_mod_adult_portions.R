#' adult_portions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_adult_portions_ui <- function(id){
  ns <- NS(id)
  tagList(
    card("",
      plotOutput(ns("adult_portions"))
         )
  )
}
    
#' adult_portions Server Functions
#'
#' @noRd 
mod_adult_portions_server <- function(id, portion_data, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$adult_portions <- renderPlot({
        req(portion_data(), ecoregion())
        plot <- ggplot(data=portion_data(), aes(y=Country, x=adult_portions, fill=Fleet)) +
                  geom_bar(stat="identity", position=position_dodge())+
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                  labs(y="thousands")+
                  ggtitle ("Number of adult portions by Country and Species caught")
      if(ecoregion() == "Greater North Sea") {
        plot + facet_wrap(spec~.,scales="free_x",drop=FALSE,ncol=3)
      } else {
        plot + facet_wrap(Stock~.,scales="free_x",drop=FALSE,ncol=3)
      }
    }) 
  })
}
    
## To be copied in the UI
# mod_adult_portions_ui("adult_portions_1")
    
## To be copied in the server
# mod_adult_portions_server("adult_portions_1")

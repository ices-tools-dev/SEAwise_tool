#' socioeconomics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_socioeconomics_ui <- function(id){
  ns <- NS(id)
  tagList(
    card("",
      plotOutput(ns("socioeco_plot"))
         )
  )
}
    
#' socioeconomics Server Functions
#'
#' @noRd 
mod_socioeconomics_server <- function(id, ecoregion, social_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$socioeco_plot <- renderPlot({
      req(social_data(), ecoregion())
      
      dat <- social_data()
      colnames(dat) <- tolower(colnames(dat))
      
      ggplot(aes(x=year,y=value,colour=fleet),data=dat)+
        geom_line(aes(x=year,y=value,colour=fleet, group=fleet),size=1)+
        facet_wrap(country~variable,scale="free",ncol=6) +
        theme(axis.text.x = element_text(angle=45))
    }) 
  })
}
    
## To be copied in the UI
# mod_socioeconomics_ui("socioeconomics_1")
    
## To be copied in the server
# mod_socioeconomics_server("socioeconomics_1")

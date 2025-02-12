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
    card(height = "70vh", full_screen = TRUE, max_height = "100%",
         layout_sidebar(sidebar = sidebar(uiOutput(ns("plot_filters"))),
                        plotOutput(ns("adult_portions")))
    )
  )
}
    
#' adult_portions Server Functions
#'
#' @noRd 
mod_adult_portions_server <- function(id, portion_data, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data <- reactive({
      dat <- portion_data()
      colnames(dat) <- tolower(colnames(dat))
      dat
    })
    
    output$plot_filters <- renderUI({
      req(data(), ecoregion())
      countries <- sort(unique(data()$country))
      if (ecoregion() == "greater_north_sea"){
        stocks <- sort(unique(data()$spec))
      } else {
        stocks <- sort(unique(data()$stock))
      }
      tagList(
        selectizeInput(ns("country_filter"), "Select Countries", choices = countries, selected = countries, multiple = T),
        selectizeInput(ns("stock_filter"), "Select Stock", choices = stocks, selected = stocks, multiple = T)
      )
    })
    
    filtered_data <- reactive({
      req(data(), input$country_filter, input$stock_filter)
      if (ecoregion() == "greater_north_sea"){
        data() %>% filter(country %in% input$country_filter, spec %in% input$stock_filter)
      } else {
        data() %>% filter(country %in% input$country_filter, stock %in% input$stock_filter)
      }
    })
    
    
    output$adult_portions <- renderPlot({
        req(filtered_data(), ecoregion())
        plot <- ggplot(data=filtered_data(), aes(y=country, x=adult_portions, fill=fleet)) +
                  geom_bar(stat="identity", position=position_dodge())+
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                  labs(y="thousands")+
                  ggtitle ("Number of adult portions by Country and Species caught")
      if(ecoregion() == "greater_north_sea") {
        plot + facet_wrap(spec~.,scales="free_x",drop=FALSE,ncol=3)
      } else {
        plot + facet_wrap(stock~.,scales="free_x",drop=FALSE,ncol=3)
      }
    }) 
  })
}
    
## To be copied in the UI
# mod_adult_portions_ui("adult_portions_1")
    
## To be copied in the server
# mod_adult_portions_server("adult_portions_1")

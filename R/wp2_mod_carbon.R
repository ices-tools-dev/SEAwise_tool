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
    card(height = "70vh", full_screen = TRUE, max_height = "100%",
         layout_sidebar(sidebar = sidebar(uiOutput(ns("plot_filters"))),
                        plotOutput(ns("carbon_plot")))
    )
  )
}
    
#' carbon Server Functions
#'
#' @noRd 
mod_carbon_server <- function(id, carbon_data, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data <- reactive({
      validate(
        need(!is.null(carbon_data()), message = "Carbon emissions data not available.")
      )
      dat <- carbon_data()
      colnames(dat) <- tolower(colnames(dat))
      dat
    })
    
    output$plot_filters <- renderUI({
      req(data(), ecoregion())
      countries <- unique(data()$country)
      variables <- unique(data()$variable)
      tagList(
        selectizeInput(ns("country_filter"), "Select Countries", choices = countries, selected = countries, multiple = T),
        )
    })
    
    filtered_data <- reactive({
      req(data(), input$country_filter)
      data() %>% filter(country %in% input$country_filter)
    })
    
    
    output$carbon_plot <- renderPlot({
      req(nrow(filtered_data()) > 0, ecoregion())
      
      ggplot(data=data.frame(filtered_data()), aes(x=year, y=value, colour=fleet)) + 
        geom_point(inherit.aes = T, size = 1.5)+
        geom_line(inherit.aes = T, stat="identity",size=1)+
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

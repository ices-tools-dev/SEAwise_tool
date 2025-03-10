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
    card(height = "70vh", full_screen = TRUE, max_height = "100%",
         layout_sidebar(sidebar = sidebar(uiOutput(ns("plot_filters"))),
                        plotOutput(ns("socioeco_plot")))
        )
  )
}
    
#' socioeconomics Server Functions
#'
#' @noRd 
mod_socioeconomics_server <- function(id, ecoregion, social_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data <- reactive({
      dat <- social_data()
      colnames(dat) <- tolower(colnames(dat))
      dat
    })
    
    output$plot_filters <- renderUI({
      req(data(), ecoregion())
      countries <- unique(data()$country)
      variables <- unique(data()$variable)
      tagList(
        selectizeInput(ns("country_filter"), "Select Countries", choices = countries, selected = countries, multiple = T),
        selectizeInput(ns("variable_filter"), "Select Fleet variables", choices = variables, selected = variables, multiple = T)
      )
    })
    
    filtered_data <- reactive({
      req(data(), input$country_filter, input$variable_filter)
      data() %>% filter(country %in% input$country_filter, variable %in% input$variable_filter)
    })
    
    
    output$socioeco_plot <- renderPlot({
      req(nrow(filtered_data()) > 0, ecoregion())
      
      var_labels <- c(
        GVA   = "Gross Value Add",
        land_val = "Landings value",
        vessels = "Vessels",
        BE = "Belgium",
        DE = "Germany",
        ES = "Spain",
        FRA = "France",
        IE = "Ireland",
        UKE = "England"
      )

      ggplot(aes(x=year,y=value,colour=fleet),data=filtered_data())+
        geom_point(inherit.aes = T, size = 1.5) +
        geom_line(aes(x=year,y=value,colour=fleet, group=fleet),size=1)+
        scale_x_discrete(
          breaks = as.numeric(seq(min(filtered_data()$year), max(filtered_data()$year), by = 2))
        )+
        scale_colour_discrete(name = "Fleet type",
                              labels = c("large" = "Large scale", "small" = "Small scale"))+
        facet_grid(variable ~ country, scales="free_y", drop=FALSE, labeller = as_labeller(var_labels))+
        labs(x='Year', y = "Euro")+
        theme(axis.text.x = element_text(angle=45, hjust = 1))
    }) 
  })
}
    
## To be copied in the UI
# mod_socioeconomics_ui("socioeconomics_1")
    
## To be copied in the server
# mod_socioeconomics_server("socioeconomics_1")

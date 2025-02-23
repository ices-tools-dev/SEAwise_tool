#' wp2_projections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wp2_projections_ui <- function(id){
  ns <- NS(id)
  tagList(
      card(height = "70vh", full_screen = TRUE, max_height = "100%",
           layout_sidebar(sidebar = sidebar(uiOutput(ns("filters"))),
      plotOutput(ns("projections_plot"))
         )
      )
  )
}
    
#' wp2_projections Server Functions
#'
#' @noRd 
mod_wp2_projections_server <- function(id, projection_data, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$filters <- renderUI({
      validate(
        need(!is.null(projection_data()), message = "Projection data not available."),
      )
      tagList(
        selectInput(ns("fleet"), "Fleet", choices = unique(projection_data()$SSF_LSF), selected = 'LSF'),
        selectInput(ns("names_filter"), "Social indicator", choices = unique(projection_data()$name), selected = 'GVA'),
      )
    })
    filtered_data <- reactive({
      validate(
        need(!is.null(projection_data()), message = "Projection data not available."),
        need(!is.null(input$names_filter), message = "Social indicator not selected."),
        need(!is.null(input$fleet), message = "Fleet not selected."),
      )
      dat <- projection_data()
      dat %>% filter(name %in% input$names_filter, 
                     SSF_LSF %in% input$fleet)
    })
    
    
    output$projections_plot <- renderPlot({
      req(nrow(filtered_data()>0))
      p1 <- ggplot(filtered_data(),aes(x = year , color = Climate, group = Climate))+
        geom_point(aes(y = median), size = 1.5)+
        geom_line(aes(y = median), size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = higher, fill = Climate), alpha =.1, linetype = 0, show.legend = FALSE)+
        facet_wrap(~Mgt_scenario, scales = 'free_y')+
        scale_y_continuous('')+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(color = 'Climate scenario')#+
      #ggtitle(inpu)
      p1
    })
    
  })
}
    
## To be copied in the UI
# mod_wp2_projections_ui("wp2_projections_1")
    
## To be copied in the server
# mod_wp2_projections_server("wp2_projections_1")

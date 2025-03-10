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
    card(height = "70vh", full_screen = TRUE, max_height = "100%",
         layout_sidebar(sidebar = sidebar(uiOutput(ns("plot_filters"))),
                        plotOutput(ns("fuel_plot")))
    )
  )
}
    
#' fish_fuel Server Functions
#'
#' @noRd 
mod_fish_fuel_server <- function(id, ecoregion, fuel_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$plot_filters <- renderUI({
      countries <- unique(fuel_data()$country)
      selectizeInput(ns("country_input"), "Select Country", choices = countries)
    })
    y_label <- bquote(
      atop(
        Fish ~ Price ~ (Euro/kg)
      )
    )
    x_label <- bquote(
      atop(
        Fuel ~ Price ~ (Euro/kg)
      )
    )
    
    output$fuel_plot <- renderPlot({
      req(nrow(fuel_data())>0, ecoregion(), input$country_input)
      dat <- filter(fuel_data(), country == input$country_input)
      
      
      formula<-y~x
        
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
        
      plot <- ggplot(dat, aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
        geom_point(size = 1.5) +
        geom_smooth(method='lm',se=T)+
        ggtitle(paste(var_labels[input$country_input],sep=" ")) +
        ylab(y_label)+
        scale_colour_discrete(name = "Fleet type",
                              labels = c("large" = "Large scale", "small" = "Small scale"))+
        labs(x=x_label, y = y_label)+
        stat_fit_glance(method = 'lm',
                        method.args = list(formula = formula),
                        geom = 'text',
                        aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
                        label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
        expand_limits(y = 2)
        
      if(ecoregion() == "greater_north_sea"){
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

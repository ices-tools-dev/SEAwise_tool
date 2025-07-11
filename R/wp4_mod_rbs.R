#' rbs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom ggnewscale new_scale_fill
#' @importFrom bslib card_footer
mod_rbs_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(height = "70vh", full_screen = TRUE, max_height = "100%",
         #layout_sidebar(sidebar = sidebar(uiOutput(ns("plot_filters"))),
                        withSpinner(uiOutput(ns("rbs_main_panel"),height = "65vh"))
         
                                    
      #)
    ),
    card(card_header("Figure Information"),
         uiOutput(ns("caption")))
    
  )
}
    
#' rbs Server Functions
#'
#' @noRd 
mod_rbs_server <- function(id, data, map_parameters, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$plot_filters <- renderUI({
  
      if (ecoregion() == "greater_north_sea") {
        tagList(
          radioButtons(
            inputId = ns("rbs_switch"),
            label = "Select single year or RBS time series:",
            choices = c("Single Year" = "focus", "Time Series" = "time_series"),
            selected = "time_series"
          ),
          # Conditionally create the year selector if "focus" is selected
          if (!is.null(input$rbs_switch) && input$rbs_switch == "focus") {
            selectInput(
              inputId = ns("rbs_year"),
              label = "Relative Benthic State in Year:",
              choices = as.character(2009:2018),
              selected = "2018"
            )
          } else {
            NULL
          }
        )
      } else {
        # If ecoregion() is anything else, show nothing or some alternative
        NULL
      }
    })
      
      # if(ecoregion() == "greater_north_sea"){
      #   tagList(
      #     radioButtons(ns("rbs_switch"), "Select focused view or RBS time series:", choices = c("Single Year" = "focus", "Time Series" = "time_series"), selected = "time_series"),
      #       if(input$rbs_switch == "focus"){
      #         selectInput(ns("rbs_year"), "Relative Benthic State in Year:", choices = as.character(2009:2018), selected = "2018")
      #       } else {NULL}
      #     
      #   )
      # 
      #   
      # }
    # })
    
    output$rbs_main_panel <- renderUI({
      req(ecoregion())
      #req(!is.null(input$rbs_switch))
      
      # if (ecoregion() == "greater_north_sea" && input$rbs_switch == "focus") {
      #   withSpinner(plotOutput(ns("rbs_plot"),height = "70vh"))
      # } else 
        if(ecoregion() == "greater_north_sea"){  #&& input$rbs_switch == "time_series") {
        plotOutput(ns("rbs_time_series"),height = "70vh")
      } else { 
        withSpinner(plotOutput(ns("rbs_plot"),height = "70vh"))
      }
    })
    
    output$rbs_time_series <- renderImage({
      req(ecoregion())
      # req(input$rbs_switch)
      if (ecoregion() == "greater_north_sea"){ #&& input$rbs_switch == "time_series"){
        imagefile <- system.file("extdata/wp4", "NS_rbs.jpeg", package = "SEAwiseTool")
        if (imagefile == "") {
          # File not found – return NULL to avoid errors
          return(NULL)
        }
        
        return(list(
          src = imagefile,
          width = "auto",      # or any numeric value or "auto"
          height = "500",     # or any numeric value or "auto"
          alt = "RBS timeseries"
        ))
      } else {
        cat(">>> Checking data before plotting\n")
        return(NULL)
      }
    }, deleteFile = FALSE
    )
    
    output$rbs_plot <- renderPlot({
      
      if (ecoregion() == "greater_north_sea"){
        req(!is.null(input$rbs_switch) && input$rbs_switch == "focus")
        req(!is.null(input$rbs_year))
        data_range <- data[[input$rbs_year]]
        
        ggplot(data = data,aes(fill = data_range, col=data_range))+
          geom_sf(na.rm=T)+
          scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS",direction = -1)+
          scale_color_viridis_c(option="viridis",na.value = NA, name = "RBS",direction = -1)+
          geom_sf(data=land,col=NA,fill="grey")+
          theme_classic()+
          theme(plot.background=element_blank(),
                panel.background=element_blank(),
                axis.text.y   = element_text(size=16),
                axis.text.x   = element_text(size=16),
                axis.title.y  = element_text(size=16),
                axis.title.x  = element_text(size=16),
                panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
                legend.text   = element_text(size=11),
                legend.title  = element_text(size=11))+
          scale_x_continuous(breaks=map_parameters()$coordxmap)+
          scale_y_continuous(breaks=map_parameters()$coordymap,expand=c(0,0))+
          coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), 
                   ylim=c(map_parameters()$coordslim[3], map_parameters()$coordslim[4]))+
          ylab("Latitude")+
          xlab("Longitude")
        
      } else if (ecoregion() %in% c("mediterranean", "central_mediterranean", "eastern_mediterranean")) {
        
        ggplot()+
          geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_AdriaticSea_GSAs17_18),data = data[[1]],na.rm=T)+
          scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS in GSA 17 & 18",direction = -1)+
          new_scale_fill() +
          geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_WesternIonianSea_GSA19),data = data[[2]],na.rm=T)+
          scale_fill_viridis_c(option="magma",na.value = NA, name = "RBS in GSA 19",direction = -1)+
          new_scale_fill() +
          geom_raster(aes(x = x, y = y, fill =GSA20_RBS),data = data[[3]],na.rm=T)+
          scale_fill_viridis_c(option="plasma",na.value = NA, name = "RBS in GSA 20",direction = -1)+
          geom_sf(data=land,col=NA,fill="grey")+
          geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
          geom_sf_label(data=map_parameters()$gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
          theme_classic()+
          theme(plot.background=element_blank(),
                panel.background=element_blank(),
                axis.text.y   = element_text(size=16),
                axis.text.x   = element_text(size=16),
                axis.title.y  = element_text(size=16),
                axis.title.x  = element_text(size=16),
                panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
                legend.text   = element_text(size=11),
                legend.title  = element_text(size=11))+
          scale_x_continuous(breaks=map_parameters()$coordxmap)+
          scale_y_continuous(breaks=map_parameters()$coordymap,expand=c(0,0))+
          coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
          ylab("Latitude")+
          xlab("Longitude")
      } else if (ecoregion() %in% c("celtic_seas", "western_waters")){
        
        ggplot()+
          geom_raster(aes(x = x, y = y, fill =state),data = data$rbs_cs,na.rm=T)+
          scale_fill_viridis_c(option="plasma",na.value = NA, name = "RBS with the IGFS survey",direction = -1)+
          geom_sf(data=land,col=NA,fill="grey")+
          theme_classic()+
          theme(plot.background=element_blank(),
                panel.background=element_blank(),
                axis.text.y   = element_text(size=16),
                axis.text.x   = element_text(size=16),
                axis.title.y  = element_text(size=16),
                axis.title.x  = element_text(size=16),
                panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
                legend.text   = element_text(size=11),
                legend.title  = element_text(size=11))+
          scale_x_continuous(breaks=map_parameters()$coordxmap_cs)+
          scale_y_continuous(breaks=map_parameters()$coordymap_cs,expand=c(0,0))+
          coord_sf(xlim=c(map_parameters()$coordslim$coordslim_cs[1], map_parameters()$coordslim$coordslim_cs[2]),
                   ylim=c(map_parameters()$coordslim$coordslim_cs[3], map_parameters()$coordslim$coordslim_cs[4]))+
          ylab("Latitude")+
          xlab("Longitude")
        
      } else if (ecoregion() %in% c("bay_of_biscay", "western_waters")){
        
        ggplot()+
          geom_raster(aes(x = x, y = y, fill =  RBS_surface_sarmean),data = data$rbs_bob,na.rm=T)+
          scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS with EVHOE survey",direction = -1)+
          geom_sf(data=land,col=NA,fill="grey")+
          theme_classic()+
          theme(plot.background=element_blank(),
                panel.background=element_blank(),
                axis.text.y   = element_text(size=16),
                axis.text.x   = element_text(size=16),
                axis.title.y  = element_text(size=16),
                axis.title.x  = element_text(size=16),
                panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
                legend.text   = element_text(size=11),
                legend.title  = element_text(size=11))+
          scale_x_continuous(breaks=map_parameters()$coordxmap_bob)+
          scale_y_continuous(breaks=map_parameters()$coordymap_bob,expand=c(0,0))+
          coord_sf(xlim=c(map_parameters()$coordslim$coordslim_bob[1], map_parameters()$coordslim$coordslim_bob[2]),
                   ylim=c(map_parameters()$coordslim$coordslim_bob[3], map_parameters()$coordslim$coordslim_bob[4]))+
          ylab("Latitude")+
          xlab("Longitude")
      }
    }) #%>% bindCache(ecoregion(), input$rbs_year)
  
    output$caption <- renderUI({
      text <- paste(select_text(figure_texts, ecoregion = ecoregion(), "rbs", "caption"))
      HTML(text)
    })
  })
}

## To be copied in the UI
# mod_rbs_ui("rbs_1")

## To be copied in the server
# mod_rbs_server("rbs_1")

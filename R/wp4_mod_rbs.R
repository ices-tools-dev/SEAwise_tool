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
mod_rbs_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(full_screen = T, min_height = "70vh",
         uiOutput(ns("rbs_year_selector")),
         card_body(max_height_full_screen = "70vh",
                   withSpinner(plotOutput(ns("rbs_plot"),height = "70vh"))
         )
    )
    
  )
}
    
#' rbs Server Functions
#'
#' @noRd 
mod_rbs_server <- function(id, data, map_parameters, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$rbs_year_selector <- renderUI({
      if(ecoregion() == "greater_north_sea"){
        tagList(
          card_header(
          selectInput(ns("rbs_year"), "Relative Benthic State in Year:", choices = as.character(2009:2018), selected = "2018")
          )
        )
      }
    })
    
    output$rbs_plot <- renderPlot({
      
      if (ecoregion() == "greater_north_sea"){
        req(input$rbs_year)
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
      } else if (ecoregion() %in% c("celtic_seas", "bay_of_biscay", "western_waters")){
        
        ggplot()+
          geom_raster(aes(x = x, y = y, fill =  RBS_surface_sarmean),data = data$rbs_bob,na.rm=T)+
          scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS with EVHOE survey",direction = -1)+
          new_scale_fill() +
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
          scale_x_continuous(breaks=map_parameters()$coordxmap$coordxmap_ww)+
          scale_y_continuous(breaks=map_parameters()$coordxmap$coordymap_ww,expand=c(0,0))+
          coord_sf(xlim=c(map_parameters()$coordslim$coordslim_ww[1], map_parameters()$coordslim$coordslim_ww[2]), 
                   ylim=c(map_parameters()$coordslim$coordslim_ww[3],map_parameters()$coordslim$coordslim_ww[4]))+
          ylab("Latitude")+
          xlab("Longitude")
      }
    }) %>% bindCache(ecoregion(), input$rbs_year)
  })
}

## To be copied in the UI
# mod_rbs_ui("rbs_1")

## To be copied in the server
# mod_rbs_server("rbs_1")

#' bycatch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @importFrom dplyr filter
mod_bycatch_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(height = "70vh", full_screen = TRUE, max_height = "100%",
         layout_sidebar(sidebar = sidebar(uiOutput(ns("bycatch_selection_panel"))),
                        uiOutput(ns("bycatch_main_panel"))))
  )
}

#' bycatch Server Functions
#'
#' @noRd 
mod_bycatch_server <- function(id, data, map_parameters, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$bycatch_selection_panel <- renderUI({
      if(ecoregion()=="greater_north_sea") {
        tagList(
          selectInput(ns("bycatch_species"), "Select bycatch species", choices = unique(data$species)),
          radioButtons(ns("bycatch_switch"), "Select bycatch risk for specific season and gear or view all:", choices = c("Selection", "All combinations (SLOW >30s)"))        
        )
      }
    })
    
    output$bycatch_main_panel <- renderUI({
      if(ecoregion()=="greater_north_sea"){
        req(input$bycatch_switch)
        if (input$bycatch_switch == "Selection") {
          tagList(
            card(
            card_header(
              fluidRow(column(width = 6, 
                              selectInput(ns("bycatch_season"), "Select bycatch risk season", choices = unique(data$season))
                              ),
                       column(width = 6,
              selectInput(ns("bycatch_gear"), "Select bycatch gear season", choices = unique(data$gear))
              ))
            ),
            withSpinner(plotOutput(ns("bycatch_plot"), height = "55vh"))
          ))
        } else {
          tagList(
            card(
              withSpinner(plotOutput(ns("bycatch_facet_plot"), height = "65vh"))
            )
          )
        }
      } else if(ecoregion() %in% c("mediterranean", "central_mediterranean", "eastern_mediterranean")){
        ns("bycatch_med_plot")
      } else if(ecoregion() %in% c("celtic_seas")){
        plotOutput(ns("bycatch_ww_cetacean_plot"), height = "70vh")
      } else if(ecoregion() %in% c("bay_of_biscay", "western_waters")){
        plotOutput(ns("bycatch_ww_shearwater_plot"), height = "70vh")
      }
    })
    
    filtered_data <- reactive({
      
      req(input$bycatch_species)
      data %>% filter(species == input$bycatch_species)
    })
    
    output$bycatch_plot <- renderPlot({
      req(input$bycatch_species, input$bycatch_gear, input$bycatch_season)
      
      nameFilllit <- bquote(atop(
        Predicted ~ fisheries ~ related,
        litter ~ (Numbers/km^2)
      ))
      
      dat <- filter(filtered_data(),
                    gear == input$bycatch_gear,
                    season == input$bycatch_season)
      
      ggplot()+
        geom_raster(aes(x = x, y = y, fill =value), data = dat, na.rm=T)+
        scale_fill_viridis_d(name= "Bycatch mortality risk" ,na.value="white",labels=c("Low","Medium","High",""),option ="viridis",drop = FALSE)+
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
        scale_x_continuous(breaks= map_parameters()$coordxmap)+
        scale_y_continuous(breaks= map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")
      
    }) %>% bindCache(ecoregion(), input$bycatch_switch, input$bycatch_species, input$bycatch_gear, input$bycatch_season)
    
    output$bycatch_facet_plot <- renderPlot({
      req(filtered_data())
      
      nameFilllit <- bquote(atop(
        Predicted ~ fisheries ~ related,
        litter ~ (Numbers/km^2)
      ))
      
      ggplot()+
        geom_raster(aes(x = x, y = y, fill =value), data = filtered_data(), na.rm=T)+
        scale_fill_viridis_d(name= "Bycatch mortality risk" ,na.value="white",labels=c("Low","Medium","High",""),option ="viridis",drop = FALSE)+
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
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")+
        facet_grid(rows = vars(gear),
                   cols = vars(season))
    }) %>% bindCache(ecoregion(), input$bycatch_switch)
    
    output$bycatch_med_plot <- renderPlot({
      if(ecoregion() %in% c("mediterranean", "central_mediterranean", "eastern_mediterranean")) {
        nameFillgsa18 <- c("Bycatch mortality risk of longnose spurdog to OTB gear in GSA 18")
        nameFillgsa20 <- c("Exposure of bull ray to LLS gear in GSA 20")
        
        ggplot()+
          geom_sf(aes(fill = R3.score),col=NA,data = data[[1]],na.rm=T)+
          scale_fill_viridis_d(name= stringr::str_wrap(nameFillgsa18,25) ,na.value="white",labels=c("Low","Medium","High",""),option ="plasma",drop = FALSE) +
          new_scale_fill() +
          geom_raster(aes(x = x, y = y, fill =T4_2_MPO_LLS_ex_score_GSA20),data = data[[2]],na.rm=T)+
          scale_fill_viridis_c(option="plasma",na.value = NA, name = stringr::str_wrap(nameFillgsa20,25),direction=-1)+
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
      }
    }) %>% bindCache(ecoregion(), input$bycatch_switch)
    
    output$bycatch_ww_shearwater_plot <- renderPlot({
      
      nameFillshearwater <- c("Bycatch mortality risk of balearic shearwater to longlines in the Bay of Biscay")
      ggplot() +
        geom_sf(aes(fill = z),col=NA,data = data$shearwater,na.rm=T)+
          scale_fill_viridis_d(name= stringr::str_wrap(nameFillshearwater,25) ,na.value="white",labels=c("Low","Medium","High",""),option ="viridis",drop = FALSE) +
          geom_sf(data=land,col=NA,fill="grey")+
          # theme_classic()+
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
        
    })
    output$bycatch_ww_cetacean_plot <- renderPlot({
      nameFillcetacean <- c("Bycatch mortality risk of harbour porpoise to netters in Irish waters in summer")
     
      ggplot() +
          geom_sf(aes(fill = R1.score),col=NA,data = data$cetacean,na.rm=T)+
          scale_fill_viridis_d(na.value="white",labels=c("Low","Medium","High",""),option ="plasma",drop = FALSE,name = stringr::str_wrap(nameFillcetacean,25)) +
          geom_sf(data=land,col=NA,fill="grey")+
          # theme_classic()+
          theme(plot.background=element_blank(),
                panel.background=element_blank(),
                axis.text.y   = element_text(size=16),
                axis.text.x   = element_text(size=16),
                axis.title.y  = element_text(size=16),
                axis.title.x  = element_text(size=16),
                panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
                legend.text   = element_text(size=11),
                legend.title  = element_text(size=11))+
          scale_x_continuous(breaks=map_parameters()$coordxmap$coordxmap_cs)+
          scale_y_continuous(breaks=map_parameters()$coordxmap$coordymap_cs,expand=c(0,0))+
          coord_sf(xlim=c(map_parameters()$coordslim$coordslim_cs[1], map_parameters()$coordslim$coordslim_cs[2]), 
                   ylim=c(map_parameters()$coordslim$coordslim_cs[3],map_parameters()$coordslim$coordslim_cs[4]))+
          ylab("Latitude")+
          xlab("Longitude")
    })
  })
}

## To be copied in the UI
# mod_bycatch_ui("bycatch_1")

## To be copied in the server
# mod_bycatch_server("bycatch_1")

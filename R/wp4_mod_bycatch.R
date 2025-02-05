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
    uiOutput(ns("bycatch_selection_panel")),
    uiOutput(ns("bycatch_main_panel"))
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
            selectInput(ns("bycatch_season"), "Select bycatch risk season", choices = unique(data$season)),
            selectInput(ns("bycatch_gear"), "Select bycatch gear season", choices = unique(data$gear)),
            card(withSpinner(plotOutput(ns("bycatch_plot"), height = "75vh")))
          )
        } else {
          card(withSpinner(plotOutput(ns("bycatch_facet_plot"), height = "75vh")))
        }
      } else if(ecoregion()=="mediterranean"){
        card(withSpinner(plotOutput(ns("bycatch_med_plot"), height = "75vh")))
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
      
    })
    
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
    })
    
    output$bycatch_med_plot <- renderPlot({
      if(ecoregion() == "mediterranean") {
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
    })
  })
}

## To be copied in the UI
# mod_bycatch_ui("bycatch_1")

## To be copied in the server
# mod_bycatch_server("bycatch_1")

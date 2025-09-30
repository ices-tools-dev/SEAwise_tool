#' context UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_context_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("context_modal"), "Background", width = "175px")
  )
}
    
#' context Server Functions
#'
#' @noRd 
mod_context_server <- function(id, active_tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
   
    context <- reactive({
      req(!is.null(active_tab))
      
      texts <- context_texts[context_texts[["section"]] == active_tab,]$text
    })
    
    observeEvent(input$context_modal, {
      req(!is.null(context()))
      showModal(
        modalDialog(size = "xl",
                    title = "Behind the results:",
                    HTML(context()),
                    easyClose = TRUE,
                    footer = modalButton("Close")
        )
      )
    })
  })
}
    
## To be copied in the UI
# mod_context_ui("context_1")
    
## To be copied in the server
# mod_context_server("context_1")

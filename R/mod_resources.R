#' resources UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card card_header
#' 
mod_resources_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(card_header("Contact"),
         HTML('<p>For questions and feedback relating to the SEAwise EBFM toolbox: <a href="mailto:neil.maginnis@ices.dk">neil.maginnis@ices.dk</a></p>')),
    card(card_header("Application License"),
         tags$p("The SEAwise EBFM toolbox is distributed under the MIT License (MIT). Copyright © 2025 ICES."),
         tags$p(tags$b("Recommended citation:"), "SEAwise EBFM Toolbox, [date accessed]. ICES, Copenhagen, Denmark.",
            tags$a("[web address]"
              ,href =  "https://www.ices.dk/"
            )),
         ),
    card(card_header("Data License"),
         tags$p(
           "The SEAwise EBFM toolbox adheres to the ",
           tags$a(
             "ICES data policy.",
             href   = "https://ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx",
             target = "_blank", 
             rel    = "noopener noreferrer"
           )
         ),
         tags$p(
           "The application's source code is available on ",
           tags$a(
             "GitHub.",
             href   = 'https://github.com/ices-tools-dev/SEAwise_tool',
             target = "_blank", 
             rel    = "noopener noreferrer"
           )
         )
    ),
    card(
      card_header("Useful Links"),
      tags$p(
        "The science underpinning the results presented here is fully described in the ",
        tags$a(
          "SEAwise deliverable reports.",
          href   = "https://seawiseproject.org/seawise-results/",
          target = "_blank", 
          rel    = "noopener noreferrer"
        )
      ),
      tags$p(
        "View the metadata record for the SEAwise EBFM tool ",
        tags$a(
          "here.",
          href   = "https://gis.ices.dk/geonetwork/srv/eng/catalog.search#/metadata/973df372-de7c-4a44-ab10-f2777d5c547a",
          target = "_blank", 
          rel    = "noopener noreferrer"
        )
      )
    )
  )
}
    
#' resources Server Functions
#'
#' @noRd 
mod_resources_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_resources_ui("resources_1")
    
## To be copied in the server
# mod_resources_server("resources_1")

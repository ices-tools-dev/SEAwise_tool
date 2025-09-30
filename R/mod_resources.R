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
    card(
      card_header("Acknowledgements & Data Sources", class = "bg-primary"),
      tags$p(
        "This toolbox has been developed as part of the ",
        tags$a("SEAwise project", 
               href   = "https://seawiseproject.org/", 
               target = "_blank", rel = "noopener noreferrer"),
        ", funded by the European Union’s Horizon Europe research and innovation programme (Grant Agreement No. 101000318), and coordinated by the International Council for the Exploration of the Sea (ICES)."
      ),
      tags$p(
        "It integrates outputs from the SEAwise project. For full details on the research, see the ",
        tags$a("SEAwise deliverable reports", 
               href   = "https://seawiseproject.org/seawise-results/", 
               target = "_blank", rel = "noopener noreferrer"),
        "."
      )),
    card(card_header("Licensing and usage", class = "bg-primary"),
         tags$p(
           "The code and data in the SEAwise EBFM toolbox are available under the ",
           tags$a(
             "CC BY 4.0 license",
             href   = "https://creativecommons.org/licenses/by/4.0/",
             target = "_blank", 
             rel    = "noopener noreferrer"
           ),
         ),
         tags$p(tags$b("Recommended citation:"), "SEAwise EBFM Toolbox, [date accessed]. ICES, Copenhagen, Denmark.",
            tags$a("https://www.ices.dk/data/assessment-tools/Pages/SEAwise-EBFM-toolbox.aspx"
              ,href =  "https://www.ices.dk/data/assessment-tools/Pages/SEAwise-EBFM-toolbox.aspx"
            )),
         tags$p(
           "The application's source code is available on ",
           tags$a(
             "GitHub",
             href   = 'https://github.com/ices-tools-dev/SEAwise_tool',
             target = "_blank", 
             rel    = "noopener noreferrer"
           )
         ),
        tags$p(
          "View the metadata record for the SEAwise EBFM toolbox ",
          tags$a(
            "here",
            href   = "https://gis.ices.dk/geonetwork/srv/eng/catalog.search#/metadata/973df372-de7c-4a44-ab10-f2777d5c547a",
            target = "_blank", 
            rel    = "noopener noreferrer"
          )
        )
    ),
    card(
      card_header("Useful Links", class = "bg-primary"),
      tags$p(
        "The analysis code is available via the project's ",
        tags$a(
          "GitHub repository.",
          href   = "https://github.com/ices-tools-dev/SEAwise",
          target = "_blank", 
          rel    = "noopener noreferrer"
        )
      ),
      tags$p(
        "Visit SEAwise's",
        tags$a(
          "accessible EBFM tool",
          href   = "https://seawiseproject.org/tool/",
          target = "_blank", 
          rel    = "noopener noreferrer"
        )
      )
    ),
    card(card_header("Contact", class = "bg-primary"),
         HTML('<p>If you experience problems with the SEAwise EBFM toolbox please <a href="accessions@ices.dk">let us know</a>.</p>')),
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

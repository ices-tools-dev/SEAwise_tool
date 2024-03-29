#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom htmltools css
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    navbarPage(
      theme = seawise_theme,
      position = "static-top",
      collapsible = TRUE,
      windowTitle = "SEAwise",
      id = "tabset",
      fluid = TRUE,
      title = span(tags$img(src ="www/PRIMARY_SeaWiseLOGO_Full_Colour.png",
                            style = "padding-right:10px;padding-bottom:10px; padding-top:0px; margin-top: -10px",
                            height = "50px"), "SEAwise"),
      tabPanel("Home", mod_home_ui("home_1")
               ),
      navbarMenu(title = "About",
        tabPanel("Themes",
                 mod_themes_ui("themes_1")),
        tabPanel("Case Studies",
                 mod_case_studies_ui("case_studies_1")),
        tabPanel("SEAwise Partners"),
        tabPanel("Publications")),
      navbarMenu("Results",
        tabPanel("Social and Economic Effects of and on Fishing"),
        tabPanel("Ecological Effects on Fisheries"),
        tabPanel("Ecological Effects of Fisheries"),
        tabPanel("Spatial Management impacts on Ecological Systems and Fisheries"),
        tabPanel("Evaluation of Fisheries Management Strategies in an Ecosystem Context"),
        ),
      tabPanel("Resources")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom bslib card card_header card_body card_image layout_column_wrap
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  add_resource_path(
    "img",
    app_sys("app/img")
  )
  
  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SEAwise"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

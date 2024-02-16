#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
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
      title = span(tags$img(src ="www/PRIMARY_SeaWiseLOO_Full Colour.png",
                            style = "padding-right:10px;padding-bottom:10px; padding-top:0px; margin-top: -10px",
                            height = "50px"), "SEAwise"),
      navbarMenu(title = "About",
        tabPanel("About2"),
        tabPanel("SEAwise Partners")),
      tabPanel("Themes",
               mod_themes_ui("themes_1")),
      tabPanel("Case Studies",
               mod_case_studies_ui("case_studies_1"))
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

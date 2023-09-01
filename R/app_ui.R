#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    # Your application UI logic

      navbarPage(
      div(
        img(
          src = "www/GloUrb_wide.png",
          height = 35,
          width = 100,
          style = "margin: -15px -15px"
        )
      ),
      tabPanel("Global",mod_WP1_ui("WP1_1")),
      tabPanel("City",mod_WP3_ui("WP3_1")),
      tabPanel("Doc",
               HTML(paste0("<p> To consult the documentation associated to this app",
                           "please check <a href='https://lvaudor.github.io/glourbdoc/' ",
                           "target='_blank'>",
                           "this site</a>.")))
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

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "GloUrb"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

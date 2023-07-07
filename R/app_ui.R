#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    # Your application UI logic
    fluidPage(
      div(
          img(
            src = "www/GloUrb.png",
            height = 120,
            width = 120,
            style = "margin:-11px -12px"
          )
        ),
      mod_WP1_ui("WP1_1")
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

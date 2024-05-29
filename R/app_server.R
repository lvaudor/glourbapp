#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_WP1_server("WP1_1")
  mod_WP3_server("WP3_1")
  mod_webtext_server("webtext_1")
}

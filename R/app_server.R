#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # set database connection
  conn <- glourbi::connect_to_glourb()


  r_val <- reactiveValues(

    # UI
    main_menu_tab=NULL # selected tab in main menu

  )

  # Your application server logic
  mod_WP1_server("WP1_1")
  mod_WP3_server("WP3_1", conn)
  mod_webtext_server("webtext_1",conn)
  mod_percity_server("percity_1",conn)
  mod_WDWP_server("WDWP_1")
  mod_help_server("help_1",r_val)

  # navbarPage identifier
  observeEvent(input$main_menu_tab, {
    r_val$main_menu_tab = input$main_menu_tab
  })


  # disconnect database when closing session
  onStop(function() {
    if (!is.null(conn)) {
      DBI::dbDisconnect(conn)
    }
  })
}

#' help_guide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom cicerone use_cicerone
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_help_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("help_btn"), " Help",
                 icon = icon("circle-question")),
    cicerone::use_cicerone() # Load the cicerone dependencies
  )
}

#' help_guide Server Functions
#'
#' @importFrom cicerone Cicerone
#'
#' @noRd
mod_help_server <- function(id, r_val){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



    # Set up a dynamic observer based on the active tab
    observeEvent(input$help_btn, {

      # Reset tour steps for each new tab
      # tour$reset()

      # Define the cicerone tours for each tab
      tour <- cicerone::Cicerone$new(
        done_btn_text = "Terminer",
        close_btn_text = "Fermer",
        next_btn_text = "Suivant",
        prev_btn_text = "Précédent",
      )

      # browser()

      #| notes:
      #| <p> for paragraph, <br/> for line break
      #| <strong> for bold, <em> for italics, and <ul> for bullet points
      #| <a href="https://www.google.com">Google</a> for links
      #| <img src="www/logos_mapdo_evs_ofb.png" width="100px"> for images
      #| <code> for code snippets
      #|


      print(r_val$main_menu_tab)
      ### Exploration-Tab tour ####
      tour$step("main_menu_tab",
                "Main menu",
                description="
                <p><b>Global:</b> Explore the general characteristics of the GloUrb study sites.</p>
                <p><b>Surface Water:</b> Explore and describe the changes in Surface Water for each study site.</p>
                <p><b>Discourses:</b> Visualize the main topics surrounding all associations of city and river names (assessed using web-scraped textual contents).</p>
                <p><b>OpenStreetMap:</b> Explore and analyse landuse and equipments related to the association of city and river (assessed using OpenStreetMap data).</p>",
                position="bottom")

      if (r_val$main_menu_tab == "Global") {

          tour$step("WP1_1-global_map", "Global map of general characteristics",
                    description = "This global map displays the general characteristics of GloUrb study sites.",
                    position = "right")

          tour$step("WP1_1-global_map", "Global map of general characteristics",
                  description = "This global map displays the general characteristics of GloUrb study sites.",
                  position = "right")
        print(tour)

      }


      tour$init()$start() # Start the tour
    })
  })
}

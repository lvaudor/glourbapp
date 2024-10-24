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
                <p><b>Global:</b> </p>
                <p><b>Surface Water:</b> Explore and describe the changes in Surface Water for each study site.</p>
                <p><b>Discourses:</b> Visualize the main topics surrounding all associations of city and river names (assessed using web-scraped textual contents).</p>
                <p><b>OpenStreetMap:</b> Explore and analyse landuse and equipments related to the association of city and river (assessed using OpenStreetMap data).</p>",
                position="bottom")

      if (r_val$main_menu_tab == "Global") {
        tour$step("main_menu_tab",
                  "Global",
                  description="Explore the general characteristics of the GloUrb study sites.",
                  position="bottom")

        tour$step("mod_global_1-global_map", "Global map",
                  description = "This global map displays the general characteristics of GloUrb study sites.",
                  position = "right")

        tour$step("mod_global_1-selection", "Study sites selection",
                  description = "You can display all originally considered cities or only those in sub-selections (selection1 for now).",
                  position = "right")
        tour$step("mod_global_1-select_var", "Variable displayed on map",
                  description = "The variable displayed on the map (through color) can be either a class obtained through a clustering of all available descriptors -in which case you can choose the number of study sites clusters-, or any of the general characteristics used for the clustering.",
                  position = "right")
        tour$step("mod_global_1-mod_global_menu", "Data description",
                  description = "<p><b>city</b> The description of one city (selected through a click on the map)</p>
                                   <p><b>univar</b> The univariate distribution of the descriptors across clusters of city</p>
                                   <p><b>multivar</b> The multivariate description of cities across all quantitative descriptors (assessed through a PCA)</p>
                                   <p><b>allvar</b> The univariate description of cities across all descriptors </p>
                                   <p><b>data</b> The table of descriptors (can be downloaded).",
                  position = "right")

      }
      if (r_val$main_menu_tab == "Surface Water") {

        tour$step("mod_GSW_1-city", "Choose city",
                  description = "This module shows results for all study sites in #selection 1",
                  position = "right")

        tour$step("mod_GSW_1-map_city", "Map",
                  description = "<p>This map displays the study sites and its particular sub-areas defined through <b>longitudinal (upstream-urban-downstream)</b>
                                 and <b>lateral (corridor-plain)</b> zonation.</p>
                                 <p>The colors of layer GSW change correspond to the <b>intensity of change in surface water over the years 2000-2020 compared to years 1980-2000</b>.</p>",
                  position = "right")
        tour$step("mod_GSW_1-GSWdescription", "Surface water change intensity.",
                  description = "Number of pixels with positive (green), no change (grey), and negative change (red) in surface water, per longitudinal and lateral zone.",
                  position = "right")
      }


      tour$init()$start() # Start the tour
    })
  })
}

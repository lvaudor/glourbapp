#' mod_WP3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_WP3_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("city"),
                "Choose city",
                choices=cities,
                selected="Ahmedabad"),
    leaflet::leafletOutput(ns("map_city"),height=800)

  )
}

#' mod_WP3 Server Functions
#'
#' @noRd
mod_WP3_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$map_city=leaflet::renderLeaflet({
      city=input$city
      map_file=system.file(paste0("maps/map_",input$city,".RDS"),package="glourbapp")
      mapinfo=readRDS(system.file(paste0("mapinfo/mapinfo_",
                                         input$city,".RDS"),
                                  package="glourbapp"))
      mymap=readRDS(map_file)
      mymap=mymap %>%
        leaflet::addTiles(group = "OSM map") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                         group = "Photo") %>%
        leaflet::addLayersControl(
              overlayGroups = c("OSM map","Photo",
                                unique(mapinfo$group)),
              options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::hideGroup("Photo")
      for (i in 1:length(unique(mapinfo$value))){
        mymap=mymap %>%
          leaflet::hideGroup(mapinfo$group[i])
      }
      print(mymap)
    })
  })
}

## To be copied in the UI
# mod_WP3_ui("WP3_1")

## To be copied in the server
# mod_WP3_server("WP3_1")

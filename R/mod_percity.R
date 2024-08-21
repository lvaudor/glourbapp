#' percity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_percity_ui <- function(id){
  ns <- NS(id)

  selection1=glourbi::all_cities %>%
    dplyr::filter(selection1==TRUE) %>%
    dplyr::arrange(Urban.Aggl) %>%
    dplyr::pull(Urban.Aggl)
  tagList(
    fluidRow(
      column(width=4,
             selectInput(ns("city"),
                         "Choose city",
                         choices=selection1,
                         selected=selection1[1]),
             tableOutput(ns("GSWtable"))
      ),#column
      column(width=8,
             tabsetPanel(
               tabPanel("map",leaflet::leafletOutput(ns("map_city"),height=600)),
               tabPanel("GSW distribution",plotOutput(ns("GSWdensity"),
                                                      width="650px",height="750px"))
             )
      )#column
    )#fluidRow

  )
}

#' percity Server Functions
#'
#' @noRd
mod_percity_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    get_GSWdensity=reactive({
      GSWdensity=readRDS(system.file(
        "GSWdensity.RDS",
        package="glourbapp")) %>%
        dplyr::filter(UrbanAggl==input$city) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(reach=dplyr::case_when(reach=="upstream"~"1_upstream",
                                      reach=="downstream"~"3_downstream",
                                      reach=="urban"~"2_urban")) %>%
        dplyr::arrange(reach,zone)
      print(head(GSWdensity))
      GSWdensity
    }
    )


    output$GSWdensity=renderPlot({
      print(plot_density)
      plot_density(get_GSWdensity())
    }
    )
    output$GSWtable=renderTable({
      get_GSWdensity() %>%
        dplyr::mutate(means=purrr::map(density,"dat_means")) %>%
        dplyr::select(zone,reach,means) %>%
        tidyr::unnest(cols=c(means))
    }
    )
    output$map_city=leaflet::renderLeaflet({
      CityCode=glourbi::all_cities %>%
        dplyr::filter(Urban.Aggl==input$city) %>%
        dplyr::pull(ID)
      path=system.file("per_city/",package="glourbapp")
      shape=sf::st_read(paste0(path,CityCode,".shp")) %>%
        sf::st_transform(crs = 4326) %>%
        dplyr::mutate(reach_color=dplyr::case_when(reach=="city"~"red",
                                            reach=="upstream"~"blue",
                                            reach=="downstream"~"purple"))
      mymap=leaflet::leaflet(shape) %>%
        leaflet::addPolygons(fill=FALSE,
                             color=~reach_color) %>%
        leaflet::addTiles(group = "OSM map") %>%
        leaflet::addWMSTiles(
          baseUrl = "https://sedac.ciesin.columbia.edu/geoserver/wms",
          layers = "gpw-v4:gpw-v4-population-density-rev11_2020",
          options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
          attribution = "SEDAC pop density V4",
          group="PopDensity_SEDAC"
        ) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                  group = "Photo") %>%
        leaflet::addTiles(
          urlTemplate = "https://storage.googleapis.com/global-surface-water/tiles2021/change/{z}/{x}/{y}.png",
          attribution = "2016 EC JRC/Google",
          group="Global Water Surface") %>%
        leaflet::addLayersControl(
          baseGroups=c("OSM map","PopDensity_SEDAC","Photo"),
          overlayGroups = c("Global Water Surface"),
          options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::hideGroup("Photo") %>%
        leaflet::hideGroup("PopDensity_SEDAC")
      print(mymap)
    })

  })
}

## To be copied in the UI
# mod_percity_ui("percity_1")

## To be copied in the server
# mod_percity_server("percity_1")

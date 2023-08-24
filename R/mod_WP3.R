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
    fluidRow(
      column(width=4,
             selectInput(ns("city"),
                         "Choose city",
                         choices=cities,
                         selected="Ahmedabad"),
             checkboxGroupInput(ns("group"),
                         "Choose type",
                         choices=unique(map_elems_global$group)),
             plotOutput(ns("plot_osmglobal"))
             ),#column
      column(width=8,
             leaflet::leafletOutput(ns("map_city"),height=800)
             )#column
    )#fluidRow

  )
}

#' mod_WP3 Server Functions
#'
#' @noRd
mod_WP3_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    get_mapinfo=reactive({
      mapinfo=readRDS(system.file(
        paste0("mapinfo/mapinfo_",input$city,".RDS"),
        package="glourbapp")) %>%
        dplyr::filter(nelems>0)
      }
    )
    output$plot_osmglobal=renderPlot({
      mapinfo=get_mapinfo() %>%
        dplyr::filter(group %in% input$group)
      colorscale=mapinfo %>%
        dplyr::select(key,value,color) %>%
        unique()
      colorscale_vec=colorscale$color
      names(colorscale_vec)=colorscale$value
      ggplot2::ggplot(mapinfo,
                      ggplot2::aes(x=forcats::fct_reorder(value,group),y=nelems,fill=value, group=group))+
        ggplot2::geom_bar(stat="identity",color="dark grey")+
        ggplot2::geom_text(ggplot2::aes(y=0, label=value),
                           hjust = 0, nudge_x = 0.1)+
        ggplot2::coord_flip()+
        ggplot2::scale_fill_manual(values=colorscale_vec)+
        ggplot2::theme(legend.position="none")+
        ggplot2::scale_y_sqrt()+
        ggplot2::scale_x_discrete(labels=mapinfo$group)+
        ggplot2::xlab("")
    })
    output$map_city=leaflet::renderLeaflet({
      shape=  readRDS(system.file(
                        paste0("shapes/shape_",input$city,".RDS"),
                        package="glourbapp"))
      mymap=leaflet::leaflet(shape) %>%
        leaflet::addPolygons(fill=FALSE,color="red")
      mymap=mymap %>%
        leaflet::addTiles(group = "OSM map") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                         group = "Photo") %>%
        leaflet::addTiles(
          urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
          attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
          options = leaflet::tileOptions(variant='neighbourhood', apikey = Sys.getenv("thunderforest_API_KEY")),
          group="Neighbourhoods"
        ) %>%
        leaflet::addLayersControl(
              overlayGroups = c("OSM map","Photo","Neighbourhoods"),
              options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::hideGroup("Photo") %>%
        leaflet::hideGroup("Neighbourhoods")
        print(mymap)
    })

    observe({
      mapinfo=get_mapinfo()
      mymap=leaflet::leafletProxy(ns("map_city"))
      if(!is.null(input$group)){
      for (i in 1:nrow(mapinfo)){
        if(mapinfo$group[i] %in% input$group){
          mymap=mymap %>%
          add_to_map(mapinfo$osmdata[i][[1]],
                     color=mapinfo$color[i],
                     layergroup=mapinfo$value[i]
                    )
        }
        if(!(mapinfo$group[i] %in% input$group)){
        mymap=mymap %>%
          leaflet::clearGroup(mapinfo$value[i])
        }
      }# end for loop
      }
    })
  })
}

## To be copied in the UI
# mod_WP3_ui("WP3_1")

## To be copied in the server
# mod_WP3_server("WP3_1")

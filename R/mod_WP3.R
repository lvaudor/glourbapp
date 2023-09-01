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
                         choices=glourbapp::cities,
                         selected="Ahmedabad"),
             checkboxInput(ns("wikidata"),
                           "Show Wikidata",
                           value=FALSE),
             checkboxGroupInput(ns("group"),
                         "OSM: Choose type",
                         choices=unique(glourbapp::map_elems_global$group)),
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
        package="glourbapp"))
      }
    )

    get_mapinfo_group=reactive({
      mapinfo=get_mapinfo() %>%
        dplyr::filter(nelems>0)%>%
        dplyr::filter(group %in% input$group)
    }
    )
    output$plot_osmglobal=renderPlot({
      if(length(input$group)>0){
        mapinfo=get_mapinfo_group()
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
        }
    })
    output$map_city=leaflet::renderLeaflet({
      shape=  readRDS(system.file(
                        paste0("shapes/shape_",input$city,".RDS"),
                        package="glourbapp"))
      mymap=leaflet::leaflet(shape) %>%
        leaflet::addPolygons(fill=FALSE,color="red") %>%
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

    observeEvent(input$group,{
      mapinfo=get_mapinfo()
      mymap=leaflet::leafletProxy(ns("map_city"))
     # if(nrow(mapinfo)>1){
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
    #}#end if

    })
  observeEvent(input$wikidata,{
    mymap=leaflet::leafletProxy(ns("map_city"))
    if(input$wikidata){
      mymap=mymap%>%
        leaflet::addCircleMarkers(data=glourbapp::tib_wd_elems %>%
                                    dplyr::filter(name==input$city),
                                  popup=~popup,
                                  color=~color,
                                  group="wikidata")
    }
    if(!input$wikidata){
      mymap=mymap %>%
        leaflet::clearGroup("wikidata")
    }
  })

  })
}

## To be copied in the UI
# mod_WP3_ui("WP3_1")

## To be copied in the server
# mod_WP3_server("WP3_1")

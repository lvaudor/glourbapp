#' WP1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_WP1_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    tabsetPanel(
      tabPanel("viz",
               fluidRow(
                    column(width=3,
                           wellPanel(
                           selectInput(ns("select_var"),
                                       label="choose variable",
                                       selected="cluster",
                                       choices=c("cluster",varsdistrib)),
                           textOutput(ns("description_var"))
                           ),#wellPanel
                           wellPanel(p("When considering classes:"),
                                     numericInput(ns("nclust"),
                                         "Number of classes",
                                          min=2,max=30, value=2),
                                     checkboxInput(ns("distrib_by_class"),
                                                 "display distribution by class")
                                     ),#wellPanel
                           ),#column
                    column(width=7,
                           plotOutput(ns("plot_distrib"))
                           ),#column
                    column(width=2,
                           plotOutput(ns("plot_palette"))
                    ),#column
               ),
               leaflet::leafletOutput(ns("global_map"))
    ),#tabPanel
    tabPanel("PCA",

           fluidRow(
             column(width=6,
                    plotOutput(ns("varpcaplot"))),
             column(width=6,
                    plotOutput(ns("indpcaplot")))
           )#fluidRow
    ),#tabPanel
    tabPanel("data",
             dataTableOutput(ns("tableclust"))
    )#tabPanel
  )#tabsetPanel

  )#tagList
}

#' WP1 Server Functions
#'
#' @noRd
mod_WP1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r_all_cities=reactive({
      glourbi::run_hclust(all_cities, nclust=input$nclust)
    })
    output$tableclust=renderDataTable({
      r_all_cities()
    })
    output$description_var=renderText({
      meta_all_cities %>%
        dplyr::filter(varname==input$select_var) %>%
        dplyr::select(description) %>%
        dplyr::pull()
    })
    r_calc_pca=reactive({
      all_cities_clust=r_all_cities()
      mypca=glourbi::run_pca(all_cities_clust,quali.sup="cluster")
    })
    output$varpcaplot=renderPlot({
      mypca=r_calc_pca()
      glourbi::plot_pca(mypca,type="var")
    })
    output$indpcaplot=renderPlot({
      mypca=r_calc_pca()
      glourbi::plot_pca(mypca,type="ind")
    })
    output$global_map=leaflet::renderLeaflet({
      glourbi::global_map(dataset=r_all_cities(),
                          varname=input$select_var)
    })
    output$plot_palette=renderPlot({
      glourbi::plot_palette(dataset=r_all_cities(),
                            varname=input$select_var)
    })
    output$plot_distrib=renderPlot({
      glourbi::plot_distrib(dataset=r_all_cities(),
                            varname=input$select_var,
                            byclass=input$distrib_by_class)
    })
  })
}

## To be copied in the UI
# mod_WP1_ui("WP1_1")

## To be copied in the server
# mod_WP1_server("WP1_1")

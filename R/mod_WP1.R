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
    fluidRow(column(width=3,
                    radioButtons(ns("selection"),
                                 "cities in selection",
                                 c("selection 0","selection 1"),
                                 selected="selection 0"),
                    wellPanel(
                      selectInput(ns("select_var"),
                                  label="choose variable",
                                  selected="cluster",
                                  choices=c("cluster",
                                            c(glourbi::sep_vars(glourbi::all_cities)$vars_cat,
                                              glourbi::sep_vars(glourbi::all_cities)$vars_num))),
                      textOutput(ns("description_var")),
                      conditionalPanel(
                        condition = "input.select_var == 'cluster'",ns=ns,
                        numericInput(ns("nclust"),
                                     "cluster: nb of classes",
                                     min=2,max=30, value=2)
                      )
                    ),#wellPanel
                    plotOutput(ns("plot_palette"))
                    ),#column
                    column(width=9,
                           leaflet::leafletOutput(ns("global_map")),
                           tabsetPanel(
                             tabPanel("city",
                                      fluidRow(
                                        column(width=6,
                                               "Choose a city by clicking on the map",
                                               uiOutput(ns("city_name"))
                                               ),#column
                                        column(width=6,
                                               plotOutput(ns("plot_city")))#column
                                      )#fluidRow

                             ),#tabPanel
                             tabPanel("univar",
                                      checkboxInput(ns("distrib_by_class"),
                                                    "display univar distribution by cluster"),
                                      plotOutput(ns("plot_distrib"))
                             ),#tabPanel
                             tabPanel("multivar",
                                      fluidRow(
                                        column(width=6,
                                               plotly::plotlyOutput(ns("varpcaplot"))),
                                        column(width=6,
                                               plotly::plotlyOutput(ns("indpcaplot")))
                                      )#fluidRow
                             ),#tabPanel
                             tabPanel("boxplots",
                                      checkboxInput(ns("display_ranks"),
                                                    label="display ranks",
                                                    value=TRUE),
                                      plotOutput(ns("description_clusters"))
                             ),#tabPanel
                             tabPanel("data",
                                      downloadButton(ns("download_btn"), "Download this data"),
                                      DT::dataTableOutput(ns("tableclust"))
                             ),#tabPanel
                             tabPanel("test",
                                      textOutput(ns("test"))
                             )#tabPanel
                           )#tabsetPanel
                    )#column
    )#fluidRow
  )#tagList
}

#' WP1 Server Functions
#'
#' @noRd
mod_WP1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r_all_cities=reactive({
      input$nclust
      input$selection
      dataset=glourbi::run_hclust(glourbi::all_cities, nclust=input$nclust)
      if(input$selection=="selection 0"){dataset=dataset}
      if(input$selection=="selection 1"){dataset=dataset %>% dplyr::filter(selection1==TRUE)}
      dataset
    })
    r_get_city=reactive({
      if(is.null(input$global_map_marker_click$id)){
        clickid="Lyon--France"}else{
        clickid=input$global_map_marker_click$id}
      clickid
    })
    output$city_name=renderUI({h2(r_get_city())})
    output$tableclust=DT::renderDT({
      r_all_cities()
    })
    output$description_var=renderText({
      glourbi::meta_all_cities %>%
        dplyr::filter(varname==input$select_var) %>%
        dplyr::select(description) %>%
        dplyr::pull()
    })
    r_calc_pca=reactive({
      all_cities_clust=r_all_cities()
      mypca=glourbi::run_pca(all_cities_clust,quali.sup=input$select_var)
    })
    output$varpcaplot=plotly::renderPlotly({
      glourbi::plot_pca(dataset=r_all_cities(),
                        r_calc_pca(),
                        type="var")
    })
    output$indpcaplot=plotly::renderPlotly({
      print(colnames(r_all_cities()))
      glourbi::plot_pca(dataset=r_all_cities(),
                        r_calc_pca(),
                        type="ind")
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
    output$plot_city=renderPlot({
      glourbi::describe_city(dataset=r_all_cities(),
                             r_get_city())
    })
    output$description_clusters=renderPlot({
      glourbi::describe_clusters(dataset=r_all_cities(),
                                 display_ranks=input$display_ranks)
    })
    output$test=renderText({
      r_get_city()
    })
    output$download_btn <- downloadHandler(
      filename = function() {
        "GloUrb_WP1_table.csv"
      },
      content = function(file) {
        write.csv(r_all_cities(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_WP1_ui("WP1_1")

## To be copied in the server
# mod_WP1_server("WP1_1")

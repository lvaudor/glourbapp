#' webtext UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_webtext_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=4,
             selectInput(ns("city"),
                         "Choose city",
                         choices=glourbapp::city_river$city,
                         selected="Ahmedabad"),
              plotOutput(ns("plotwords"))
   ),#column1
   column(width=8,
          DT::dataTableOutput(ns("tib_cityriver"))
   )#column2
    )#fluidRow



  )
}

#' webtext Server Functions
#'
#' @noRd
mod_webtext_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    get_cityriver_urls=reactive({
      path=system.file("data_serp_country/",package="glourbapp")
      path=paste0(path,"tib_",input$city,".csv")
      tib=readr::read_csv(path,show_col_types = FALSE)
      tib=tib[,1:7]
    })
    get_cityriver_words=reactive({
      path=system.file("data_words_country/",package="glourbapp")
      path=paste0(path,"tib_",input$city,".csv")
      tib=readr::read_csv(path,show_col_types = FALSE)
    })
    output$plotwords=renderPlot({
      tib_words=get_cityriver_words()
      tib_words_plot=tib_words %>%
        dplyr::group_by(geoname) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::slice_head(n=15)
      ggplot2::ggplot(tib_words_plot,
                      ggplot2::aes(x=forcats::fct_reorder(word,n), y=n, fill=geoname))+
        ggplot2::geom_bar(stat="identity")+
        ggplot2::coord_flip()+
        ggplot2::xlab("lemma")+
        ggplot2::ylab("frequency")

    })
    output$tib_cityriver=DT::renderDT({
      get_cityriver_urls() %>%
        dplyr::mutate(link=glue::glue("<a href='{link}' target='_blank'>{link}</a>"))
    },escape=FALSE)
  })
}

## To be copied in the UI
# mod_webtext_ui("webtext_1")

## To be copied in the server
# mod_webtext_server("webtext_1")

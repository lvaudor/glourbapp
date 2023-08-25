#remotes::install_github("lvaudor/glourbi")
library(shiny)
shinyApp(ui=glourbapp:::app_ui,server=glourbapp:::app_server)

# app.R
# Este archivo simplemente carga la UI y el Server
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
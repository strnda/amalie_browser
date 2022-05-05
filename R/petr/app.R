library(shiny)
source("./R/petr/ui.R")
source("./R/petr/server.R")
options(shiny.sanitize.errors = FALSE)
shinyApp(ui = ui, server = server)


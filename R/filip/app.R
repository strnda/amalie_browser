library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(ggplot2)
library(fst)
library(DT)

ui <- fluidPage(
  
  # theme = bs_theme(
  #   bg = "#0b3d91", 
  #   fg = "white", 
  #   primary = "#FCC780",
  #   base_font = font_google("Space Mono"),
  #   code_font = font_google("Space Mono")
  # ),
  
  title = 'shiny',
  
  # titlePanel(title = 'title placeholder'),
  
  fluidPage(
    column(width = 5,
           leafletOutput(outputId = 'map',
                         height = 500),
           dataTableOutput(outputId = "summary_table")),
    column(width = 7,
           # textOutput(outputId = "click"),
           plotOutput(outputId = "plot",
                      height = 975))
  )
)

server <- function(input, output, session) {
  
  hru <- st_read(dsn = "./data/hru_info.shp", 
                 quiet = TRUE)
  tmst <- fread(input = "./data/soil_moisture_sensor_info.csv")
  dendro <- fread(input = "./data/dendrometer_info.csv")
  # vrty <- fread(input = "./data/vrty_info.csv")
  # vrty <- vrty[grep(pattern = "0",
  #                   x = name,
  #                   invert = TRUE),]
  mikroklima <- fread(input = "./data/mikroklima.csv")
  eddy <- fread(input = "./data/eddy.csv")
  vrty <- read_fst(path = "./data/vrty_info.fst")
  
  nfo_sensors <- as.data.table(read_fst(path = "./data/nfo_sensors.fst"))
  
  vrty_dta <- as.data.table(read_fst(path = "./data/vrty.fst"))
  vlhkost_dta <- as.data.table(read_fst(path = "./data/vlhkost_od_lukase.fst"))
  dendro_dta <- as.data.table(read_fst(path = "./data/dendro.fst"))
  mikroklima_dta <- as.data.table(read_fst(path = "./data/mikroklima.fst"))
  eddy_dta <- as.data.table(read_fst(path = "./data/eddy.fst"))
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles(group = "Podkladová mapa") %>%
      addWMSTiles(baseUrl = "http://geoportal.cuzk.cz/WMS_ORTOFOTO_PUB/WMService.aspx",
                  options = WMSTileOptions(format = "image/png",
                                           crs = "EPSG:102066"),
                  layers = "GR_ORTFOTORGB",
                  group = "Ortofoto") %>%
      addMarkers(data = vrty,
                 lng = ~Y,
                 lat = ~X,
                 layerId = ~ID,
                 label = paste("Vrt:",
                               vrty$name),
                 clusterOptions = markerClusterOptions(),
                 group = "Vrty") %>% 
      addMarkers(data = mikroklima,
                 lng = ~Y,
                 lat = ~X,
                 layerId = ~ID,
                 label = mikroklima$name,
                 clusterOptions = markerClusterOptions(),
                 group = "Mikroklima") %>% 
      addMarkers(data = eddy,
                 lng = ~Y,
                 lat = ~X,
                 layerId = ~ID,
                 label = eddy$name,
                 clusterOptions = markerClusterOptions(),
                 group = 'Stanice "Lihovar"') %>% 
      addMarkers(data = tmst,
                 lng = ~X,
                 lat = ~Y,
                 layerId = ~ID,
                 label = paste("Vlkhostní senzor:",
                               tmst$ID),
                 clusterOptions = markerClusterOptions(),
                 group = "Vlhkostní senzory") %>% 
      addMarkers(data = dendro,
                 lng = ~X,
                 lat = ~Y,
                 layerId = ~ID,
                 label = paste("Dendrometr:",
                               dendro$ID),
                 clusterOptions = markerClusterOptions(),
                 group = "Dendrometry") %>% 
      addPolygons(data = hru,
                  label = ~paste(ID, 
                                 as.character(x = OBJECTID)),
                  popup = paste(
                    "<u><b>Informace o HRU:</b></u>",
                    paste("<b>Land use/cover:</b> ", hru$Land_Use),
                    paste("<b>Plocha:</b> ", round(x = hru$Area, 
                                                   digits = 2), 
                          "m<sup>2</sup>"),
                    paste("<b>Sklonitost:</b> ", round(x = hru$Slope, 
                                                       digits = 2)),
                    sep = "</br>"
                  ),
                  fillColor = "#0EFCAA",
                  color = "#05A870",
                  weight = 1.75,
                  highlight = highlightOptions(
                    weight = 2.5,
                    fillOpacity = 0.69),
                  group = "Polygony hydr. jednotek") %>%
      addLayersControl(baseGroups = c("Podkladová mapa", 
                                      "Ortofoto"),
                       overlayGroups = c("Polygony hydr. jednotek", 
                                         "Vrty",
                                         "Vlhkostní senzory",
                                         "Dendrometry",
                                         "Mikroklima",
                                         'Stanice "Lihovar"'),
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  observe({
    
    click <- input$map_marker_click
    
    if(is.null(x = click)){
      
      return()
    } else {
      
      # output$click <- renderPrint({
      #   
      #   print(list(X = click$id,
      #              senzor = nfo_sensors[which(x = (ID == click$id)), senzor]))
      #   
      # })
      # 
      # "tmst"       "dendro"     "mikroklima" "eddy"       "vrt" 
      
      sens_click <- nfo_sensors[which(x = (ID == click$id)), senzor]
      
      if (sens_click == "tmst") {
        
        dta_plot <- vlhkost_dta[ID == click$id,]
      }
      
      if (sens_click == "dendro") {
        
        dta_plot <- dendro_dta[ID == click$id,]
      }
      
      if (sens_click == "mikroklima") {
        
        dta_plot <- mikroklima_dta[ID == click$id,]
      }
      
      if (sens_click == "eddy") {
        
        dta_plot <- eddy_dta[ID == click$id,]
      }
      
      if (sens_click == "vrt") {
        
        dta_plot <- vrty_dta[ID == click$id,]
      }
      
      output$plot <- renderPlot({
        
        ggplot(data = dta_plot) +
          geom_line(mapping = aes(x = date,
                                  y = value,
                                  group = variable)) +
          facet_wrap(facets = ~variable,
                     scales = "free", 
                     ncol = 1) +
          theme_bw()
      })
      
      stat <- dta_plot[, .(`Průměr` = mean(x = value, 
                                           na.rm = TRUE),
                           `Sm. odchylka` = sd(x = value, 
                                               na.rm = TRUE),
                           `Koef. variace` = mean(x = value, 
                                                  na.rm = TRUE) / sd(x = value, 
                                                                     na.rm = TRUE),
                           
                           `Minimum` = min(x = value, 
                                           na.rm = TRUE),
                           `Maximum` = max(x = value, 
                                           na.rm = TRUE),
                           `1. kvartil` = quantile(x = value, 
                                                   probs = .25, 
                                                   na.rm = TRUE),
                           `Medián` = quantile(x = value, 
                                               probs = .5, 
                                               na.rm = TRUE),
                           `3. kvartil` = quantile(x = value, 
                                                   probs = .75, 
                                                   na.rm = TRUE),
                           `Mezikvar. rozpětí` = IQR(x = value,
                                                     na.rm = TRUE)),
                       by = .(`Veličina` = variable)]
      
      stat_t <- as.data.frame(x = round(x = t(x = stat[, -1]),
                                        digits = 2),
                              keep.rownames = TRUE)
      
      names(x = stat_t) <- c(stat$`Veličina`)
      
      output$summary_table <- renderDataTable({
        stat_t
      },
      options = list(scrollX = TRUE,
                     paging = FALSE,
                     bFilter = FALSE,
                     bInfo = FALSE,
                     # buttons = c('csv', 'excel'),
                     server = FALSE))
    }
  })
}

shinyApp(ui = ui, 
         server = server)

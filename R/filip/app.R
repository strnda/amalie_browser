library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  
  theme = bs_theme(
    bg = "#0b3d91", 
    fg = "white", 
    primary = "#FCC780",
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")
  ),
  
  title = 'shiny',
  
  titlePanel(title = 'title placeholder'),
  
  fluidRow(
    column(5,
           leafletOutput(outputId = 'map')),
    column(7,
           textOutput(outputId = "click"),
           plotOutput(outputId = "plot"))
  )
)

server <- function(input, output, session) {
  
  hru <- st_read(dsn = "./data/hru_info.shp", 
                 quiet = TRUE)
  
  tmst <- fread(input = "./data/soil_moisture_sensor_info.csv")
  
  dendro <- fread(input = "./data/dendrometer_info.csv")
  
  vrty <- fread(input = "./data/vrty_info.csv")
  # vrty <- vrty[grep(pattern = "0",
  #                   x = name,
  #                   invert = TRUE),]
  
  mikroklima <- fread(input = "./data/mikroklima.csv")
  
  eddy <- fread(input = "./data/eddy.csv")
  
  nfo_sensors <- fread(input = "./data/nfo_sensors.csv")

  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles(group = "Podkladová mapa") %>%
      addWMSTiles(baseUrl = "http://geoportal.cuzk.cz/WMS_ORTOFOTO_PUB/WMService.aspx",
                  options = WMSTileOptions(format = "image/png",
                                           crs = "EPSG:102066"),
                  layers = "GR_ORTFOTORGB",
                  group = "Ortofoto") %>%
      addMarkers(data = vrty,
                 lng = ~X,
                 lat = ~Y,
                 layerId = ~name,
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
                 label = paste("Vlkhost senzor:",
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
    
      output$click <- renderPrint({
        
        print(list(X = click$id,
                   senzor = nfo_sensors[which(x = (ID == click$id)), senzor]))

      })
      
      output$plot <- renderPlot({
        
        df <- data.frame(
          gp = factor(rep(letters[1:3], each = 10)),
          y = rnorm(30)
        )
        ds <- do.call(rbind, lapply(split(df, df$gp), function(d) {
          data.frame(mean = mean(d$y), sd = sd(d$y), gp = d$gp)
        }))
        
        ggplot() +
          geom_point(data = df, aes(gp, y)) +
          geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3) +
          labs(title = nfo_sensors[which(x = (ID == click$id)), senzor]) +
          geom_errorbar(
            data = ds,
            aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
            colour = 'red',
            width = 0.4
          ) +
          theme_bw()
        })
    }
  })
}

shinyApp(ui = ui, 
         server = server)

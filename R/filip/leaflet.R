library(sf); library(leaflet); library(data.table)

hru <- st_read(dsn = "./data/hru_info.shp", 
               quiet = TRUE)
tmst <- fread(input = "./data/soil_moisture_sensor_info.csv")
dendro <- fread(input = "./data/dendrometer_info.csv")
vrty <- fread(input = "./data/vrty_info.csv")
vrty <- vrty[grep(pattern = "0",
                  x = name,
                  invert = TRUE),]
mikroklima <- fread(input = "./data/mikroklima.csv")
eddy <- fread(input = "./data/eddy.csv")

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
             layerId = ~name,
             label = mikroklima$name,
             clusterOptions = markerClusterOptions(),
             group = "Mikroklima") %>% 
  addMarkers(data = eddy,
             lng = ~Y,
             lat = ~X,
             layerId = ~name,
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



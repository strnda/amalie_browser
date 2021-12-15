library(sf); library(leaflet); library(data.table)

hru <- st_read(dsn = "./data/hru_info.shp")
tmst <- fread(input = "./data/soil_moisture_sensor_info.csv")
dendro <- fread(input = "./data/dendrometer_info.csv")
vrty <- fread(input = "./data/vrty_info.csv")
vrty <- vrty[vrty$Stav == "Ano",]


head(x = hru)

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
             label = paste("Vrt:",
                           vrty$name),
             clusterOptions = markerClusterOptions(),
             group = "Vrty") %>% 
  addMarkers(data = tmst,
             lng = ~X,
             lat = ~Y,
             label = paste("Vlkhost senzor:",
                           tmst$serial_number),
             clusterOptions = markerClusterOptions(),
             group = "Vlhkostní senzory") %>% 
  addMarkers(data = dendro,
             lng = ~X,
             lat = ~Y,
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
                                     "Dendrometry"),
                   options = layersControlOptions(collapsed = TRUE))


# fls <- list.files(path = "/media/phill/Extreme SSD/ups/", 
#                   pattern = ".shp", 
#                   full.names = TRUE, 
#                   recursive = TRUE)
# fls <- fls[grep(pattern = "xml|sr.lock", 
#                 invert = TRUE,
#                 x = fls)]
# 
# shp_dta <- lapply(X = fls, 
#                   FUN = st_read)
# shp_dta <- lapply(X = shp_dta, 
#                   FUN = st_transform,
#                   crs = 4326)
# nfo <- sapply(X = shp_dta,
#               FUN = names)
# do.call(what = cbind, 
#         args = nfo)
# 
# plot(shp_dta[[1]])
# 
# st_transform(x = shp_dta[[1]], 
#              crs = 4326)
# 
# 
# 
# tmst_1 <- read_excel(path = "data_raw/databaze TOMST BP+KL.xlsx",
#                      sheet = "sensors", 
#                      skip = 1)
# tmst_2 <- read_excel(path = "./data_raw/databaze TOMST IGA_29_11_21_l_uprava.xlsx",
#                      sheet = "sensors", 
#                      skip = 1)
#                      
## zacatek mereni cerven 2021
# library(jsonlite)
# x <- read_json(path = "https://user:ARQwAe8EBW3kJV3v@api.nod.czu.bluebeatle.cz/data/all?from=2021-07-18&to=2021-07-22")
# 
# head(x)
# 
# y <- do.call(rbind,
#              x)
# head(y)

vrty_1 <- st_read(dsn = "~/Desktop/Fila_vrty/Melke_vrty_Rago.shp")
vrty_2 <- st_read(dsn = "~/Desktop/Fila_vrty/Vrty_2021_verzeV.shp")
vrty_2 <- st_transform(x = vrty_2,
                       crs = 4326)

vrty_2 <- cbind(vrty_2, st_coordinates(x = vrty_2))

fwrite(x = vrty_2[, c("name", "Stav", "LandUse", "X", "Y")],
       file = "./data/vrty_info.csv")


plot(vrty_2)

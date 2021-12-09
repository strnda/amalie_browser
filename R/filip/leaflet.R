library(sf); library(leaflet); library(data.table)

hru <- st_read(dsn = "./data/hru_info.shp")
tmst <- fread(input = "./data/soil_moisture_sensor_info.csv")
dendro <- fread(input = "data/dendrometer_info.csv")
head(x = hru)

leaflet() %>% 
  addTiles() %>%
  addWMSTiles(baseUrl = "http://geoportal.cuzk.cz/WMS_ORTOFOTO_PUB/WMService.aspx",
              options = WMSTileOptions(format = "image/png",
                                       crs = "EPSG:102066"),
              layers = "GR_ORTFOTORGB") %>%
  addMarkers(data = tmst,
             lng = ~X,
             lat = ~Y,
             label = paste("Vlkhost půdy",
                           "</br>",
                           "ID senzoru:",
                           tmst$serial_number)) %>% 
  addMarkers(data = dendro,
             lng = ~X,
             lat = ~Y,
             label = paste("Dendrometr",
                           "</br>",
                           "ID senzoru:",
                           dendro$ID)) %>% 
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
                fillOpacity = 0.69)
  )

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

gpx <- readLines(con = "./data_raw/pozice_stromu_s_dm.gpx")
gpx

nfo_dendro <- data.table()

nfo_dendro[, `:=`(coord = gpx[grep(pattern = "lat",
                                   x = gpx)],
                  elevation = gpx[grep(pattern = "ele",
                                       x = gpx)],
                  name = gpx[grep(pattern = "name",
                                  x = gpx)])]

nfo_dendro[, elevation := as.numeric(x = gsub(pattern = "<ele>|</ele>", 
                                              replacement = "",
                                              x = elevation))]
str(object = nfo_dendro)

nfo_dendro[, name := as.factor(x = gsub(pattern = "<name>|</name>", 
                                        replacement = "",
                                        x = name))]
str(nfo_dendro)

nfo_dendro[, coord := gsub(pattern = "<wpt|</wpt>| lat=|lon=|\"|>|    ", 
                                        replacement = "",
                                        x = coord)]
str(nfo_dendro)

lon_lat <- as.matrix(x = do.call(what = rbind,
                                  args = strsplit(x = nfo_dendro$coord, 
                                                  split = " ")))
nfo_dendro <- cbind(nfo_dendro, lon_lat)
setnames(x = nfo_dendro,
         old = c("V1", "V2"),
         new = c("Y", "X"))
nfo_dendro <- nfo_dendro[grep(pattern = "BP|KL",
                              x = name, 
                              invert = TRUE)]

nfo_dendro[, `:=`(X = as.numeric(x = X),
                  Y = as.numeric(x = Y))]

nfo_dendro[, coord := NULL]
nfo_dendro[, ID := as.numeric(x = substr(x = name, 
                                         start = 1,
                                         stop = 17))]
nfo_dendro[, name := substr(x = name, 
                            start = 17,
                            stop = nchar(x = as.character(x = name)))]
nfo_dendro

fwrite(x = nfo_dendro,
       file = "./data/dendrometer_info.csv")

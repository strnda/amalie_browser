library(sf); library(readxl); library(data.table)

hru <- st_read(dsn = "./data/hru_info.shp")

head(x = hru)
library(leaflet)

leaflet() %>% 
  addTiles() %>%
  addWMSTiles(baseUrl = "http://geoportal.cuzk.cz/WMS_ORTOFOTO_PUB/WMService.aspx",
              options = WMSTileOptions(format = "image/png",
                                       crs = "EPSG:102066"),
              layers = "GR_ORTFOTORGB") %>%
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
tmst <- fread(file = "~/ownCloud/Active Docs/amalie/amalie_browser/data_raw/sensors_descrptionL.csv",
              sep = ";")
geometry <- st_multipoint(x = as.matrix(x = tmst[, .(x, y)]))
tmst <- as.data.frame(x = tmst)
tmst$geometry <- list(geometry)

tmst_sf <- st_sf(tmst,
                 crs = "EPSG:102066")
                   
                #   "+proj=krovak +lat_0=49.5 +lon_0=42.5 +alpha=30.28813975277778 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +pm=ferro +units=m +no_defs")
plot(tmst_sf)
sp::proj4string(sp::CRS("+proj=krovak +lat_0=49.5 +lon_0=42.5 +alpha=30.28813975277778 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +pm=ferro +units=m +no_defs" ))
st_set_geometry()

tmst_sf <- st_transform(x = tmst_sf,
                        crs = "4236")
plot(x = tmst_1$x,
     y = tmst_1$y,
     pch = 19,
     cex = 2)
points(x = tmst_2$x,
       y = tmst_2$y,
       col = "red",
       pch = 19,
       cex = 1.5)
points(x = tmst_3$x,
       y = tmst_3$y,
       pch = 19,
       col = "green",
       cex = 1)

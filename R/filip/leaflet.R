library(sf)

pol_1 <- st_read(dsn = "./data_raw/ShapeFiles/BP_D_FG_WGS.shp")
pol_1$ID <- "BP"
plot(pol_1)

pol_2 <- st_read(dsn = "./data_raw/ShapeFiles/KL_D_FG_WGS.shp")
pol_2$ID <- "KL"
plot(pol_2)

hru <- rbind(pol_1, 
             pol_2)

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

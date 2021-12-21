library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(ggplot2)
library(fst)
library(DT)

ui <- fluidPage(
  
  title = 'shiny',
  
  fluidPage(
    column(width = 5,
           leafletOutput(outputId = 'map',
                         height = 500)),
    column(width = 7,
           textOutput(outputId = "click"),
           plotOutput(outputId = "plot",
                      height = 600))
  )
)

server <- function(input, output, session) {
  
  hru <- st_read(dsn = "./data/hru_info.shp", 
                 quiet = TRUE)
  hru$layer_ID <- paste(hru$ID, 
                        as.character(x = hru$OBJECTID),
                        sep = "_")

  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles(group = "Podkladová mapa") %>%
      addWMSTiles(baseUrl = "http://geoportal.cuzk.cz/WMS_ORTOFOTO_PUB/WMService.aspx",
                  options = WMSTileOptions(format = "image/png",
                                           crs = "EPSG:102066"),
                  layers = "GR_ORTFOTORGB",
                  group = "Ortofoto") %>%
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
                  group = "Polygony hydr. jednotek",
                  layerId = ~layer_ID) %>%
      addLayersControl(baseGroups = c("Podkladová mapa", 
                                      "Ortofoto"),
                       overlayGroups = c("Polygony hydr. jednotek"),
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  observe({
    
    click <- input$map_shape_click
    
    if(is.null(x = click)){
      
      return()
    } else {
      
      output$click <- renderPrint({

        print(list(X = click$id))

      })
      
      output$plot <- renderPlot({
        
        df <- data.frame(
          gp = factor(rep(letters[1:3], each = 10)),
          y = rnorm(30)
        )
        ds <- do.call(rbind, lapply(split(df, df$gp), function(d) {
          data.frame(mean = mean(d$y), sd = sd(d$y), gp = d$gp)
        }))

        ggplot(df, aes(gp, y)) +
          geom_point() +
          geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)
      })
      
    }
  })
}

shinyApp(ui = ui, 
         server = server)

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
    column(width = 4,
           leafletOutput(outputId = 'map',
                         height = 500)),
    column(width = 8,
           align = "center",
           plotOutput(outputId = "plot",
                      height = 550),
           sliderInput(inputId = "temp",
                       label = "Nárůst teploty [°C]:",
                       min = .5,
                       max = 5,
                       step = .5,
                       value = .5, 
                       width = "500px"))
  )
)

server <- function(input, output, session) {
  
  hru <- st_read(dsn = "./data/hru_info.shp", 
                 quiet = TRUE)
  hru$layer_ID <- paste(hru$ID, 
                        as.character(x = hru$OBJECTID),
                        sep = "_")
  
  dta_kz <- read_fst(path = "./data/kz.fst",
                     as.data.table = TRUE)
  
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
      
      # output$click <- renderPrint({
      #   
      #   print(list(X = click$id))
      #   
      # })
      
      
      
      output$plot <- renderPlot({
        
        dta_plot <- dta_kz[ID == click$id & dT == input$temp, ]
        
        ggplot(data = dta_plot) +
          geom_line(mapping = aes(x = block,
                                  y = Q50,
                                  colour = cl),
                    show.legend = FALSE) +
          geom_ribbon(mapping = aes(x = block,
                                    ymin = Q25, 
                                    ymax = Q75, 
                                    fill = cl),
                      alpha = .5,
                      show.legend = FALSE) +
          facet_wrap(facets = ~variable, 
                     scales = "free", 
                     ncol = 3) +
          theme_bw() +
          labs(x = "Týden",
               y = "Relativní změna",
               title = paste0("Relativní změny hodnot veličin při nárůstu teploty o ",
                              input$temp, " [°C]")) +
          theme(plot.title = element_text(hjust = 0.5))
      })
    }
  })
}

shinyApp(ui = ui, 
         server = server)

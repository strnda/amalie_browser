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
                         height = 500),
           dataTableOutput(outputId = "summary_table")),
    column(width = 7,
           # textOutput(outputId = "click"),
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
  
  sucho_dta <- read_fst(path = "./data/indexy_sucha.fst", 
                        as.data.table = TRUE)
  
  levels(x = sucho_dta$variable) <- c("SPI - Meteorologické sucho", 
                                      "SRI - Hydrologické sucho", 
                                      "SGI - Nedostatek podzemních vod",
                                      "SSI - Půdní sucho")

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
      
      dta_plot <- sucho_dta[ID == click$id,]
      
      output$plot <- renderPlot({
        
        ggplot(data = dta_plot) +
          geom_col(mapping = aes(x = date,
                                 y = index,
                                 colour = ifelse(test = index > 0, 
                                                 yes = 'mokro',
                                                 no = 'sucho')),
                   show.legend = FALSE) +
          scale_colour_manual(values = c("steelblue4", "red3")) +
          geom_hline(yintercept = 0, 
                     colour = "black", 
                     linetype = 2) + 
          geom_hline(yintercept = c(-2, 2), 
                     colour = "grey25", 
                     linetype = 3) + 
          facet_wrap(facets = ~variable, 
                     scales = "free", 
                     ncol = 1) +
          theme_bw() +
          labs(x = "Čas", 
               y = "Hodnota indexu")
      })
      stat <- dta_plot[, .(`Průměr` = mean(x = index, 
                                           na.rm = TRUE),
                           `Minimum` = min(x = index, 
                                           na.rm = TRUE),
                           `Maximum` = max(x = index, 
                                           na.rm = TRUE),
                           `1. kvartil` = quantile(x = index, 
                                                   probs = .25, 
                                                   na.rm = TRUE),
                           `Medián` = quantile(x = index, 
                                               probs = .5, 
                                               na.rm = TRUE),
                           `3. kvartil` = quantile(x = index, 
                                                   probs = .75, 
                                                   na.rm = TRUE),
                           `Mezikvar. rozpětí` = IQR(x = index,
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

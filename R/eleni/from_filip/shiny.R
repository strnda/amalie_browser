library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(data.table)
library(DT)
library(dygraphs)
library(ggplot2)

ui <- fluidPage(
  
  theme = shinytheme('flatly'),
  
  title = 'Prague IDF browser',
  
  titlePanel(title = 'Prague IDF browser'),
  
  leafletOutput(outputId = 'map'),
  
  hr(),
  
  fluidRow(
    
    tabsetPanel(type = 'tabs',
                
                tabPanel(title = 'IDF model',
                  column(width = 8, 
                         title = 'IDF model',
                         offset = 1,
                         plotOutput('click_plot_idf')
                  ),
                  
                  column(width = 3,
                         title = 'IDF controls',
                         sliderInput(inputId = 'inputDuration', 
                                     label = 'Duration',
                                     min = 1,
                                     max = 120,
                                     value = c(5, 60),
                                     step = 1,
                                     round = 0),
                         br(),
                         checkboxInput(inputId = 'inputEmp', 
                                       label = 'Empirical values',
                                       value = FALSE),
                         checkboxInput(inputId = 'inputIdf', 
                                       label = 'IDF model', 
                                       value = TRUE)
                  )
                ),
                
                tabPanel(title = 'Timeseries plot', 
                         dygraphOutput('timeSeries')),
                
                tabPanel(title = 'Summary table',
                         dataTableOutput('table'))
    )
    

  ),
  
  hr(),
  
  fluidRow(
    
    
  )
)

server <- function(input, output, session) {
  
  ### overview
  dta <- readRDS(file = './data/prague_prec.rds')
  
  dta[, Value := round(x = Value,
                       digits = 2)]
  dta <- dta[Value >= 0, ]
  
  ### IDF
  source(file = './R/aux_fun/idf.R')
  
  idf.emp.all <- readRDS(file = './data/idf_emp.rds')
  
  idf.par.all <- readRDS(file = './data/idf_par.rds')
  
  dist.par.all <- readRDS(file = './data/dist_par.rds')
  
  ### leaflet
  pha <- readOGR(dsn = 'data/TMMESTSKECASTI_P_shp/TMMESTSKECASTI_P.shp', 
                 layer = 'TMMESTSKECASTI_P',
                 verbose = FALSE)
  
  stanice <- readRDS(file = 'data/meta_wgs84.rds')
  
  ### table
  tab <- readRDS(file = './data/data_tab.rds')
  
  icons <- makeAwesomeIcon(
    icon = 'tint', 
    markerColor = 'darkblue',
    iconColor = 'white'
  )
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addPolygons(
        data = pha,
        color = 'black', 
        weight = 1, 
        smoothFactor = 0.5,
        opacity = 1.0, 
        fillOpacity = 0.25,
        # fillColor = 'royalblue',
        highlightOptions = highlightOptions(color = 'white', 
                                            weight = 2,
                                            bringToFront = TRUE
        ),
        label = ~NAZEV_MC
      ) %>%
      addAwesomeMarkers(
        icon = icons,
        data = stanice,
        lng = ~lng,
        lat = ~lat,
        layerId = ~Znacka,
        label = paste(stanice$Znacka, '-', stanice$Lokalita),
        popup = paste('<b>', stanice$Znacka, '-', stanice$Lokalita, '</b>',
                      '<br/>',
                      'Měření:',
                      '<br/>', 
                      'od -', stanice$od,
                      '<br/>', 
                      'do -', stanice$do),
        group = 'Old'
      ) %>%
      addLayersControl(
        overlayGroups = c('Old', 'New'),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addTiles()
  })
  
  observe({
    click <- input$map_marker_click
    
    if(is.null(x = click)){
      
      return()
    } else {
    
      stanice.aux <- stanice[Znacka == click$id,]

      output$click_plot_idf <- renderPlot({
        
        click <- click$id

        idf.emp <- idf.emp.all[id == click,]
        idf.par <- idf.par.all[id == click,]

        idf.line.aux <- lapply(X = seq_along(along.with = idf.par[, rp]),
                               FUN = function(i) {
                                 data.table(
                                   dur = seq(from = min(x = input$inputDuration[1]),
                                             to = max(x = input$inputDuration[2]),
                                             length.out = 1000),
                                   inten = idf.emp.fun(t = seq(from = min(x = input$inputDuration[1]),
                                                               to = max(x = input$inputDuration[2]),
                                                               length.out = 1000),
                                                       a = idf.par[i, a],
                                                       n = idf.par[i, n])
                                 )
                               }
        )

        names(x = idf.line.aux) <- idf.par[, rp]

        idf.line <- rbindlist(l = idf.line.aux,
                              idcol = 'rp')
        idf.line[, rp := as.numeric(x = rp)]
        
        pal <- c('#999999', '#E69F00', '#56B4E9', '#009E73',
                 '#CCCC00', '#0072B2', '#D55E00', '#CC79A7')

        p <- ggplot()
          
        if (input$inputEmp) {
          
          p <- p + geom_point(data = idf.emp,
                              mapping = aes(x = as.numeric(gsub(pattern = 'min',
                                                                replacement = '',
                                                                x = dur
                              )),
                              y = inten,
                              colour = factor(rp)))
            
        }
        
        if (input$inputIdf) {
          
          p <- p + geom_line(data = idf.line,
                             mapping = aes(x = dur,
                                           y = inten,
                                           colour = factor(rp)))
            
        }
        
        p <- p + scale_color_manual(values = pal,
                                    name = 'Return period [years]') +
          labs(x = 'Duration [minutes]',
               y = 'Intensity [mm/hour]',
               title = paste(stanice.aux[, Znacka], '-', stanice.aux[, Lokalita])) +
          theme_bw() +
          theme(legend.position = 'bottom', 
                plot.title = element_text(face = 'bold')) +
          guides(colour = guide_legend(nrow = 1))
          
        return(p)
      })
      
      output$timeSeries <- renderDygraph({
        
        dygraph(data = dta[id == 'D01', .(DTM, Value / 1000 * 60)],
                main = paste(stanice.aux[, Znacka], '-', stanice.aux[, Lokalita])) %>%
          dyRangeSelector() %>%
          dyOptions(colors = 'steelblue')
      })
      
      output$table <- renderDataTable({
        
        datatable(data = tab[[as.numeric(gsub(pattern = 'D', 
                                              replacement = '',
                                              x = click$id))]][, c(-1:-8, -12:-14)], 
                  rownames = month.name,
                  options = list(paging = FALSE,
                                 searching = FALSE,
                                 info = FALSE,
                                 scrollX = 12),
                  class = 'cell-border stripe', 
                  autoHideNavigation = TRUE, 
                  caption = paste(stanice.aux[, Znacka], '-', stanice.aux[, Lokalita]))
        
      })
    }
  })
}

shinyApp(ui = ui, 
         server = server)

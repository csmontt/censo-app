library(shiny)
library(rgdal)
library(leaflet)
library(dplyr)
library(maptools)
library(rgeos)
library(DT)
library(xlsx)


filenames <- list.files("./data/", pattern="\\.rds$")

options(digits = 1)
options(shiny.sanitize.errors = TRUE)
options(scipen=999)

choices_num_vars <- c("TOTAL_PERS", "TOTAL_VIV","HOMBRES", "MUJERES", "EDAD_0A5", 
                      "EDAD_6A14", "EDAD_15A64", "EDAD_65YMAS","INMIGRANTES",
                      "PUEBLO","VIV_PART", "VIV_COL","MATACEP", "MATREC",
                      "MATIRREC","VPOMP", "P03A_1","P03A_2",
                      "P03A_3","P03A_4","P03A_5","P03A_6","P03B_1","P03B_2",
                      "P03B_3", "P03B_4","P03B_5","P03B_6","P03B_7","P03C_1",
                      "P03C_2","P03C_3", "P03C_4","P03C_5","P05_1","P05_2",
                      "P05_3","P05_4")
                      
choices_num_vars <- paste0(choices_num_vars, "_total")

# ui object -----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(p("Explorador CENSO 2017", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset",
                label = "Seleccionar comuna",
                choices = filenames,
                selected = "ARAUCO.rds"
                ), 
      selectInput(inputId = "urbano_rural",
                  label = "Seleccionar Urbano y/o Rural",
                  choices = c("Urbano", "Rural"),
                  multiple = TRUE,
                  selected = "Urbano"), 
      selectInput(inputId = "polygonSelected", 
                  label = "Seleccionar nivel",
                  choices = c("mz_ent", "dc_zc_loc", "dc"),
                  selected = "mz_ent"),
      selectInput(inputId = "variableselected", 
                  label = "Seleccionar variable",
                  choices = c(choices_num_vars)),
      # Button
      downloadButton("downloadData", "Descargar tabla")
    ),
    
    mainPanel(
      leafletOutput(outputId = "map", width = "100%", height = 610),
      DTOutput(outputId = "table"),
      width = 8
    )
  )
)

# server() ------------------------------------------------------------
server <- function(input, output){
  
  spdf <- reactive({
                datos <- readRDS(paste0("./data/", input$dataset))
                return(datos)
  })
  
  tabla <-  reactive({
        map <- spdf()
        num <- map@data %>% 
                group_by_at(input$polygonSelected) %>%
                summarise_at(.vars = vars(TOTAL_PERS:P05_4),
                             .funs = c(total="sum"), 
                             na.rm = TRUE)
        
        not_num <- map@data %>%
                group_by(get(input$polygonSelected)) %>%
                filter(row_number() == 1) %>%
                select(REGION:NOM_CATEGO)

        df <- merge(not_num, num, by = input$polygonSelected)
        
        return(df)
       
  })
  
  output$table <- renderDT({
          datatable(tabla()[,-2], rownames = FALSE, selection = "single", 
                    options = list(stateSave = TRUE,scrollX = T))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      name <- paste(input$dataset, ".xlsx", sep = "")
      name <- gsub("\\.rds", "", name)
      name
    },
    content = function(file) {
      write.xlsx(tabla(), file, row.names = FALSE)
    }
  )
  
  

        
  output$map <- renderLeaflet({
    map <- spdf()
    
    # Disolución polígonos
    mz_pol <- unionSpatialPolygons(map[map$AREA %in% input$urbano_rural, ],
                                   map@data[map$AREA %in% input$urbano_rural, input$polygonSelected])

    # Transformación a SpatialPolygonsDataFrame
    mz_pol <- SpatialPolygonsDataFrame(spTransform(mz_pol,
                                                CRS("+proj=longlat +ellps=sphere +no_defs")),
                                    data.frame(nivel = names(mz_pol),
                                               row.names = names(mz_pol),
                                               stringsAsFactors=FALSE))

    # Crear datos agregados a nivel seleccionado
    mz_pol_data <- tabla()
    names(mz_pol_data)[1] <- "nivel"

    # Hacemos un merge entre el SpatialPolygonsDataFrame y los datos agregados
    # recien creados.
    mz_pol@data <- inner_join(mz_pol@data, mz_pol_data, by = "nivel")

    # Create variableplot
    mz_pol$variableplot <- as.numeric(mz_pol@data[, input$variableselected]) 


    # Create leaflet
    pal <- colorBin("YlOrRd", domain = mz_pol$variableplot, bins = 7) 

    labels <- sprintf("%s: %g", mz_pol$nivel, mz_pol$variableplot) %>% lapply(htmltools::HTML) 

    leaflet(mz_pol) %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite)  %>%
            addPolygons(
                    group = "microdatos",
                fillColor = ~pal(variableplot), 
                color = "white",
                dashArray = "3",
                fillOpacity = 0.5,
                label = labels) %>%
             addLegend(pal = pal, values = ~variableplot, opacity = 0.7,
                         title = "Frecuencia", position = "bottomleft") 
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)



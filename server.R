pacman::p_load(tidyverse, readxl, leaflet, leaflet.extras, shiny, sf,
               osmdata, magrittr, terra, units, geodata, elevatr, stars)

shinyServer(function(input, output) {
  
  ## Cargando shape de cosecha
  
  data <- reactive({
    req(input$shape)
    
    ext <- tools::file_ext(input$shape$name)
    data <- switch(ext,
      geojson = st_read(input$shape$datapath, quiet = T),
      zip = {
        unzip(input$shape$datapath, exdir = 'files')
        st_read(dir('files/', pattern = '*.shp', full.names = T), quiet = T)
      }
    )
    
    data %>%
      st_transform(4326)%>%
      mutate(id = row_number())
  })
  
  ## Mostrando shape en leaflet
  
  output$mapa <- renderLeaflet(
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = data(), weight = 1, layerId = ~id,
                  popup = ~ paste('Id lote:', id))%>%
      addDrawToolbar(editOptions = editToolbarOptions(edit = T))
  )
  
  
  ## Cargando documento con equipos
  
  equipos <- reactive({
    req(input$file)
    
    read_excel(input$file$datapath) %>%
      select(1, 3, 4) %>%
      rename(Equipo = 1, Pendiente = 2)
  })
  
  
  ## Mostrando tabla de equipos
  
  output$equipos <- renderDataTable(expr = equipos(),
                                    options = list(searching = F))
  
  
  ## Seleccionando lote
  
  output$lote <- renderUI({
    req(input$shape)
    
    selectInput(inputId = 'var', label = 'Seleccione el lote a cosechar:',
                choices = data()$id)
  })
  
  ## Calculando las pendientes
  
  output$res <- renderPrint({
    req(input$var)
    req(input$file)
    
    data() %>%
      filter(id == as.integer(input$var)) %>% 
      crop(x = rast(input$dem$datapath), y = .) %>%
      terrain(v = 'slope', unit = 'degrees') * 100/360 -> slope
    
    if(input$manual){
      
      min <- round(min(values(slope), na.rm = T))
      max <- round(max(values(slope), na.rm = T))
      
      slope %>%
        st_as_stars() %>%
        st_contour(breaks = seq(min, max, length.out = input$breaks))
      
    } else {
      slope %>%
        st_as_stars() %>%
        st_contour(breaks = c(0, sort(unique(equipos()$Pendiente))))
    }
  })
  
  # Configurando panel
  
  isPanelOpen <- reactiveVal(FALSE)
  
  observeEvent(input$mapa_shape_click, { 
    print(input$mapa_shape_click)
    #isPanelOpen(TRUE)
  })
  
  observeEvent(input$hidePanel, isPanelOpen(FALSE))
  
  output$informacion <- renderReact({
    Panel(
      headerText = "Información del lote", 
      isOpen = isPanelOpen(),
      "", type = 1,
      onDismiss = JS("() => { Shiny.setInputValue('hidePanel', Math.random()); }")
    )
  })
})

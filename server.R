pacman::p_load(tidyverse, readxl, leaflet, leaflet.extras, shiny, sf, osmdata, stars, magrittr, whitebox, units)

shinyServer(function(input, output) {
  
  ## Cargando shape de cosecha
  data <- reactive({
    req(input$shape)
    
    ext <- tools::file_ext(input$shape$name)
    data <- switch(ext,
      geojson = st_read(input$shape$datapath),
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
      select(1,3) %>%
      rename(Equipo = 1, Pendiente = 2)
  })
  
  
  ## Mostrando tabla de equipos
  
  output$equipos <- renderDataTable(expr = equipos(),
                                    options = list(pageLength = 3, searching = F))
  
  
  ## Seleccionando lote
  
  id <- reactive({
    req(input$shape)
    data()$id
  })
  
  output$lote <- renderUI({
    selectInput(inputId = 'var', label = 'Seleccione el lote a cosechar:',
                choices = id())
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

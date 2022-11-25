pacman::p_load(tidyverse, readxl, leaflet, leaflet.extras, shiny, sf, osmdata,
               osmdata, terra, units, elevatr, stars, tidyterra, plotly, magrittr)

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
                                    options = list(searching = F, pageLength = 3))
  
  
  ## Seleccionando lote
  
  output$lote <- renderUI({
    req(input$shape)
    
    selectInput(inputId = 'var', label = 'Seleccione el lote a cosechar:',
                choices = data()$id)
  })
  
  ## Información del dem
  
  output$elevacion <- renderPrint({
    req(input$dem)
    
    rast(input$dem$datapath) %>%
      rename(Elevacion = 1) %>%
      summary(warn = F)
  })
  
  ## Calculando las pendientes
  
  seleccion <- reactive({
    req(input$var)
    
    data() %>%
      filter(id == as.integer(input$var))
  })
  
  slope <- reactive({
    req(input$var)
    
    rast(input$dem$datapath) %>%
      crop(y = seleccion(), snap = 'out') %>%
      terrain(v = 'slope', unit = 'radians') %>%
      tan() *100
  })
  
  contorno <- reactive({
    req(input$file, input$var)
      
      min <- floor(min(values(slope()), na.rm = T))
      max <- ceiling(max(values(slope()), na.rm = T))
    
    if(input$manual){
      
      contorno <- slope() %>%
        st_as_stars() %>%
        st_contour(breaks = seq(min, max, length.out = input$breaks))
      
    } else {
      
      breaks <- sort(unique(equipos()$Pendiente))
      breaks[length(breaks)] <- max
      
      contorno <- slope() %>%
        st_as_stars() %>%
        st_contour(breaks = c(0, breaks))
        
    }
    
    contorno %>%
      st_intersection(seleccion()) %>%
      st_simplify() %>%
      mutate(id = row_number())
  })
  
  output$res <- renderTable({
    req(input$var)
    
    contorno() %>%
      select(1) %>%
      mutate('Area (ha)'= st_area(.) %>% set_units('ha') %>% round(3)) %>%
      st_drop_geometry() %>%
      rename('Pendiente (%)' = 1)
  })
  
  output$plot <- renderPlotly({
    req(input$dem, input$var)
    
    plot <- ggplot(contorno(), aes(text = str_glue("Grupo: {id}")))+
              geom_sf(data = seleccion(), fill = "white")+
              geom_sf(aes(fill = slope))+
              scale_fill_viridis_d()+
              labs(fill = 'Pendiente (%)')+
              theme_light()+
              theme(axis.text = element_blank(), 
                    axis.ticks = element_blank(),
                    panel.grid = element_blank())
    
    ggplotly(plot)
  })
  
  output$patios <- renderPrint({
    req(input$pendiente, input$var)
    
    tryCatch(expr = {
      
      vias <- data() %>%
        st_buffer(dist = input$vias * 1000) %>%
        {opq(bbox = st_bbox(.), timeout = 5)} %>%
        add_osm_feature(key = "highway") %>%
        osmdata_sf() %>%
        pluck('osm_lines') %>%
        pull(geometry) %>%
        st_sfc(crs = 4326)  
    
      slope() %>%
        st_as_stars() %>%
        st_contour(breaks = c(0, input$pendiente)) %>%
        st_intersection(y = seleccion()) %>%
        filter(Max == input$pendiente) %>%
        st_cast(to = 'POLYGON') %>%
        mutate(Area = st_area(.) %>% set_units('ha') %>% round(3)) %>%
        st_centroid() %>%
        mutate(Latitud = st_coordinates(.)[,2],
               Longitud  = st_coordinates(.)[,1],
               Vias = st_distance(x = ., y = vias) %>%
                 set_units('km') %>% round(2) %>%
                 apply(MARGIN = 1, FUN = min)) %>%
        select(Latitud, Longitud, Area, Vias) %>%
        st_drop_geometry() %>%
        remove_rownames() 
    }, error = \(x) cat('No se encuentran vías cercanas. \nPor favor incremente el radio de búsqueda'))
  })
  
  output$optimos <- renderDataTable({
    
    clasificacion <- function(valor, intervalo) {
      valores <- as.character(intervalo) %>%
        str_split(pattern = ',', simplify = T) 
      min <- substring(text = valores[1], first = 2,
                       last = nchar(valores[1])) %>% as.numeric()
      max <- substring(text = valores[2], first = 1,
                       last = nchar(valores[2]) - 1) %>% as.numeric()
      between(x = valor, left = min, right = max)
    }
    
    equipos() %>%
      mutate(Grupo = map_int(.x = Pendiente,
                             .f = \(x) detect_index(.x = contorno() %>% pull(slope),
                                                    .f = \(y) clasificacion(valor = x, intervalo = y)))) %>%
      select(Equipo, Grupo)
  })
  
  isDialogOpen <- reactiveVal(F)
  observeEvent(input$creditos, isDialogOpen(T))
  observeEvent(input$hideDialog, isDialogOpen(F))
  observeEvent(input$dialogSend, isDialogOpen(F))
  
  output$modal <- renderReact({
    
    cuerpo <- div(
                 p("Juan Sebastián Mendoza Páez", style = "margin-bottom: 0px"),
                 p("Estudiante Ingenieria Forestal", style = "margin-bottom: 0px"),
                 p("PAE - Planificación Forestal", style = "margin-bottom: 0px"),
                 p("Universidad Nacional de Colombia", style = "margin-bottom: 0px"),
                 p(" Sede Medellín", stle = "margin-bottom: 0px"),
                 style = "display: flex; align-items: center; flex-direction: column; font-size: 15px")
    
    dialogContentProps <- list(
      type = 1,
      title = div('Desarrollador', style = 'display: flex; justify-content: center; color: #20124D'),
      closeButtonAriaLabel = 'Close',
      subText = cuerpo
    )
    
    Dialog(
      hidden = !isDialogOpen(),
      onDismiss = JS("() => { Shiny.setInputValue('hideDialog', Math.random()); }"),
      dialogContentProps = dialogContentProps,
      modalProps = list(),
      DialogFooter(
        PrimaryButton.shinyInput('dialogSend', text = 'Ok')
      )
    )
    
  })
  
})

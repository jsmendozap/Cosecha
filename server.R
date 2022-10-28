pacman::p_load(tidyverse, readxl, leaflet, leaflet.extras, shiny, sf,
               osmdata, terra, units, elevatr, stars, tidyterra, plotly)

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
  
  contorno <- reactive({
    req(input$var)
    req(input$file)
    
    data() %>%
      filter(id == as.integer(input$var)) -> seleccion
    
    crop(x = rast(input$dem$datapath), y = seleccion) %>%
      mask(mask = seleccion, touches = T) %>%
      terrain(v = 'slope', unit = 'radians') %>%
      tan() *100 -> slope
    
    if(input$manual){
      
      min <- floor(min(values(slope), na.rm = T))
      max <- ceiling(max(values(slope), na.rm = T))
      
      contorno <- slope %>%
        st_as_stars() %>%
        st_contour(breaks = seq(min, max, length.out = input$breaks))
      
    } else {
      contorno <- slope %>%
        st_as_stars() %>%
        st_contour(breaks = c(0, sort(unique(equipos()$Pendiente)))) 
    }
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
    req(input$dem)
    
    plot <- ggplot(contorno())+
              geom_sf(data = data() %>% filter(id == input$var), fill = "white")+
              geom_sf(aes(fill = slope))+
              scale_fill_viridis_d()+
              labs(fill = 'Pendiente (%)')+
              theme_light()+
              theme(axis.text = element_blank(), 
                    axis.ticks = element_blank(),
                    panel.grid = element_blank())
    
    ggplotly(plot)
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

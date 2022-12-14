pacman::p_load(shiny, shiny.fluent, leaflet, shinyWidgets, bslib, plotly, shiny.mui)

bs4_card <- function(body, title, style = "") {
  div(class="table-responsive",
      class = "card",
      div(class = "card-header bg-primary d-flex justify-content-center", title),
      div(class = "card-body d-flex justify-content-center", body,
          style = style)
  )
}

card <- function(body, style = "") {
  div(class="table-responsive",
      class = "card",
      div(class = "card-body d-flex justify-content-center", body,
          style = style)
  )
}

acordeon <- function(resumen, detalles){
  Accordion(
    AccordionSummary(expandIcon = icon(name = "fa-light fa-tree",
                                       lib = "font-awesome",
                                       style = 'color: #6F7378',
                                       verify_fa = F),
                     Typography(resumen, style = 'font-weight: bold')),
    AccordionDetails(
      Typography(detalles)
    ))
}

info <- nav(title = 'Información', 
            sidebarLayout(
              sidebarPanel(width = 3,
                           fileInput(inputId = 'shape', label = 'Shape de plantación:'),
                           fileInput(inputId = 'file', label = 'Equipos disponibles:'),
                           selectInput(inputId = 'deminfo', label = 'Forma de ingreso del DEM',
                                       choices = c(Descarga = 'descarga', Archivo = 'ingresar'),
                                       selected = 'ingresar'),
                           conditionalPanel(
                             condition = "input.deminfo == 'descarga'",
                             textInput(inputId = 'key', label = 'Api-Key de OpenTopography: '),
                             actionButton(inputId = 'descarga', label = 'Descargar DEM')
                           ),
                           conditionalPanel(
                             condition = "input.deminfo == 'ingresar'",
                             fileInput(inputId = 'dem', label = 'Modelo de elevación Digital:')
                           ),
                           reactOutput('informacion')
              ), # Cierre sidebar
              mainPanel(width = 9,
                        fluidRow(
                          column(6, bs4_card(leafletOutput(outputId = 'mapa', height = '77vh'),
                                             title = 'Ubicación de la plantación', style = "padding: 1px")),
                          column(5, bs4_card(dataTableOutput('equipos'), 'Resumen equipos'),
                                    br(),
                                    column(6, bs4_card(verbatimTextOutput('elevacion'),
                                                       title = 'Elevación del terreno',
                                                       style = 'padding: 3px; margin-top: 3px')))
                        )
              ) # Cierre panel principal
            ) # Cierre layout
)

lote <- nav(title = 'Lote',
            sidebarLayout(
              sidebarPanel(width = 3,
                           uiOutput('lote'),
                           materialSwitch(inputId = 'manual', label = 'Intervalo manual', status = 'info'),
                           conditionalPanel(condition = "input.manual == true",
                                            sliderInput(inputId = 'breaks', label = 'Cantidad de contornos:',
                                                        min = 2, max = 10, value = 3, step = 1)),
                           numericInput(inputId = 'pendiente', label = 'Máxima pendiente', value = 15,
                                        min = 1, max = 30),
                           numericInput(inputId = 'vias', label = 'Radio de búsqueda de vías (Km)', value = 1,
                                        min = 0, max = 10)
              ), # Cierre sidebar
              mainPanel(width = 9, fluidRow(column(width = 6, style = 'display: flex; align-items: center; flex-direction: column',
                                        h3('Información por lote', style = 'font-weight: bold'),
                                        plotlyOutput(outputId = 'plot', height = '75vh')),
                                 column(width = 6,
                                        br(), br(), br(),
                                        acordeon(resumen = "Área plantada por pendiente",
                                                 detalles = card(tableOutput(outputId = 'res'))),
                                        br(),
                                        acordeon(resumen = 'Patios de cosecha',
                                                 detalles = card(verbatimTextOutput(outputId = 'patios'))),
                                        br(),
                                        acordeon(resumen = 'Equipos óptimos',
                                                 detalles = card(dataTableOutput(outputId = 'optimos')))
                                        )
                                 )
                        )
            ) # Cierre Layout
)

creditos <- nav_item(div(actionButton(inputId = 'creditos', label = 'Créditos', 
                                     style = "background-color: #2c3e50; border-color: #2c3e50"),
                         tags$a(icon("github", "fa-2x"), href='https://github.com/jsmendozap/Cosecha', target = "_blank")),
                     style = 'display: flex', 
                     reactOutput('modal'))

shinyUI(
  page_navbar(
    title = div(icon(name = "fa-light fa-tree", lib = "font-awesome",
                     verify_fa = F), "Cosecha Forestal",
                style = "font-size: 25px; font-weight: bolder"),
    bg = '#2c3e50',
    !!!list(info, lote, nav_spacer(), creditos),
    header = tags$style(".navbar-header {display: flex}"),
    theme = bs_theme(bootswatch = "flatly")
))

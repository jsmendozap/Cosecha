library(shiny)
library(shiny.fluent)
library(leaflet)
library(shinyWidgets)
library(bslib)
library(plotly)

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
                          column(6, leafletOutput(outputId = 'mapa', height = 500)),
                          column(6, dataTableOutput('equipos'),
                                    br(),
                                    column(6, verbatimTextOutput('elevacion')))
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
                                                        min = 2, max = 10, value = 3, step = 1))
              ), # Cierre sidebar
              mainPanel(fluidRow(column(width = 8,
                                        h3('Identificación de pendientes por lote'),
                                        plotlyOutput(outputId = 'plot')),
                                 column(width = 4,
                                        br(), br(), br(),
                                        tableOutput(outputId = 'res'))
                                 )
                        )
            ) # Cierre Layout
)

creditos <- nav_item(div(actionButton(inputId = 'creditos', label = 'Créditos', 
                                     style = "background-color: #20124D; border-color: #20124D"),
                         tags$a(icon("github", "fa-2x"), href='https://github.com/jsmendozap/Cosecha', target = "_blank")),
                     style = 'display: flex', 
                     reactOutput('modal'))

shinyUI(
  page_navbar(
    title = div(icon(name = "fa-light fa-tree", lib = "font-awesome",
                     verify_fa = F), "Cosecha Forestal",
                style = "font-size: 25px; font-weight: bolder"),
    bg = '#20124D',
    !!!list(info, lote, nav_spacer(), creditos),
    header = tags$style(".navbar-header {display: flex}"),
    theme = bs_theme(bootswatch = "flatly")
))

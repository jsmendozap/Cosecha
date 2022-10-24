library(shiny)
library(shiny.fluent)
library(leaflet)
library(shinyWidgets)
library(bslib)

info <- tabPanel(title = 'Información', 
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

lote <- tabPanel(title = 'Lote',
            sidebarLayout(
              sidebarPanel(width = 3,
                           uiOutput('lote'),
                           materialSwitch(inputId = 'manual', label = 'Intervalo manual', status = 'info'),
                           conditionalPanel(condition = "input.manual == true",
                                            sliderInput(inputId = 'breaks', label = 'Cantidad de contornos:',
                                                        min = 2, max = 10, value = 3, step = 1))
              ), # Cierre sidebar
              mainPanel(verbatimTextOutput(outputId = 'res'))
            ) # Cierre Layout
)

shinyUI(
  navbarPage(
    title = div(icon(name = "fa-light fa-tree", lib = "font-awesome",
                     verify_fa = F), "Cosecha Forestal",
                class = "text-2xl font-semibold"), 
    info, lote,
    nav_spacer(),
    nav_item(tags$a(icon("github"), href='https://github.com/jsmendozap/Cosecha', target = "_blank")),
    header = tags$style(".navbar-header {display: flex}"),
    tags$script(src = "https://cdn.tailwindcss.com"),
    theme = bs_theme(bootswatch = "flatly")
))

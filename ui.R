library(shiny)
library(shiny.fluent)
library(leaflet)

shinyUI(fluidPage(
    
    div(tags$script(src = "https://cdn.tailwindcss.com")),
    theme = bslib::bs_theme(bootswatch = "flatly"),
    
    div(
      h1('Planificación de cosecha forestal', class = "text-4xl font-black text-center text-gray-50 p-3"),
      class = "bg-gray-700 rounded-md my-0.5"
    ),
    
    tabsetPanel(
      tabPanel(title = 'Información',
        div('', class = "p-2"),
        sidebarLayout(
            sidebarPanel(width = 3,
              fileInput(inputId = 'shape', label = 'Shape de plantación:'),
              fileInput(inputId = 'file', label = 'Equipos disponibles:'),
              div('', class = 'p-2'),
              reactOutput('informacion')
            ),
    
            mainPanel(width = 9,
                      fluidRow(
                        column(5, leafletOutput(outputId = 'mapa', height = 500),
                               class = "border-2 border-gray-400 rounded-md"),
                        column(7, dataTableOutput('equipos'))
                      )
            )
        )
      ),
      tabPanel(title = 'Cosecha por lotes',
               div('', class = "p-2"),
               sidebarLayout(
                 sidebarPanel(
                  uiOutput('lote')
                  ),
                 mainPanel()
               ),
               
               
               #actionButton('showPanel', label = 'Abrir panel')
               )
    )
))

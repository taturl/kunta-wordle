# Tällä sovelluksella voi arvuutella Suomen kuntia
# Käyttää OpenStreetMap ja Suomen kuntarajoja
# (kiitos https://github.com/geoharo/Geokml)
# Author: Tatu Lindroos

library(shiny)
library(leaflet)
library(sf)

kuntarajat <- sf::st_read('Kuntarajat.kml')

ui <- fluidPage(
  titlePanel("Mikä kunta kyseessä?"),
  selectInput("guess", NULL, c('', sort(kuntarajat$Name))),
  leafletOutput("mymap"),
  p(),
  textOutput("answer"),
  splitLayout(
    actionButton("check", "Tarkista"),
    p(style = "text-align: right; padding-right: 8px", 'Tatu Lindroos'),
    tags$a(href = 'https://github.com/taturl/kunta-wordle',
           'github',
           target = '_blank')
  )
)

server <- function(input, output, session) {
  # Setup for this session
  n <- sample(length(kuntarajat$geometry), 1)
  session$userData$kunta_name = kuntarajat$Name[n]
  kunta <- kuntarajat$geometry[n]
  session$userData$kunta <- as_Spatial(st_zm(kunta))
  session$userData$lat <- 0
  session$userData$lng <- 0
  session$userData$n_arvauksia <- 4
  
  observeEvent(input$check, {
    if (input$check != 0) {
      session$userData$n_arvauksia <- session$userData$n_arvauksia - 1
      if (session$userData$n_arvauksia >= 0 &&
          input$guess == session$userData$kunta_name) {
        output$answer <- renderText({
          ""
        })
        showModal(modalDialog(
          title = "Oikein!",
          paste0("Kunta oli ", session$userData$kunta_name, ".")
        ))
        
        lat = 65.01236
        lng = 25.46816
        zoom = 4.5
        
      } else if (session$userData$n_arvauksia > 0) {
        output$answer <-
          renderText({
            paste0("Väärin! Arvauksia jäljellä ",
                   session$userData$n_arvauksia)
          })
        lat = session$userData$lat
        lng = session$userData$lng
        zoom = max(1, 9 - (input$check / 2))
      } else {
        output$answer <- renderText({
          ""
        })
        showModal(modalDialog(
          title = "Väärin!",
          paste0("Oikea kunta oli ", session$userData$kunta_name, ".")
        ))
        lat = 65.01236
        lng = 25.46816
        zoom = 4.5
      }
      
      proxy <- leafletProxy("mymap")
      proxy %>% setView(lat = lat,
                        lng = lng,
                        zoom = zoom)
    }
    
  })
  
  output$mymap <- renderLeaflet({
    mymap <-
      leaflet(
        session$userData$kunta,
        options = leafletOptions(
          zoomSnap = 0.5,
          zoomDelta = 0.5,
          zoomControl = FALSE
        )
      ) %>%
      addPolygons() %>%
      addProviderTiles(providers$Stamen.Watercolor)
    
    mymap_center <-
      getMapData(mymap)@polygons[[1]]@Polygons[[1]]@labpt
    session$userData$lat <- mymap_center[2]
    session$userData$lng <- mymap_center[1]
    
    mymap %>% setView(
      lat = session$userData$lat,
      lng = session$userData$lng,
      zoom = 9
    )
    
  })
}

shinyApp(ui, server)

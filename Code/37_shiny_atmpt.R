

library(shiny)
library(leaflet)
library(tidyverse)
library(here)


# ui <- fluidPage(
#   sliderInput(inputId = "numero",
#               label = "Pick a damn numba",
#               value = 25, min = 1, max = 100),
#   plotOutput("hist")
# )
# 
# server <- function(input, output) {
#   output$hist <- renderPlot({
#     title = "Another cool ass histogram"
#     hist(rnorm(input$numero), main = title)
#   })
# }
# 
# shinyApp(ui = ui, server = server)

# navbarPage(title = "Shiny-Box",
#            tabPanel(title = "Home", 
#                     "content 1"),
#            
#            # fill content for tab 2
#            tabPanel(title = "Sales Overview",
#                     fluidPage(
#                       sidebarLayout(
#                         sidebarPanel(
#                           selectInput("status", label = "Status of Purchase:",
#                                       choices = c("Purchased", "Cancelled")),
#                         ),
#                         mainPanel(
#                           p("There will be plot here")
#                         )
#                       )
#                     )),
#            
#            tabPanel(title = "Interactive Map",
#                     "content 3"),
#            inverse = T
# )

################ this works!!! KEEP ##############
# ui <- fluidPage(
#           titlePanel("CCMS Datasets Available for Use"),
#                  sidebarPanel(
#                    selectInput(inputId = "datatype2",
#                                label = "What data are you searching for?",
#                                choices = c("Fishes", "Phytoplankton", "Elephant Seals")),
#                    textOutput("background")),
#                  mainPanel(leafletOutput("map1")
#                  ))
################# end ###################

ui <- navbarPage(title = "Home",
                 tabPanel(title = "Data",
                  ## for some reason, this works with or without fluid page
                  fluidPage(
                    titlePanel("CCMS Datasets Available for Use"),
                    sidebarPanel(
                      selectInput(inputId = "datatype2",
                                  label = "What data are you searching for?",
                                  choices = c("Fishes", "Phytoplankton", "Elephant Seals")),
                      textOutput("background")),
                    mainPanel(leafletOutput("map1")
                    ))),
                 tabPanel(title = "Information",
                          fluidPage(
                            titlePanel("idk some new title"),
                            selectInput(inputId = "idk",
                                        label = "just a try",
                                        choices = c("a", "b", "c")
                          ))),
                          tabPanel(title = "Contact Us",
                                   fluidPage(
                                     sidebarPanel(
                                     radioButtons(inputId = "researcher_input",
                                                  label = "Select Faculty Researcher",
                                                  choices = c("Benjamin Ruttenberg", "Dean Wendt",
                                                              "Alexis Pasulka", "Heather Liwanag"))),
                                    mainPanel(textOutput("researcher_info")))), inverse = T)

server <- function(input, output) {
  shinydat <- read_csv(here("Data", "shiny_input_bounds.csv"))
  background <- read_csv(here("Data", "shiny_background.csv"))
  researchers <- read_csv(here("Data", "shiny_researcher_background.csv"))
  
  output$map1 <- renderLeaflet({
    corner1 <- shinydat %>%
      filter(datatype == input$datatype2, corner == 1) 
    corner4 <- shinydat %>%
      filter(datatype == input$datatype2, corner == 4)
    leaflet() %>%
      setView(lng = unique(corner1$cen_lon), lat = unique(corner1$cen_lat), 
              zoom = unique(corner1$zoom))%>%
      #setView(lng = -121.338427, lat = 36.203187, zoom = 7.5)%>%
      addTiles() %>%
      addRectangles(lng1 = corner1$longitude , lat1 = corner1$latitude, 
                    lng2 = corner4$longitude  , lat2 = corner4$latitude,
                    fillColor = "transparent", label = corner4$label)%>%
      addMiniMap(position = "bottomleft", width = 200, height = 200) %>%
      addProviderTiles("Esri.WorldImagery")
  })
  output$background <- renderText({
    datatype_background <- background %>%
      filter(datatype == input$datatype2)
    print(datatype_background$blurb)
  })
  output$researcher_info <- renderText({
    researcher_filter <- researchers %>%
      filter(researcher == input$researcher_input)
    print(researcher_filter$info)
  })
}

shinyApp(ui = ui, server = server)
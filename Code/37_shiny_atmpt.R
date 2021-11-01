

library(shiny)
library(shinyWidgets)
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

##### just some tab code ###############
# tabPanel(title = "Information",
#          fluidPage(
#            titlePanel("idk some new title"),
#            selectInput(inputId = "idk",
#                        label = "just a try",
#                        choices = c("a", "b", "c")
#            )))

####### end tab code ####################


ui <- navbarPage(title = HTML("<a href=\"http://www.marine.calpoly.edu/\">CCMS</a>"), 
                 tabPanel(title = "Categories", ## used to be 'data'
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
                 tabPanel(title = "All Data",
                          fluidPage(
                            mainPanel(leafletOutput("full_map", width = "1000px", 
                                                    height = "600px")))),
                          tabPanel(title = "Researcher Background",
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
  full_map <- read_csv(here("Data", "shiny_input_bounds_full_map.csv"))
  
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
  
  output$full_map <- renderLeaflet({
    icon_phyto1 <- makeIcon(
      iconUrl = "C:/Users/erinj/Documents/GitHub/CCFRP/Icons/phyto1.png",
      #iconUrl = "https://raw.githubusercontent.com/erinmarjo/CCFRP/main/Icons/pacmac.png",
      iconWidth = 60, iconHeight = 110)
    icon_cpr <- makeIcon(
      iconUrl = "C:/Users/erinj/Documents/GitHub/CCFRP/Icons/choppa_lowres.png",
      iconWidth = 160, iconHeight = 85)
    fmap_corner1 <- full_map %>%
      filter(corner == 1)
    fmap_corner4 <- full_map %>%
      filter(corner == 4)
    fmap_fish <- full_map %>%
      filter(datatype == "Fishes", corner == 1, site == "M")
    fmap_phyto <- full_map %>%
      filter(datatype == "Phytoplankton", corner == 1)
    
    
    leaflet() %>%
      setView(lng = -120.843112, lat = 35.460225, zoom = 9)%>%
      addTiles() %>%
      # addRectangles(lng1 = fmap_corner1$longitude , lat1 = fmap_corner1$latitude, 
      #               lng2 = fmap_corner4$longitude  , lat2 = fmap_corner4$latitude,
      #               fillColor = "transparent")%>%
      addMiniMap(position = "bottomleft", width = 200, height = 200) %>%
      addMarkers(lng = fmap_fish$longitude, lat = fmap_fish$latitude, icon = icon_cpr,
                 label = fmap_fish$label) %>%
      addMarkers(lng = fmap_phyto$longitude, lat = fmap_phyto$latitude, icon = icon_phyto1,
                 label = fmap_phyto$label) %>%
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
library(dplyr)
library(shiny)
library(ggplot2)
library(leaflet)
library(DT)

assigned_prospects <- readRDS("example.Rda")

server <- function(input, output, session) {
  # filter data based on user's inputs
  data <- reactive({
    ap <- assigned_prospects
    if (input$do == "All") {
      ap
    } else {
      ap <- ap %>%
        filter(officer == input$do)
    }
    if (input$priority == "All") {
      ap
    } else {
      ap <- ap %>%
        filter(priority == input$priority)
    }
    if (input$rating == "All") {
      ap
    } else {
      ap <- ap %>%
        filter(Prospect_rating == input$rating)
    }
    if (input$stage == "All") {
      ap
    } else {
      ap <- ap %>%
        filter(stage == input$stage)
    }
    if (input$affin == "All") {
      ap
    } else {
      ap <- ap %>%
        filter(connection == input$affin)
    }
    if (input$seg == "All") {
      ap
    } else {
      ap <- ap %>%
        filter(constituency == input$seg)
    }
    return(ap)
  })
  
  # Create Leaflet map based on reactive data()
  output$map <- renderLeaflet({
    leaflet(height = 100) %>%
      addTiles() %>%
      setView(-95.396050, 39.532776, zoom = 4) %>%
      addCircleMarkers(data = data(), ~jitter(longitude, factor = .01), ~jitter(latitude, factor = .01), 
                      radius = 5, stroke = FALSE, fillOpacity = 0.5, popup = ~name)
  })
  
  # Display table of values based on what is on map
  tabledata <- reactive({
    t <- data() %>%
      select(name:class, stage:officer, priority, total_giving, Prospect_rating, latitude, longitude)
    return(t)
  })
  
  alumniInBounds <- reactive({
    
    if (is.null(input$map_bounds))
      return(data()[FALSE,])
    
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(tabledata(),
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  output$table <- DT::renderDataTable({
    alumniInBounds()
  })
  
  output$bargraph <- renderPlot({
    ggplot(data(), aes_string(input$xvar)) + geom_bar(fill = "#00cccc", colour = "black") + xlab(input$xvar) + theme_classic()
  })
  
  
}


ui <- basicPage(
  includeCSS("styles.css"),
  h1("Prospect Visualizer"),
  h4("Note: All data is randomly generated."),
  sidebarLayout(
    sidebarPanel(selectInput("do", "Primary Development Officer", 
                             choices=c("All", unique(assigned_prospects$officer)),
                             selected = "All", width = 175),
                 selectInput("priority", "Donor Priority",
                             choices=c("All", unique(assigned_prospects$priority)), width = 175),
                 selectInput("rating", "Capacity Rating",
                             choices=c("All", unique(assigned_prospects$Prospect_rating)), width = 175),
                 selectInput("stage", "Stage", choices = c("All", unique(assigned_prospects$stage)),
                             selected = "All", width = 175),
                 selectInput("affin", "Affinity", choices=c("All", unique(assigned_prospects$connection)),
                             selected = "All", width = 113),
                 selectInput("seg", "Constituency", choices = c("All", unique(assigned_prospects$constituency)),
                             selected = "All", width = 200), width = 2),
    mainPanel(fluidRow(
      column(7,
             leafletOutput("map", height = 520)),
      column(5,
             plotOutput("bargraph"),
             selectInput("xvar", "Display", choices = c("stage", "Prospect_rating", "connection", "priority", "constituency"), 
                         selected = "status")
             )
      # TODO create plot to display number of connection types
      ), width = 10
      
    )
  ),
  fluidRow(
    column(12,
           DT::dataTableOutput("table", width="98%")))
)

shinyApp(ui=ui, server=server)

library(shiny)
library(leaflet)
library(magrittr)
library(RColorBrewer)
##SFdata<- read.csv('Police_Department_Incidents.csv')
crimedata<-read.csv('crimedata.csv', stringsAsFactors = FALSE)

ui <- bootstrapPage(
  headerPanel("2015 Crimes in San Francisco"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("SFmap"),
  absolutePanel(top = 20, right = 20,
                selectInput('Category', 'Choose the category', choices = c('BRIBERY'=1, 'SUICIDE'=2, 'ARSON' =3, 'BURGLARY'=4, 'DRUNKENNESS'=5, 'VEHICLE THEFT'=6))
                
  ),
  h4('The number of crimes in a given category on each day of the week'),
  plotOutput("plot2"),
  tableOutput("MarkedCrime")
)

crimePlot<-function(new_crimedata){
  crimeTable<-table(new_crimedata$DayOfWeek)
  print(crimeTable)
  plot2<- plot(crimeTable, col="red", bg=109, lty=5, type= 'o', xlab='DayOfWeek')
  return(plot2)}

server <- function(input, output, session) {
  selected_crimedata <- reactiveValues(clickedMarker=NULL)
  filteredData <- reactive({
    crimename<-c('BRIBERY', 'SUICIDE', 'ARSON', 'BURGLARY', 'DRUNKENNESS','VEHICLE THEFT')
    print(crimename[as.numeric(input$Category)])
    new_crimedata<<-SFdata[SFdata$Category==crimename[as.numeric(input$Category)],]
    output$plot2<- renderPlot(crimePlot(new_crimedata))
    return(new_crimedata)
    
  })
  
  print("Test")
  output$SFmap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%   
      setView(-122.42, 37.78, zoom = 13) %>% 
      addCircleMarkers(data=filteredData(), filteredData()$X, filteredData()$Y,popup = filteredData()$Category, radius = 5,clusterOptions = markerClusterOptions())
  })
  
  
  observeEvent(input$SFmap_marker_click,{
    print("observed map_marker_click")
    selected_crimedata$clickedMarker <- input$SFmap_marker_click
    print(selected_crimedata$clickedMarker)
    output$MarkedCrime <- renderTable({
      Longitude = c(data=selected_crimedata$clickedMarker$lng ) 
      Latitude = c(data=selected_crimedata$clickedMarker$lat) 
      df = data.frame(Longitude, Latitude)  
      return(df)
      caption="Longitude and Latitude of selected crime location"
      ##return(list(data=selected_crimedata$clickedMarker$lng, data=selected_crimedata$clickedMarker$lat))
      ##colnames(MarkedCrime) <- c("lat", "lng")
      ##colnames(MarkedCrime)=c("lat","lng")
      MarkedCrime
      },caption="Longitude and Latitude of selected crime location")}
    )

    
}
shinyApp(ui, server)
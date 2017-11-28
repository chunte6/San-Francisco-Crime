library(shiny)
library(dplyr)
library(magrittr)
library(lubridate)
library(BH)
crimedata<-read.csv('crimedata.csv', stringsAsFactors = FALSE)

crimeplot<-function(dayOfWeek, TimeStart, TimeEnd, Category) {
  policeData <- subset(crimedata, DayOfWeek == dayOfWeek)
  policeTable <- table(hour(policeData$Time))
  policeTable<- as.table(policeTable[(TimeStart +1):(TimeEnd+1)])
  policePlot <- plot(policeTable, type = 'o', xlab = 'Time', xlim = c(TimeStart, TimeEnd))
  print(policeTable)
  return(policePlot)
}

ui <- shinyUI(pageWithSidebar(
  headerPanel('Police Incidents in SF in 2015'),
  sidebarPanel(
    selectInput('dayOfWeek', 'Choose the day', choices = c('Sunday' = 1, 'Monday' = 2, 'Tuesday' = 3, 'Wednesday' = 4, 'Thursday' = 5, 'Friday' = 6, 'Saturday' = 7)),
    sliderInput('TimeStart', 'Choose the starting hour', min = 0, max = 24, value = 0),
    sliderInput('TimeEnd', 'Choose the ending hour', min = 0, max=24, value=0),
    selectInput('Category', 'Choose the category', choices = c('Bribery'=1, 'Suicide'=2))
  ),
  mainPanel(
    p('This shiny app will show you the police incidents.'),
    h4('The day you chose:'),
    verbatimTextOutput('dayOfWeek'),
    h4('The starting time you chose:'),
    verbatimTextOutput('TimeStart'),
    h4('The ending time you chose:'),
    verbatimTextOutput('TimeEnd'),
    h4('The category you chose:'),
    verbatimTextOutput('Category'),
    h4('Police Incident Category'),
    plotOutput('policePlot')
    
  )
))

function(input, output) {
  output$dayOfWeek <- renderPrint({input$DayOfWeek})
  output$TimeStart <- renderPrint({input$TimeStart})
  output$TimeEnd <- renderPrint({input$TimeEnd})
  output$Category <- renderPrint({input$Category})
  output$crimePlot <- renderPlot({
    title<-"Crime at Certain Time of Certain Day Chosen"
    policePlot(input$DayOfWeek, input$TimeStart,input$TimeEnd, input$Category, main=title)
    })
}

shinyApp(ui = ui, server = server) 
#install.packages("devtools")
library(devtools)
#install_github("ramnathv/rCharts")
library(shiny)
library(shinydashboard)
library(rCharts)

#read the data
#dataframe_sanky <- read.csv("data/PTW_Visual_updated.csv")

ui<-
  #dashboardPage(
  #dashboardHeader(title = 'Path Value Analysis',titleWidth = 350),
  #dashboardSidebar(width=0
  #),
  #dashboardBody(
    fluidPage(
      headerPanel("Hello !"),
      mainPanel(
        actionButton("button", "Click !"),
        textOutput("textoutput")
      ))


server<-(function(input, output, session){
 
  processText <- function(text, timer){
    text = paste(text,"Changed")
    text
  }
  
  rv = reactiveValues()
  
  rv$message = "Initial Message"
  
  df <- data.frame(text =  c("Message1" , "Message2", "Message3"), timer = c(1,10,15))

  Vector <- c()
  observeEvent(input$button, {
    withProgress({
      
      for(i in 1:3)
      {
       Vector[i]<- processText(df$text[i],df$timer[i])
       setProgress(message = processText(df$text[i],df$timer[i]))
       Sys.sleep(df$timer[i])
       #output$textoutput <- renderText(rv$message)
      }
      
      #setProgress(message = df$text[2])
      #Sys.sleep(df$timer[2])
      
      #setProgress(message = df$text[3])
      #Sys.sleep(df$timer[3])
      
    })
  })
  
  observe({
    str = ""
    for(i in 1:3)
    {
      #browser()
      str = paste(str, i)
    }
   # browser()
    
    rv$message <- str
    output$textoutput <- renderText(rv$message)
    #output$textoutput <- renderText({ str })
  })
  
})
    
shinyApp(ui = ui, server = server)
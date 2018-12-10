library(shiny)
library(data.table)
library(scales)
library(LDAvis)

#reading the data
varName<-"variable_1_OpenEnd"
basePath<-"C:/Users/ramya.ananth/Desktop/text mining/"
freqPath<-paste(basePath,varName,sep="/")
freqWords<-read.csv(paste(freqPath,"termsList.csv",sep="/"))
#changing the column names to remove special characters
#setnames(data,colnames(data),gsub("[^[:alnum:]]","_",colnames(data)))



# Define UI for slider demo application
shinyUI(fluidPage(
  
  #  Application title
  
  #####titlePanel("Survey Text Mining"),
  titlePanel("Survey: Text Mining"),
  
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      selectInput("FreqWords",label = "Select the Frequent Word", choices = unique(freqWords$word))
       ),
    
    # Show a table summarizing the values entered
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Frequency Report",
                           div(id="ab",plotOutput("FreqReport"),dataTableOutput("FreqReportTable"))),
                  tabPanel("RelationReport",
                           div(id="ab", plotOutput("RelationReport"),dataTableOutput("RelationReportTable"))),
                  tabPanel("Topic Modeling Report", visOutput("TopicModels"))
      )
    )
                  
  )
))
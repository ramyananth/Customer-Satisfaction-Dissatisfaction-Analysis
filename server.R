library(shiny)
library(data.table)
library(Hmisc)
library(ggplot2)
library(scales)
library(wordcloud)
library(LDAvis)
library(stringr)
library(stringi)
library(topicmodels)


#reading the data
varName<-"variable_1_OpenEnd"
basePath<-"C:/Users/ramya.ananth/workspace/text_mining/"
freqPath<-paste(basePath,varName,sep="/")
topicModelsPath<-paste(basePath,"topicModels/ldaOut.RData",sep="")

# Define server logic for slider
shinyServer(function(input, output) {
  
  
  output$FreqReport<-renderPlot({
    freqWords<-fread(paste(freqPath,"termsList.csv",sep="/"))
    wordcloud(freqWords$word,freqWords$freq,scale=c(4,1),colors=brewer.pal(8, "Dark2"),random.color = T,fixed.asp = F,rot.per = 0)
  })
  
  output$FreqReportTable<-renderDataTable({
    freqWords<-fread(paste(freqPath,"termsList.csv",sep="/"))  
  })
  
  output$RelationReport<-renderPlot({
    relationPath<-paste(basePath,"relationship analysis/relationship report/myreport/",varName,"/",input$FreqWords,"/relationship.csv",sep="")
    relationData<-read.csv(relationPath,stringsAsFactors = F)
    relationData<-relationData[1:50,]
    func_split<-function(x){unlist(strsplit(x,".",fixed = T))[2]}
    relationData$words<-apply(relationData["word"],1,func_split)
    wordcloud(relationData$words,relationData$assocScore,scale=c(4,1),colors=brewer.pal(8, "Dark2"),random.color = T,fixed.asp = F,rot.per = 0)
  })
  
  output$RelationReportTable<-renderDataTable({
    relationPath<-paste(basePath,"relationship analysis/relationship report/myreport/",varName,"/",input$FreqWords,"/relationship.csv",sep="")
    print(relationPath)
    relationData<-read.csv(relationPath)
    func_split<-function(x){unlist(strsplit(x,".",fixed = T))[2]}
    relationData$words<-apply(relationData["word"],1,func_split)
    relationData<-relationData[c("words","assocScore")]
    
  })
  
  
  output$TopicModels<-renderVis({
    load(topicModelsPath)
    fitted = ldaOut
    corpus = docs
    doc_term = dtm
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
      temp <- paste(corpus[[i]]$content, collapse = ' ')
      doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    #temp_frequency <- inspect(doc_term)
    #freq_matrix <- data.frame(ST = colnames(temp_frequency),
    #                          Freq = colSums(temp_frequency))
    #rm(temp_frequency)
    dtmdf<-as.data.frame(as.matrix(dtm))
    colSums(dtmdf)
    # Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                   vocab = vocab,
                                   doc.length = doc_length,
                                   term.frequency = colSums(dtmdf))
  })
  
})



#code for automatic topic detection
#this code uses topicmodels package in R for generation of topics instead of lda

#Parameters required
#-----------------------------------------------------------------
input_path<-"C:/Users/ramya.ananth/workspace/text_mining/Bad Customer experince data_new.csv"
varName<-c("variable_1_OpenEnd")
reportLoc<-"C:/Users/ramya.ananth/workspace/text_mining/topicModels/"
topics<-3
iter<-100
keywords<-10

dir.create(reportLoc)

#Libraries required
#-----------------------------------------------------------------
library('tm')
library('stringr')
library('data.table')
library('topicmodels')
library('dplyr')
library('stringi')
library('LDAvis')
library('servr')

#---------------------------------------------------------------------------------------
# Loading the data
#---------------------------------------------------------------------------------------
data<-read.csv(input_path)
#---------------------------------------------------------------------------------------



#changing the column names to remove special characters
setnames(data,colnames(data),gsub("[^[:alnum:]]","_",colnames(data)))
varName <- gsub("[^[:alnum:]]","_",varName)
data[,varName]<-tolower(as.character(data[,varName]))

evaluate=TRUE

ir=c()
while(evaluate){
  if(length(ir)){
  data <- data[-ir,]
  }
  # index of "^$"|"^\\s*$" and NA in analysisData
  index_remove <- grepl(pattern = "(^$)|(^\\s*$)", x = data[,varName])
  index_remove <- index_remove | is.na(data[,varName])
  
  if(any(as.logical(index_remove))){
  data<-data[-index_remove,]
  }
  
  
  
  #create corpus from vector
  docs <- Corpus(VectorSource(data[,varName]))
  
  
  #start preprocessing
  #Transform to lower case
  docs <-tm_map(docs,content_transformer(tolower))
  
  
  #remove potentially problematic symbols
  toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
  docs <- tm_map(docs, toSpace, "[^[:alnum:] ]")
  
  #remove stopwords
  #docs <- tm_map(docs, removeWords, stopwords("english"))
  
  #remove whitespace
  docs <- tm_map(docs, stripWhitespace)
  #Stem document
  #docs <- tm_map(docs,stemDocument)
  
  
  #Create document-term matrix
  dtm <- DocumentTermMatrix(docs)
  
  #collapse matrix by summing over columns
  freq <- colSums(as.matrix(dtm))
  
  rowS = rowSums(as.matrix(dtm))
  ir = which(rowS == 0)
  if(length(ir) == 0){
    evaluate = FALSE
  }
}
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)

#List all terms in decreasing order of freq and write to disk
write.csv(freq[ord],paste(reportLoc,"word_freq.csv",sep=""))




#Number of topics
k <- topics
best <- TRUE

  #Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(best=best))

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste(reportLoc,"LDAGibbs",k,"DocsToTopics.csv",sep=""))


#top desired terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,keywords))
write.csv(ldaOut.terms,file=paste(reportLoc,"LDAGibbs",k,"TopicsToTerms.csv",""))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste(reportLoc,"LDAGibbs",k,"TopicProbabilities.csv",sep=""))


topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Required packages
  
  # Find required quantities
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
  
  return(json_lda)
}

jsonObj<-topicmodels_json_ldavis(ldaOut,docs,dtm)
save(ldaOut,docs,dtm,file=paste(reportLoc,"ldaOut.RData",sep=""))
serVis(jsonObj, out.dir = 'vis', open.browser = TRUE)



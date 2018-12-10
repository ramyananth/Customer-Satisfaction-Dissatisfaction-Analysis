

#Parameters required
#-----------------------------------------------------------------
input_path="C:/Users/ramya.ananth/workspace/text_mining/final_trans_1000/transcripts/calls_995_trans_grouped.csv"
output_path="C:/Users/ramya.ananth/workspace/text_mining/final_trans_1000/transcripts/calls_995_trans_grouped_pos.csv"
varName<-c("transcripts")
extractVerbs <- "TRUE"
extractNouns <- "TRUE"
extractAdjectives <- "TRUE"


#Note: 

# openNLP has a dependency on rJava which uses configured java path. Sometimes
# when is not configured in the path or java_path in system paths, it fails to
# load. So please provide the java path below so that the code can configure
# it for you




#Configurations to load rJava library
#====================================

# #Reading java path from the system paths
# #---------------------------------------
jPath <- "C:/Program Files (x86)/Java/jre1.6.0_24"
# 
# #Adding client to JPath as jvm.dll exists inside that folder is used to load rJava
# #---------------------------------------------------------------------------------
jPath.client=paste(jPath,'/client',sep='')
# 
# #Checking for jre client or jre server
# #-------------------------------------
if(file.exists(jPath.client)){
  jPath <- jPath.client
}else{
  jPath <- paste(jPath,'/server',sep='')
}
# 
# #Setting temporary java client/server path
# #-----------------------------------------
try(Sys.setenv('path'=paste(Sys.getenv('path'),jPath,sep=';')),silent=TRUE)
try(Sys.setenv("JAVA_HOME" = jPath))


#Libraries required
#-----------------------------------------------------------------

library('tm')
library('openNLP')
library('stringr')
library('rJava')
library('RTextTools')
library('NLP')
library('data.table')

#---------------------------------------------------------------------------------------
# Loading the data
#---------------------------------------------------------------------------------------
data<-read.csv(input_path)
#---------------------------------------------------------------------------------------

#changing the column names to remove special characters
setnames(data,colnames(data),gsub("[^[:alnum:]]","_",colnames(data)))


# Defining some custom functions
# 1. To install required packages
#---------------------------------------------------------------------------------------
tagPOS <- function(corpus, language = "en"){
  
  sent_token_annotator  <- Maxent_Sent_Token_Annotator()
  word_token_annotator  <- Maxent_Word_Token_Annotator()
  pos_tag_annotator     <- Maxent_POS_Tag_Annotator()
  
  corpus.set.to.return  <- NULL 
  for(i in 1:length(corpus)){
    corpus.element.annotated <- annotate(corpus[i], 
                                         list(sent_token_annotator,
                                              word_token_annotator))
    
    
    
    pos.tagged <- annotate(corpus[i], pos_tag_annotator, 
                           corpus.element.annotated)
    pos.tagged.word <- subset(pos.tagged, type == "word")
    
    tags <- sapply(pos.tagged.word$features, `[[`, "POS")
    
    
    sent.tagged <-  paste(apply(cbind(pos.tagged.word$start,pos.tagged.word$end, tags),1,
                                function(word.terms, sent){return(paste(substr(sent,word.terms[1],word.terms[2]),word.terms[3],sep="/"))},
                                sent=corpus[i]),collapse=" ")
    
    corpus.set.to.return[i] <- sent.tagged
    
  }
  return(corpus.set.to.return)
}

posTagger <- function(varName,
                      extractVerbs,
                      extractNouns,
                      extractAdjectives) {
  
  dataSize <- nrow(data)
  
  varIndex <- which(colnames(data)==varName)
  
  
  # To covert dataset into a Corpus; required for executing 'tm_map' functions
  #----------------------------------------------------------------------------
  
  dataList <- Corpus(VectorSource(data[,varIndex]))
  
  
  # To remove punctuations from dataset
  #----------------------------------------------------------------------------
  
  dataList <- tm_map(dataList, removePunctuation, preserve_intra_word_dashes = TRUE)
  dataList <- unlist(dataList[1:dataSize])
  
  # POS-tag the dataset
  #----------------------------------------------------------------------------
  
  dataList <- tagPOS(dataList, language = "en")
  
  nounList = c()
  verbList = c()
  adjList = c()
  
  # Extracting nouns, verbs and adjectives from the tagged data
  #----------------------------------------------------------------------------
  
  for(i in 1:length(dataList)) {
    
    wordlist <- unlist(strsplit(dataList[i], " ", fixed=TRUE))
    splitVars <- sapply(wordlist, function(str){strsplit(str, "/", fixed=TRUE)},
                        simplify = TRUE, USE.NAMES = FALSE)
    
    splitMat <- as.data.frame(t(data.frame(splitVars)))
    
    words = as.character(splitMat$V1)
    postags = as.vector(splitMat$V2)
    
    noun = ""
    verb = ""
    adj = ""
    
    for(j in 1:length(words)) {
      
      tag2 = substr(postags[j],1,2)
      if(length(tag2)==0){
        print(tag2)
        
      }
      else{
        
        if(tag2 == 'NN') {
          noun = paste(noun,words[j],sep=" ")
        }
        
        if(tag2 == 'VB') {
          verb = paste(verb,words[j],sep=" ")
        }
        
        if(tag2 == 'JJ') {
          adj = paste(adj,words[j],sep=" ")
        }
        
      }
    }
    
    nounList = c(nounList,noun)
    verbList = c(verbList,verb)
    adjList = c(adjList,adj)
    
  }
  
  verbList = tolower(verbList)
  nounList = tolower(nounList)
  adjList = tolower(adjList)
  
  tmDataSetNew <- cbind(data)
  
  # Adding POS entity data to the original dataset
  #----------------------------------------------------------------------------
  i=0
  col=NULL
  if(extractVerbs) {
    tmDataSetNew <- cbind(tmDataSetNew,verbList)
    col<-c(col,paste(varName,"_verbList",sep=""))
    i=i+1
  }
  
  if(extractNouns) {
    tmDataSetNew <- cbind(tmDataSetNew,nounList)
    col<-c(col,paste(varName,"_nounList",sep=""))
    i=i+1
  }
  
  if(extractAdjectives) {
    tmDataSetNew <- cbind(tmDataSetNew,adjList)
    col<-c(col,paste(varName,"_adjList",sep=""))
    i=i+1
  }
  
  if(extractVerbs | extractNouns | extractAdjectives){
    colnames(tmDataSetNew)[(length(tmDataSetNew)-(i-1)):length(tmDataSetNew)]<-col
  }
  
  
  
  return(tmDataSetNew)
}
colnm<- NULL

tmDataSetFinal<-data.frame()
ev <- extractVerbs
en <- extractNouns
ea <- extractAdjectives
for (i in 1:length(varName)) {
  extractVerbs <- ev
  extractNouns <- en
  extractAdjectives <- ea
  v <- paste(varName[i],'verbList',sep="_")
  n <- paste(varName[i],'nounList',sep="_")
  a <- paste(varName[i],'adjList',sep="_")
  if(v %in% colnames(data)) extractVerbs = FALSE
  if(n %in% colnames(data)) extractNouns = FALSE
  if(a %in% colnames(data)) extractAdjectives = FALSE
  dat<-c(colnm, try(posTagger(varName[i], as.logical(extractVerbs), as.logical(extractNouns), as.logical(extractAdjectives)),silent=T))
  if(i == 1){
    tmDataSetFinal <- dat
  }else{
    tmDataSetFinal<-cbind(tmDataSetFinal,dat)
  }
}


write.csv(tmDataSetFinal,output_path,row.names = FALSE)

# To clear all variable used
#----------------------------------------------------------------------------
#rm(list = ls())
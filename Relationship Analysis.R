

#---------------------------------------------------------------------------------------
# Parameters needed
#---------------------------------------------------------------------------------------
 input_path      <- "C:/Users/ramya.ananth/workspace/text_mining/Bad Customer experince data_new.csv"
 output_path     <- "C:/Users/ramya.ananth/workspace/text_mining/"
 varName         <- c("variable_1_OpenEnd")
 keyword         <- c("")
 reportName      <- 'myreport'
 KeywordPath     <- "C:/Users/ramya.ananth/workspace/text_mining/variable_1_OpenEnd/termsList.csv"
#---------------------------------------------------------------------------------------



library(tm)
library(lda)
library(stringr)
library(RTextTools)
library(NLP)
#---------------------------------------------------------------------------------------

 #loading the Keywords
 if(KeywordPath != ""){
  keys <- fread(KeywordPath)
  keyword <- keys[,word]
  }
 
 #---------------------------------------------------------------------------------------
 # Loading the data
 #---------------------------------------------------------------------------------------
 data<-read.csv(input_path)
 #---------------------------------------------------------------------------------------
 
 #changing the column names to remove special characters
 setnames(data,colnames(data),gsub("[^[:alnum:]]","_",colnames(data)))
 
 
 
#---------------------------------------------------------------------------------------
# Function to generate the association report
#---------------------------------------------------------------------------------------
genWordAssocReports <- function(varName, keyword, output_path) {
  
  print(keyword)
  # To read data from a csv file and create a dataset from required column(s)
  #---------------------------------------------------------------------------------
  varIndex  <- which(colnames(data)==varName)
  corpus    <- tolower(data[,varIndex])
  corpus    <- corpus[grep(pattern=keyword,x=corpus)]
  corpus <- str_replace_all(corpus, "[^[:alnum:]]", " ")
  
  # To create a document term matrix from the dataset
  #----------------------------------------------------------------------------
  dtmatrix <- create_matrix(corpus,language="english",
                            minWordLength = 3,minDocFreq = 1,ngramLength = 0,
                            removeNumbers = FALSE,stemWords = FALSE,
                            removePunctuation = FALSE,stripWhitespace = FALSE,
                            toLower = TRUE,removeStopwords = FALSE,
                            weighting = weightTf)
  
  
  # To find associations for a given keyword and sort the results in descending order of scores
  #--------------------------------------------------------------------------------------------
  associationList <- findAssocs(dtmatrix, keyword, 0.0)
  
  rm("dtmatrix")
  # Creating word and frequency columns
  #----------------------------------------------------------------------------
  word        <- names(unlist(associationList))
  assocScore  <- as.numeric(unlist(associationList))
  
  # Creating a dataset with the above two columns
  #----------------------------------------------------------------------------
  ap.d <- data.frame(word,assocScore)
  ap.d.sorted <- ap.d[order(-assocScore,word),]
  
  # To remove the analyzed keyword from the association list
  #---------------------------------------------------------
  if(ap.d.sorted$word[1]==keyword) {
    ap.d.sorted = ap.d.sorted[2:nrow(ap.d.sorted),]
  }
  
  # To write the association report in a csv
  #----------------------------------------------------------------------------
  
  dir.create(paste(output_path,"relationship analysis","relationship report",reportName,varName,keyword,sep="/"),recursive=T)   # Location of the output file
  #----------------------------------------------------------------------------
  location <- paste(output_path,"relationship analysis","relationship report",reportName,varName,keyword,"relationship.csv",sep="/")
  # Writing the CSV
  #----------------------------------------------------------------------------
  write.csv(ap.d.sorted, file = location, append = FALSE, col.names = TRUE, row.names = FALSE, quote=F)
  
  # To clear all variable used
  #----------------------------------------------------------------------------
  rm(list=c("input_path","fileName","fileLoc","varName","corpus","word",
            "output_path","varIndex","assocScore","associationList",
            "ap.d","ap.d.sorted","tmDataSet","dtmatrix","keyword"))
  
}
#---------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------------
# Calling the functions
#---------------------------------------------------------------------------------------

for( i in 1:length(keyword)){
    check <- genWordAssocReports(varName, keyword[i], output_path)
  }
#---------------------------------------------------------------------------------------
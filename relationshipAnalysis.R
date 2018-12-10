#---------------------------------------------------------------------------------------
# Sample Parameters
#---------------------------------------------------------------------------------------
input_path      <- "C:/Users/ramya.ananth/workspace/text_mining/Bad Customer experince data_new.csv"
output_path     <- "C:/Users/ramya.ananth/workspace/text_mining/new"
varName         <- c("variable_1_OpenEnd")
keyword         <- c("")
reportName      <- 'myreport'
KeywordPath     <- "C:/Users/ramya.ananth/workspace/text_mining/variable_1_OpenEnd/termsList.csv"

#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# Libraries
#---------------------------------------------------------------------------------------
library(lsa)
library(tau)
library(data.table)
library(tm)
library(RTextTools)
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# Function: to generate word association
#---------------------------------------------------------------------------------------

# Custom Function: To find words similar to the given term
termAssociationLSA <- function(matrixLSA,term) {
  
  compareTermsTable<-round(as.data.frame(associate(matrixLSA, term, measure = "cosine", threshold = 0.1)),2)
  compareTermsTable <- cbind(word=row.names(compareTermsTable),compareTermsTable)
  colnames(compareTermsTable)[2]= "score"
  row.names(compareTermsTable) <- NULL
  return(compareTermsTable)
}


# Custom Function: To call the LSA function
CreateLSAMatrix<-function(text){
  splt<-strsplit(text," ")  
  dtm1<-Corpus(VectorSource(splt), readerControl = list(language="en")) #specifies the exact folder where my text file(s) is for analysis with tm.
  ctrl <- list(wordLengths = c(2, Inf))
  dtm2<-t(as.DocumentTermMatrix(DocumentTermMatrix(dtm1,control = ctrl)))
  dtm2<-as.textmatrix(as.matrix(dtm2))
  myMatrix = lw_logtf(dtm2) * gw_idf(dtm2)
  return(lsa(myMatrix, dims=dimcalc_share(share = .8)))
  
}

# Custom Function: To generate the relationship report for the given term
genWordAssocReports<-function(inputPath,data,varName,keyword,output_path) {
  
  # To write the association report in a csv
  #----------------------------------------------------------------------------
  dir.create(paste(output_path,"relationship analysis","relationship report",reportName,varName,sep="/"),recursive=T)
  
  data    <- as.character(data[grep(pattern=paste("\\b",keyword,"\\b",sep = ""),x=data,ignore.case = TRUE)])
  if(length(data)==1){
    data <- c(data,"")
  }
  myMatrix <- as.textmatrix(CreateLSAMatrix(data))
  for(word in keyword){
  associationTable <- termAssociationLSA(myMatrix,word)
  
  # Location of the output file
  #----------------------------------------------------------------------------
  
  location <- paste(output_path,"relationship analysis","relationship report",reportName,varName,word,sep="/")
  dir.create(location)
  # Writing the CSV
  #----------------------------------------------------------------------------
  #plot_neighbors(x = as.character(associationTable[1:10,"word"]),method = "PCA", n=10 ,dims = 3, tvectors = as.textmatrix(CreateLSAMatrix(data)))
  write.csv(associationTable, file = paste(location,"relationship.csv",sep="/"), append = FALSE, col.names = TRUE, row.names = FALSE, quote=FALSE)
  }
  # To clear all variable used
  #----------------------------------------------------------------------------

}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#loading the Keywords
if(KeywordPath != ""){
  keys <- fread(KeywordPath)
  keyword <- keys[,word]
}

#---------------------------------------------------------------------------------------
# Loading the data
#---------------------------------------------------------------------------------------
data_use<-read.csv(input_path,stringsAsFactors = FALSE)

#---------------------------------------------------------------------------------------

#changing the column names to remove special characters
setnames(data_use,colnames(data_use),gsub("[^[:alnum:]]","_",colnames(data_use)))

index_true<-which(is.locale(as.character(data_use[,varName])) == "FALSE")
if(length(index_true))
{
  data_use[index_true,varName]<-iconv(as.character(data_use[index_true,varName]),from="latin1" ,to = "UTF-8")
}
data <- data_use[,varName]

check <- try(genWordAssocReports(input_path, data, varName, keyword, output_path),silent=T)
  if(!class(check)=='try-error'){
    write.table("Relationship Report successfully generated",paste(output_path,"relationship analysis","relationship report",reportName,"RELATIONSHIP_REPORT_COMPLETED.txt",sep="/"),quote=FALSE,row.names=F,col.names=F)
  }   



#Parameters required
#-----------------------------------------------------------------
input_path="C:/Users/ramya.ananth/workspace/text_mining/Bad Customer experince data.csv"
output_path="C:/Users/ramya.ananth/workspace/text_mining/Bad Customer experince data_new.csv"
varName=c("OpenEnd")
newVarName=c("variable_1")
removeEmails  = TRUE
removeUrls = TRUE
removePhoneNumber = TRUE
removeNumber = TRUE
removePunctuations = TRUE
removeStopwords = TRUE
stripWhitespaces = TRUE
stemDoc = FALSE


library('tm')
library('stringr')
library('RTextTools')
library('data.table')

#---------------------------------------------------------------------------------------
# Loading the data
#---------------------------------------------------------------------------------------
data<-read.csv(input_path)
#---------------------------------------------------------------------------------------

#changing the column names to remove special characters
setnames(data,colnames(data),gsub("[^[:alnum:]]","_",colnames(data)))



#subsetting the data for walmart
data[,varName] <- gsub(pattern = "[^[:alnum:] ,]","",data[,varName])
data <- data[-(which(str_trim(as.character(data$Question)) == "Q37e")),]
data <- data[-(which(str_trim(as.character(data[,varName])) == "")),]


index<-1:nrow(data)

#---------------------------------------------------------------------------------------
# function : datacleansing
#---------------------------------------------------------------------------------------
dataHandling <- function (x,
                          removeEmails  = FALSE,
                          removeUrls = FALSE,
                          removePhoneNumber = FALSE,
                          removeNumber = FALSE,
                          removePunctuations = FALSE,
                          removeStopwords = FALSE,
                          stripWhitespaces = FALSE,
                          stemDoc = FALSE) {
  
  
  
  # Regular expressions to match 1. Email, 2. URL, 3. Phone number
  #---------------------------------------------------------------
  
  email.expression <- "[A-Za-z0-9-]+[.A-Za-z0-9-]*@[A-Za-z0-9-]+(\\.com|\\.co.in|\\.net|\\.org|\\.info|\\.edu|\\.mil|\\.gov|\\.biz|\\.ws|\\.us|\\.tv|\\.cc|\\.aero|\\.arpa|\\.coop|\\.int|\\.jobs|\\.museum|\\.name|\\.pro)|\\.travel|\\.nato)"
  url.expression <- "(http://|https://|www.)[[:alnum:]~!#$%&+-=?,:/;._]*"
  phonenumber.expression <- "\\+?(\\d{2,3})[- ]?\\(?(\\d{3,5})\\)?[- ]?(\\d{3,5})[- ]?(\\d{4})?"
  
  # To read data from a single csv file and create a dataset of required column
  #----------------------------------------------------------------------------
  
    
  corpus <- as.character(x)
 
  # To remove emails from dataset
  #----------------------------------------------------------------------------
  
  
  if(removeEmails) {
    corpus <- gsub(email.expression, ' ', corpus, ignore.case=T)
  }
  
  # To remove urls from dataset
  #----------------------------------------------------------------------------
  
  if(removeUrls) {
    corpus <- gsub(url.expression, ' ', corpus, ignore.case=T)
  }
  
  # To remove phone numbers from dataset
  #----------------------------------------------------------------------------
  
  if(removePhoneNumber) {
    corpus <- gsub(phonenumber.expression, ' ', corpus, ignore.case=T, perl=TRUE)
  }
  
  # To covert dataset into a Corpus; required for executing 'tm_map' functions
  #----------------------------------------------------------------------------
  
  corpus <- Corpus(VectorSource(corpus))
  
  # To remove stopwords from corpus
  #----------------------------------------------------------------------------
  
  if(removeStopwords) {
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
  }
  
  # To remove numbers from corpus
  #----------------------------------------------------------------------------
  
  if(removeNumber) {
    corpus <- tm_map(corpus, removeNumbers)
  }
  
  # To remove punctuations from corpus
  #----------------------------------------------------------------------------
  
  if(removePunctuations) {
    corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  }
  
  # To stem words present in the corpus data
  #----------------------------------------------------------------------------
  
  if(stemDoc) {
    corpus <- tm_map(corpus, stemDocument)
  }
  
  # To remove additional whitespaces from corpus
  #----------------------------------------------------------------------------
  
  if(stripWhitespaces) {
    corpus <- tm_map(corpus, stripWhitespace)
  }
  
  # Converting corpus into a vector
  #----------------------------------------------------------------------------
  corpus           <- tm_map(corpus,PlainTextDocument)
  x           <- unlist(lapply(corpus,as.character))
  return(x)
  
}

prefix <- NULL
newvarnames <- NULL
if (exists("newVarName") & newVarName != "") {
  prefix <- newVarName
  rm("newVarName")
  newvarnames <- paste(prefix, "_", varName, sep="")
}

if (!(is.null(newvarnames))) {
  
  for(i in 1:length(varName)){
    
    temp <- as.character(data[, varName[i]])
    data[,varName[i]]<-as.character(data[,varName[i]])
    data[index, varName[i]]<-iconv(data[index, varName[i]],from = "WINDOWS-1252",to="UTF-8")
    
    temp[index] <- dataHandling (x=data[index, varName[i]],
                                 removeEmails,
                                 removeUrls,
                                 removePhoneNumber,
                                 removeNumber,
                                 removePunctuations,
                                 removeStopwords,
                                 stripWhitespaces,
                                 stemDoc)
    
    data[index, newvarnames[i]] <- temp
    
  }
  
}

data <- data[-(which(str_trim(as.character(data[,paste(prefix,"_",paste(varName),sep="")])) == "")),]

write.csv(data,output_path,row.names = FALSE)

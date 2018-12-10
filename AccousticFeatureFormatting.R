
###########################################################################
# Project: DSAT Prediction 			                                      #
# Author : Ramya Ananth                                                   #
# Code   : For cleansing accoustic variables                              #
###########################################################################


#Install the following package if it is not there in your system.
# install.packages("splitstackshape")

library(splitstackshape)

#Import the file
D1 <- read.csv("C:\\Users\\ramya.ananth\\workspace\\text_mining\\Demo\\Acoustic_Features_Combined.csv" ,header = FALSE)

#Below step is equivalent to the text to columns in Excel
D2 <- cSplit(indt = D1,splitCols = "V1",sep = " ",drop = TRUE)

#Labeling the Column Names
colnames(D2) <- c("Sl" , "FileName",
                  "N_Transction",	"Avg_Caller_Duration",
                  "Max_Caller_Duration",	"Percentage_Caller_Duration",
                  "Avg_Hold_Time",	"Maximum_Hold_time"	,"Total_Holds",
                  "Avegare_Reaction_Time",	"Begin_End_Caller_Ratio","Duration",	"Max_Emo",
                  "Impulse"	,"Wt_Avg_Caller",	"Wt_Max_Caller"	,
                  "Avg_Init_Emo_Caller",	"Avg_Fin_Emo_Caller",	"Delta_Emotion")

#Extracting the Reference ID 
D2$Collated_Reference_ID <- substr(D2$FileName,2,14)

#Removing Quotes from the FileName
D2$FileName <- gsub('"','',D2$FileName)

#Imputation
#Minor Cleansing of Data. The imputations are based on the bigger analytical dataset values.

D2$Avg_Hold_Time[is.na(D2$Avg_Hold_Time)] <- 0
D2$Maximum_Hold_time[is.na(D2$Maximum_Hold_time)] <- 0
D2$Maximum_Hold_time[is.infinite(D2$Maximum_Hold_time)] <- 0
D2$Avegare_Reaction_Time[is.na(D2$Avegare_Reaction_Time)] <- 2.15
D2$Begin_End_Caller_Ratio[is.na(D2$Begin_End_Caller_Ratio)] <- 0
D2$Begin_End_Caller_Ratio <- ifelse(D2$Begin_End_Caller_Ratio > 268.25, 268.25,D2$Begin_End_Caller_Ratio) 
D2$Impulse <- ifelse(D2$Impulse == -99 ,D2$Max_Emo ,D2$Impulse)
D2$Avg_Init_Emo_Caller[is.na(D2$Avg_Init_Emo_Caller)] <- 0
D2$Avg_Fin_Emo_Caller[is.na(D2$Avg_Fin_Emo_Caller)] <- 0
D2$Delta_Emotion[is.na(D2$Delta_Emotion)] <- 0

#Removing Sl Column and bringing Collated_Reference_ID to the beginning of the table
D2$Sl <- D2$Collated_Reference_ID
D2$Collated_Reference_ID <- NULL
colnames(D2)[1] <- "Collated_Reference_ID"


#Export
write.csv(D2,"C:\\Users\\ramya.ananth\\workspace\\text_mining\\Demo\\Acoustic_Features_Combined_U1.csv" ,row.names = FALSE)


#####End of the code#####
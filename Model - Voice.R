######################################################################
# Project: DSAT Prediction                                           #
# Author : Ramya Ananth                                              #
# Code   : model for the voice data                                  #
######################################################################

#Headers#
##Comments


#####Code for model on the voice data#####


#Set the library where the input data is available. The code exports the final data to the same location#
setwd("C:\\Users\\ramya.ananth\\workspace\\text_mining\\DemoModelScoring")

#Start:Importing the Data#
EntireData<-as.data.frame(read.csv("Analytical_data_version2B.csv",header=T,na.strings=c("" , "NA" , "-"),stringsAsFactors = TRUE))
#End:Importing the Data#

#Start:Subsetting the Data#
##The data is subsetted to the records where the current model is applicable to.
Voice<-EntireData[which(EntireData$C_RNT_TotalTouchPointChat_D_INT_C ==0 & EntireData$C_RNT_TotalTouchPointEmail_D_INT_C ==0 ),]
#End:Subsetting the Data#


#Start: Data Preparation#
##A new dataframe is created which has only the variables required by the model. 
##It helps create a concise output which could be used to study the model or do QC.

##Creating a table. Adding  Contact ID, Reference ID and relevant variables for the model
Voice2 <- as.data.frame(Voice$B_RNT_ContactId_A_Category_N)
Voice2$B_RNT_ContactId_A_Category_N <- Voice$`Voice$B_RNT_ContactId_A_Category_N`
Voice2$`Voice$B_RNT_ContactId_A_Category_N` <- NULL

Voice2$B_RNT_ContactId_A_Category_N    <- Voice$B_RNT_ContactId_A_Category_N
Voice2$B_RNT_ReferenceId_A_Category_N    <- Voice$B_RNT_ReferenceId_A_Category_N
Voice2$B_TXT_CurrentEscalationFlag_A_Category_O <- as.factor(Voice$B_TXT_CurrentEscalationFlag_A_Category_O)

##Bucketing and creating dummies
Voice2$B_ACOUS_MaxHoldTime_A_Sec_C_High <- as.factor(ifelse(Voice$B_ACOUS_MaxHoldTime_A_Sec_C>=350,"1","0"))
Voice2$B_ACOUS_MaxHoldTime_A_Sec_C_Medium <- as.factor(ifelse(Voice$B_ACOUS_MaxHoldTime_A_Sec_C>=150 & Voice$B_ACOUS_MaxHoldTime_A_Sec_C < 350,"1","0"))
Voice2$B_RNT_Process_D_Category_N_refund.return.repair <- as.factor(ifelse(toupper(Voice$B_RNT_Process_D_Category_N) %in% c("REFUND/RETURN" ,"REPAIR"),"1","0"))
Voice2$B_RNT_Parts_D_Category_N_ScreenNApp <- as.factor(ifelse(toupper(Voice$B_RNT_Parts_D_Category_N) %in% c("SCREEN ISSUE" ,"APPLICATION & ACCOUNT ISSUE"),"1","0"))
Voice2$B_TXT_PctPositiveSentence_D_INT_C_High <-  as.factor(ifelse(Voice$B_TXT_PctPositiveSentence_D_INT_C>= 0.05, "1","0"))
Voice2$C_TXT_PctNegativeSentenceCategory_D_O_M <- as.factor(ifelse(Voice$C_TXT_PctNegativeSentenceCategory_D_O == "M",1,0))
Voice2$C_TXT_PctNegativeSentenceCategory_D_O_L <- as.factor(ifelse(Voice$C_TXT_PctNegativeSentenceCategory_D_O == "L",1,0))


##Creating flag for topic variables and imputing for missing on the same
Voice2$B_TXT_VoiceTopicFour_Flag <- as.factor(ifelse(Voice$B_TXT_VoiceTopicFour_A_Count_C>0,1,0))
Voice2$B_TXT_VoiceTopicFour_Flag[is.na(Voice2$B_TXT_VoiceTopicFour_Flag) ]<- 0
Voice2$B_TXT_AgentTopicEight_flag <- as.factor(ifelse(Voice$B_TXT_AgentTopicEight_A_Count_C>0,1,0))
Voice2$B_TXT_AgentTopicEight_flag[is.na(Voice2$B_TXT_AgentTopicEight_flag)] <- 0
Voice2$B_TXT_VoiceTopicEight_Flag <- as.factor(ifelse(Voice$B_TXT_VoiceTopicEight_A_Count_C>0,1,0))
Voice2$B_TXT_VoiceTopicEight_Flag[is.na(Voice2$B_TXT_VoiceTopicEight_Flag) ]<- 0


##Creating a combination variable of Escalation Flag and Voice Topic 4, using a logical AND
Voice2$EscalationFlagWithTopic4 <- as.factor(as.numeric(Voice$B_TXT_CurrentEscalationFlag_A_Category_O)==1 & as.numeric(Voice2$B_TXT_VoiceTopicFour_Flag == 1))


#Start: Reading Model Object#
##Change location of the model object here
M_New <- readRDS("C:\\Users\\ramya.ananth\\workspace\\text_mining\\ModelObjects\\VoiceV2.rds")
#End: Reading Model Object#


#Start: Prediction/ Scoring of the dataset using the model#
##The probability cut-offs can be changed here.
Voice2$Predicted  <- as.factor(ifelse(predict(M_New,Voice2,type = 'response') >0.3, "Yes" ,"No"))
#End: Prediction/ Scoring of the dataset using the model#

#End: Prediction/ Scoring of the dataset using the model#

#Start: Exporting the final dataset#
##Full address can be given, if you want a different location.
write.csv(Voice2,"VoiceScored.csv" , row.names = FALSE)
#End: Exporting the final dataset#

#####End of Code#####
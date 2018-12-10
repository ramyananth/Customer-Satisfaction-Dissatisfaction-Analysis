'''
Purpose: This code takes in the .wav format audio file or google transcrips in 
certain format and process them accordingly.

1> In case of audio file input i.e. ".wav" file. It first converts it into raw
file format. After converting it into raw file it uses the google credentials in
.json format which needs to be given by the user, it uploads the raw file onto 
google cloud storage. we need to give the name of the bucket in which the file
needs to be uploaded. Filename on the google cloud storage is dynamically generated.
After the upload is complete, the location of file on GS is passed to the
transcription part of the code. Transcription is done using Google Speech API
and a local file trancripts.csv is generated which contains the transcripts.

Once the transcripts are generated, we pass them to the text mining module
which processes the texts using the machine learning and semantic engine and 
produces output that are stored in a result folder.



2> In case the transcripts file is directly passed as an input then it is directly
passed to the text mining module which processes the texts using the machine 
learning and semantic engine and produces output that are stored in a result folder.

Input: a ".wav" file or ".csv" file containing the trancripts

Way to run using CMD :  

    python audio_to_text_mine_output.py /home/user/test.wav
    or
    python audio_to_text_mine_output.py /home/user/transcript.csv
    
    
Output:

trancripts.csv
<test>.raw
entitySentiment.csv
escalation.csv
keywords.csv
problemDetection.csv
sentiment_with_os.csv

'''



'''
loading the packages that will be used in the program and defining the paths
'''
import sys
import os
from subprocess import check_output

home = os.path.dirname(os.path.abspath(__file__))
os.sys.path.append(home)
os.chdir(home)
print(home)
print(os.getcwd())
import pandas as pd


os.environ["JAVA_HOME"] = "/usr/lib/jvm/java-8-openjdk-amd64"

import cPickle as pickle
import mine_text.mtServicesAPI as mt
import mine_text.mcsa_word_relationships as mcwa



if sys.argv[1][-3:] == "wav":

    #Part 1: Converts the audio file which is a .wav file into a raw file
    path_of_audio_files = sys.argv[1]
    print(path_of_audio_files)
    name_of_audio_file = path_of_audio_files.split("/")[-1]
    print(name_of_audio_file)
    
    base_path = path_of_audio_files[:-4] + "/"
    try:
        os.mkdir(base_path)
        print("i created the directory as it did not exist")
    except:
        pass    
    
    
    output_path = "".join((base_path,"/","raw_files/"))
    try:
        os.mkdir(output_path)
        print("i created the directory as it did not exist")
    except:
        pass    
    
    try:
        conv_raw_text ="sox " + path_of_audio_files +" -t raw --channels=1 --bits=16 --rate=8000 --encoding=signed-integer --endian=little " + "".join((output_path,name_of_audio_file))[:-4] + ".raw"
        check_output(conv_raw_text, shell=True)
    except:
        print("converting in raw file failed")
        
    path_of_raw_file = "".join((output_path,name_of_audio_file))[:-4] + ".raw"
    name_of_raw_file = name_of_audio_file[:-4] + ".raw"
    
    
    #Part 2: Uploads the .raw file onto google cloud storage
    print("now trying to upload to google storage")
    
    #references 
    # https://github.com/salrashid123/gcpsamples
    # https://github.com/GoogleCloudPlatform/python-docs-samples/tree/master/storage/cloud-client
    
    from google.cloud import storage
    import google.auth
    import os
    
    #instead of .json give the path of the service account credential
    bucket_name = "test_audio_upload"
    os.environ["GOOGLE_APPLICATION_CREDENTIALS"]="".join((home,"/SpeechRecognition.json"))
    print(os.environ["GOOGLE_APPLICATION_CREDENTIALS"])
    credentials, project = google.auth.default()
    
    client = storage.Client(credentials=credentials)
    
    #ignore the following in case only upload is needed
    buckets = client.list_buckets()
    
    for bkt in buckets:
    	print(bkt)
    	
    #enter the bucket name where file is to be uploaded
    bucket = client.get_bucket(bucket_name)
    #pass the file name to be used on google storage
    blob = bucket.blob(name_of_raw_file)
    
    #pass the path of the file that is to be uploaded
    blob.upload_from_filename(path_of_raw_file)
    
    blobs = bucket.list_blobs()
    for blob in blobs:
    	print(blob.name)
     
    
    
    print("google cloud upload complete")
    
	#Part 3: Runs the Google Speech API for trancription
	
	print("starting the transcription now") 
     
    #!/usr/bin/env python
    # Copyright 2016 Google Inc. All Rights Reserved.
    #
    # Licensed under the Apache License, Version 2.0 (the "License");
    # you may not use this file except in compliance with the License.
    # You may obtain a copy of the License at
    #
    #      http://www.apache.org/licenses/LICENSE-2.0
    #
    # Unless required by applicable law or agreed to in writing, software
    # distributed under the License is distributed on an "AS IS" BASIS,
    # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    # See the License for the specific language governing permissions and
    # limitations under the License.
    """Google Cloud Speech API sample application using the REST API for async
    batch processing."""
    
    # [START import_libraries]
    import argparse
    import base64
    import json
    import time
    import os
    from googleapiclient import discovery
    import httplib2
    from oauth2client.client import GoogleCredentials
    import datetime
    # [END import_libraries]
    
    
    start = datetime.datetime.now()
    #os.environ["GOOGLE_APPLICATION_CREDENTIALS"]='C:/Users/saurabh.singh/workspace/lenovo/google_speech_api/sox-14-4-2/SpeechRecognition.json'
    #path of audio files that has been uploaded on google drive
    trans_output_path = "".join((base_path,"/","transcripts/"))
    try:
        os.mkdir(trans_output_path)
        print("i am making the transcripts folder as it does not exist")
    except:
        pass    
    
    # Application default credentials provided by env variable
    # GOOGLE_APPLICATION_CREDENTIALS
    def get_speech_service():
        credentials = GoogleCredentials.get_application_default().create_scoped(
            ['https://www.googleapis.com/auth/cloud-platform'])
        http = httplib2.Http()
        credentials.authorize(http)
    
        return discovery.build('speech', 'v1beta1', http=http)
    # [END authenticating]
    
    
    def main(file):
        """Transcribe the given audio file asynchronously.
    
        Args:
            speech_file: the name of the audio file.
        """
        # [START construct_request]
    
        service = get_speech_service()
        service_request = service.speech().asyncrecognize(
            body={
                'config': {
                    # There are a bunch of config options you can specify. See
                    # https://goo.gl/KPZn97 for the full list.
                    'encoding': 'LINEAR16',  # raw 16-bit signed LE samples
                    'sampleRate': 8000,  # 16 khz
                    # See https://goo.gl/A9KJ1A for a list of supported languages.
                    'languageCode': 'en-US',  # a BCP-47 language tag
                },
                'audio': {
                    'uri': file
                    }
                })
        # [END construct_request]
        # [START send_request]
        response = service_request.execute()
        print(json.dumps(response))
        # [END send_request]
    
        name = response['name']
        # Construct a GetOperation request.
        service_request = service.operations().get(name=name)
    
        while True:
            # Give the server a few seconds to process.
            print('Waiting for server processing...')
            time.sleep(1)
            # Get the long running operation with response.
            response = service_request.execute()
    
            if 'done' in response and response['done']:
                break
        try:
            return response['response']['results']
        except:
            return "no results"
    
    # [START run_application]
    gs_name=bucket_name
    file = "".join(("gs://",gs_name,"/",name_of_raw_file))
    csvWriteObj=open("".join((trans_output_path,"transcripts.csv")),"w")
    csvWriteObj.write("FileName|Cuts|Transcripts|confidence\n")
    try:
        trans = main(file)
        if trans != "no results":
            for i,tran in enumerate(trans):
                csvWriteObj.write("%s|%d|%s|%f\n"%(name_of_raw_file,i+1,tran["alternatives"][0]["transcript"],tran["alternatives"][0]["confidence"]))
    except:
        print("transcription process failed")
    csvWriteObj.close()    
            
    end = datetime.datetime.now()
    print("transcripts complete")
    
    name_of_transcript_file = "transcripts.csv"
    
else:
    base_path = sys.argv[1]
    print(base_path)
    name_of_transcript_file = base_path.split("/")[-1]
    print(name_of_transcript_file)
    
    base_path = base_path[:-4] + "/"
    try:
        os.mkdir(base_path)
        print("i created the directory as it did not exist")
    except:
        pass    
    trans_output_path = "/".join(sys.argv[1].split("/")[:-1]) + "/"
    print(trans_output_path)


#Part 4: Formats the transcrips.csv for text mining module input

print("processing the transcript.csv for text mining input")

text_mine_output_path = "".join((base_path,"/","results/"))
try:
    os.mkdir(text_mine_output_path)
    print("i am making the transcripts folder as it does not exist")
except:
    pass    

import pandas as pd
import re
import nltk
import math

input_file_path= "".join((trans_output_path,name_of_transcript_file))


data = pd.read_csv(input_file_path,sep="|") 


grouped_data = data.groupby(['FileName'])

final_data = grouped_data.Transcripts.apply(lambda x: ". ".join([str(i) for i in x]))

#writing files to a csv 

filename = []
index = []
cust_texts = []



for ind,chat in enumerate(final_data):
    print(ind)
    try:
        cust_text = nltk.sent_tokenize(chat)
    except:
        print("current chat failed")
        continue
    for ct in cust_text:
        filename.append(final_data.index[ind])
        index.append(ind)
        cust_texts.append(ct)

result = pd.DataFrame({"filename":filename,"index":index,"cust_text":cust_texts})
result["sentence_sentiment"]="neutral"
result["overall_sentiment"]=""
result["problem_detection"]=0
result["prior_escalation"]=0
result["current_escalation"]=0


data = result

analytical_data = pd.DataFrame()
total_rows =0
unique_index=0
data = result

try:
    l1 = list(data.columns).index("caller")
    caller_name = data.ix[0,"caller"]
    index_fill=[]
    for i in data.index:
        if data.ix[i,"caller"] == caller_name:
            index_fill.extend([i])
        else:
            caller_name = data.ix[i,"caller"]
            data.ix[index_fill,"unique_index"] = unique_index
            unique_index = unique_index + 1 
            pop_value = index_fill.pop(len(index_fill)-1)
            data.ix[index_fill,"overall_sentiment"] = ""
            index_fill=[i]
            try:
                if(math.isnan(float(data.ix[pop_value,"overall_sentiment"]))):
                    print("not relevant")
                    data.ix[pop_value,"overall_sentiment"] = "not relevant"
            except:
                pass
    data.ix[index_fill,"unique_index"] = unique_index
    pop_value = index_fill.pop(len(index_fill)-1)
    data.ix[index_fill,"overall_sentiment"] = ""
    unique_index = unique_index + 1
    try:
        if(math.isnan(float(data.ix[pop_value,"overall_sentiment"]))):
            print("not relevant")
            data.ix[pop_value,"overall_sentiment"] = "not relevant"
    except:
        pass
    data = data[["unique_index","REFERENCE_ID","cust_text","sentence_sentiment","overall_sentiment","problem_detection","prior_escalation","current_escalation"]]

except:
    l1 = list(data.columns).index("index")
    index_n = data.ix[0,"index"]
    index_fill=[]
    for i in data.index:
        if data.ix[i,"index"] == index_n:
            index_fill.extend([i])
        else:
            index_n = data.ix[i,"index"]
            data.ix[index_fill,"unique_index"] = unique_index
            unique_index = unique_index + 1 
            pop_value = index_fill.pop(len(index_fill)-1)
            data.ix[index_fill,"overall_sentiment"] = ""
            index_fill=[i]
            try:
                if(math.isnan(float(data.ix[pop_value,"overall_sentiment"]))):
                    print("not relevant")
                    data.ix[pop_value,"overall_sentiment"] = "not relevant"
            except:
                pass
    data.ix[index_fill,"unique_index"] = unique_index
    pop_value = index_fill.pop(len(index_fill)-1)
    data.ix[index_fill,"overall_sentiment"] = ""
    unique_index = unique_index + 1   
    try:
        if(math.isnan(float(data.ix[pop_value,"overall_sentiment"]))):
            print("not relevant")
            data.ix[pop_value,"overall_sentiment"] = "not relevant"
    except:
        pass
    data = data[["unique_index","filename","cust_text","sentence_sentiment","overall_sentiment","problem_detection","prior_escalation","current_escalation"]]
    
total_rows = total_rows + data.shape[0]
print(data.problem_detection.unique()[0])
if data.problem_detection.unique()[0] == 0 or data.problem_detection.unique()[0] == 1:
    print("i am here")
    analytical_data = analytical_data.append(data)
else:
    print("i came here and failed")
    
    
#cleaning the data
del_from_data = ["&lt;br /&gt;","&amp;#39;", "&amp;nbsp;"," &amp;amp;" ,"&amp;quot", " &amp;" , "&gt;","&lt;"]
for dels in del_from_data:
    analytical_data.cust_text = analytical_data.cust_text.apply(lambda x:str(x).replace(dels,""))


#Part 5: Runs Text Mining Module and saves the result into a local folder
print("starting the text mining processing")

data = analytical_data



#clean text

def clean_text(text):
    text = text.split(" ")
    text = [te[:-2] for te in text]
    text= " ".join(text)
    return(text)
    
#list of output files for transcript results
entitySentimentPath="".join((text_mine_output_path,"entitySentiment.csv"))
escalationPath="".join((text_mine_output_path,"escalation.csv"))
sentimentPath="".join((text_mine_output_path,"sentiment.csv"))
problemDetectionPath="".join((text_mine_output_path,"problemDetection.csv"))
keywordsPath="".join((text_mine_output_path,"keywords.csv"))


#opening the csv object to write the files
entitySentiment=open(entitySentimentPath,"wb")
entitySentiment.write("unique_index|REFERENCE_ID|cust_text|Entity|Phrase|Sentiment\n")



escalation=open(escalationPath,"wb")
escalation.write("unique_index|REFERENCE_ID|cust_text|current_escalation|current_esc_prob|previous_escalation|previous_esc_prob\n")

sentiment=open(sentimentPath,"wb")
sentiment.write("unique_index|REFERENCE_ID|cust_text|sentiment|prob\n")


problemDetection=open(problemDetectionPath,"wb")
problemDetection.write("unique_index|REFERENCE_ID|cust_text|Context|result|prob\n")

keywords = open(keywordsPath,"wb")
keywords.write("unique_index|REFERENCE_ID|cust_text|Keywords|Modifiers\n")


for ind,calls in enumerate(data.cust_text):
    print(ind)
    try:
        unique_index = data.ix[data.index[ind],"unique_index"]
        REFERENCE_ID = data.ix[data.index[ind],"filename"]
        nlp=mt.mtRunServices(calls)
        for items in nlp[0]["entitySentiment"]["result"]:
            entitySentiment.write("%d|%s|%s|%s|%s|%s\n"%(unique_index,REFERENCE_ID,calls,clean_text(items["entity"]),clean_text(items["phrase"]),items["sentiment"]))
        escalation.write("%d|%s|%s|%s|%f|%s|%f\n"%(unique_index,REFERENCE_ID,calls,nlp[0]["escalation"]["result"]["current_escalation"]["result"],nlp[0]["escalation"]["result"]["current_escalation"]["scores"][nlp[0]["escalation"]["result"]["current_escalation"]["result"]],nlp[0]["escalation"]["result"]["previously_escalated"]["result"],nlp[0]["escalation"]["result"]["previously_escalated"]["scores"][nlp[0]["escalation"]["result"]["previously_escalated"]["result"]]))
        sentiment.write("%d|%s|%s|%s|%f\n"%(unique_index,REFERENCE_ID,calls,nlp[0]["sentiment"]["result"],nlp[0]["sentiment"]["scores"][nlp[0]["sentiment"]["result"]]))
        pContexts= nlp[0]["problemDetection"]["context"]
        pContexts.extend(nlp[0]["problemDetection"]["contextDomainNoun"])
        if pContexts.__len__():
            for indp,context in enumerate(pContexts):
                problemDetection.write("%d|%s|%s|%s|%d|%f\n"%(unique_index,REFERENCE_ID,calls,context,nlp[0]["problemDetection"]["result"],nlp[0]["problemDetection"]["scores"][str(nlp[0]["problemDetection"]["result"])]))
        else:
            problemDetection.write("%d|%s|%s|%s|%d|%f\n"%(unique_index,REFERENCE_ID,calls,"",nlp[0]["problemDetection"]["result"],nlp[0]["problemDetection"]["scores"][str(nlp[0]["problemDetection"]["result"])]))
        #getting the modifiers
        res=mt._mtNLPipeline(calls)
        ver=mcwa.findNounrelatedKeyVerbs(res)
        adj=mcwa.findNounIntensifyingAdj(res)
        #cleaning the ver and adj keys:
        ver1={}
        for key in ver.keys():
            key1=key[:-2]
            key1 = key1.replace("_NG_"," ")
            ver1[key1] = ver[key]
    
        adj1={}
        for key in adj.keys():
            key1=key[:-2]
            key1 = key1.replace("_NG_"," ")
            adj1[key1] = adj[key]
    
        for words in nlp[0]["keywords"]["result"]:
            modifiers=[]
            if ver1.has_key(words):
                modifiers.extend(ver1[words])
            if adj1.has_key(words):
                modifiers.extend(adj1[words])
            mod = " ".join(modifiers) 
            keywords.write("%d|%s|%s|%s|%s\n"%(unique_index,REFERENCE_ID,calls,words,clean_text(mod)))
    except:
        continue
    
        
entitySentiment.close()
escalation.close()
sentiment.close()
problemDetection.close()
keywords.close()
        
path_dataset = sentimentPath

data =  pd.read_csv(path_dataset,sep="|",encoding="latin")
data["predicted_overall_sentiment"] = ""

uniq =  data.unique_index.unique()

for ind,index in enumerate(uniq):
    print(ind) 
    subset = data[data.unique_index == index]
    index_for_os = subset.index[subset.index.__len__()-1]
    current_sentence_sentiment = subset.sentiment
    sent_senti_under_consideration = current_sentence_sentiment[-5:]
    overall_sentiment = "neutral"
    if "positive" in sent_senti_under_consideration.value_counts().keys() and sent_senti_under_consideration.value_counts()["positive"] > 3:
        overall_sentiment = "positive"
    elif "negative" in sent_senti_under_consideration.value_counts().keys() and  sent_senti_under_consideration.value_counts()["negative"] > 3:
        overall_sentiment = "negative"
    elif "positive" in sent_senti_under_consideration[-3:].value_counts().keys() and sent_senti_under_consideration[-3:].value_counts()["positive"] == 3:
        overall_sentiment = "positive"
    elif "negative" in sent_senti_under_consideration[-3:].value_counts().keys() and sent_senti_under_consideration[-3:].value_counts()["negative"] == 3:
        overall_sentiment = "negative"
    elif "positive" in sent_senti_under_consideration[-2:].value_counts().keys() and sent_senti_under_consideration[-2:].value_counts()["positive"] == 2 and "neutral" in sent_senti_under_consideration.value_counts().keys() and sent_senti_under_consideration.value_counts()["neutral"] >= 2:
        overall_sentiment = "positive"
    elif "negative" in sent_senti_under_consideration[-2:].value_counts().keys() and sent_senti_under_consideration[-2:].value_counts()["negative"] >= 1:
        overall_sentiment = "negative"
    elif "negative" in sent_senti_under_consideration.value_counts().keys() and "positive" in sent_senti_under_consideration.value_counts().keys() and sent_senti_under_consideration.value_counts()["negative"] >= sent_senti_under_consideration.value_counts()["positive"]:
        overall_sentiment = "negative"
    elif "negative" in sent_senti_under_consideration.value_counts().keys() and "positive" not in sent_senti_under_consideration.value_counts().keys():
        overall_sentiment = "negative"
    elif "positive" in sent_senti_under_consideration.value_counts().keys() and "negative" not in sent_senti_under_consideration.value_counts().keys() and sent_senti_under_consideration.value_counts()["positive"] >= 1:
        overall_sentiment = "positive"
    
    data.ix[index_for_os,"predicted_overall_sentiment"] = overall_sentiment
        
   
data.to_csv("".join((text_mine_output_path,"sentiment_with_os.csv")),encoding = "latin")


print("completed the text mining processing")
    

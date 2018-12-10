import sys
import scipy.io.wavfile
import numpy as np
import time
import imp

#sys.path.append("C:/Users/ramya.ananth/sentimentanalysis/iPython/SDK/OpenVokaturi-1-2/api/")
#import C:/Users/ramya.ananth/sentimentanalysis/iPython/SDK/OpenVokaturi-1-2/api/Vokaturi as V

Vokaturi = imp.load_source('Vokaturi', 'C:/Users/ramya.ananth/sentimentanalysis/iPython/SDK/OpenVokaturi-1-2/api/Vokaturi.py')

print ("Loading library...")
Vokaturi.load("C:/Users/ramya.ananth/sentimentanalysis/iPython/SDK/OpenVokaturi-1-2/lib/Vokaturi_win64.dll")

print ("Reading sound file...")
# file_name = sys.argv[1]
#(sample_rate, samples) = scipy.io.wavfile.read(file_name)


print ("Allocating Vokaturi sample array...")
buffer_length = len(samples)
c_buffer = Vokaturi.SampleArrayC(buffer_length)
c_buffer[:] = samples[:] / 32768

print ("Creating VokaturiVoice...")
voice = Vokaturi.Voice (sample_rate, buffer_length)

print ("Filling VokaturiVoice with samples...")
voice.fill(buffer_length, c_buffer)

print ("Extracting emotions from VokaturiVoice...")
emotionProbabilities = Vokaturi.EmotionProbabilities()
voice.extract(None, None, emotionProbabilities)

#print ("Neutral: %.3f" % emotionProbabilities.neutrality)
#print ("Happy: %.3f" % emotionProbabilities.happiness)
#print ("Sad: %.3f" % emotionProbabilities.sadness)
#print ("Angry: %.3f" % emotionProbabilities.anger)
#print ("Fear: %.3f" % emotionProbabilities.fear)

neutral=emotionProbabilities.neutrality
happy=emotionProbabilities.happiness
sad=emotionProbabilities.sadness
anger=emotionProbabilities.anger
fear=emotionProbabilities.fear

voice.destroy()









	





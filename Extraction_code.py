#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: statistics
"""
import os
from collections import defaultdict

EXTENSIONS = {'.wav', '.TextGrid'}

directory = 'Location of the file'

#arr = os.listdir(directory)
grouped_files = defaultdict(int)

for f in os.listdir(directory):
    name, ext = os.path.splitext(os.path.join(directory, f))
    if ext in EXTENSIONS:
        grouped_files[name] += 1
    
files=list(grouped_files) 

general=[] 
general_max=[]
general_min=[]
general_median=[]      
for i in range(0,len(files)):
    print(files[i])
    s=files[i]
    from praatio import tgio
    tg = tgio.openTextgrid(files[i]+".TextGrid")
#print(tg.tierNameList)
    entryList = tg.tierDict["1"].entryList # Get all intervals ·Name of the order item
    len(entryList)
    wordTier = tg.tierDict['1']
    labelList = [entry[2] for entry in wordTier.entryList]
#print(labelList)
    len(labelList)
    durationList = []
    startList=[]
    stopList=[]
    for start, stop, _ in wordTier.entryList:
        durationList.append(stop - start)
        startList.append(start)
        stopList.append(stop)
#print(durationList)
#print(startList)
#print(stopList)
    len(durationList)

    import parselmouth
    sound = parselmouth.Sound(files[i]+".wav")
    pitch = sound.to_pitch()
    pulses = parselmouth.praat.call([sound, pitch], "To PointProcess (cc)")
    n_pulses = parselmouth.praat.call(pulses, "Get number of points")
    formant = parselmouth.praat.call(sound, "To Formant (burg)", 0.0025, 5, 5500, 0.025, 50)
    
    #Save the recordings
    print(n_pulses)
    mean_pitchList=[]
    max_pitchList=[]
    min_pitchList=[]
    median_pitchList=[]
    f1_List=[]
    f2_List=[]
    f3_List=[]
    f4_List=[]
    f5_List=[]
    f6_List=[]
    f7_List=[]
    f8_List=[]
    f9_List=[]
    f10_List=[]
    f11_List=[]
    f12_List=[]
    f13_List=[]
    f14_List=[]
    f15_List=[]
    f16_List=[]
    for start, stop, _ in wordTier.entryList:
        mean_pitch = parselmouth.praat.call(pitch, "Get mean", start, stop, "Hertz")
        mean_pitchList.append(mean_pitch)
        max_pitch = parselmouth.praat.call(pitch, "Get maximum", start, stop, "Hertz", "Parabolic")
        max_pitchList.append(max_pitch)
        min_pitch = parselmouth.praat.call(pitch, "Get minimum", start, stop, "Hertz", "Parabolic")
        min_pitchList.append(min_pitch)
        median_pitch = parselmouth.praat.call(pitch, "Get quantile", start,stop, 0.5, "Hertz")
        median_pitchList.append(median_pitch)
    
        f1_mean = parselmouth.praat.call(formant, "Get mean",1, start, stop, "Hertz")
        f2_mean = parselmouth.praat.call(formant, "Get mean",2, start, stop, "Hertz")
        f3_mean = parselmouth.praat.call(formant, "Get mean",3, start, stop, "Hertz")
        f4_mean = parselmouth.praat.call(formant, "Get mean",4, start, stop, "Hertz")
        f1_List.append(f1_mean)
        f2_List.append(f2_mean)
        f3_List.append(f3_mean)
        f4_List.append(f4_mean)   

        f1_max = parselmouth.praat.call(formant, "Get maximum", 1,start, stop, "Hertz", "Parabolic")
        f2_max = parselmouth.praat.call(formant, "Get maximum", 2,start, stop, "Hertz", "Parabolic")
        f3_max = parselmouth.praat.call(formant, "Get maximum", 3,start, stop, "Hertz", "Parabolic")
        f4_max = parselmouth.praat.call(formant, "Get maximum", 4,start, stop, "Hertz", "Parabolic")
        f5_List.append(f1_max)
        f6_List.append(f2_max)
        f7_List.append(f3_max)
        f8_List.append(f4_max)
        
        f1_min = parselmouth.praat.call(formant, "Get minimum", 1,start, stop, "Hertz", "Parabolic")
        f2_min = parselmouth.praat.call(formant, "Get minimum", 2,start, stop, "Hertz", "Parabolic")
        f3_min = parselmouth.praat.call(formant, "Get minimum", 3,start, stop, "Hertz", "Parabolic")
        f4_min = parselmouth.praat.call(formant, "Get minimum", 4,start, stop, "Hertz", "Parabolic")
        f9_List.append(f1_min)
        f10_List.append(f2_min)
        f11_List.append(f3_min)
        f12_List.append(f4_min)
        
        f1_median = parselmouth.praat.call(formant, "Get quantile", 1, start, stop, "Hertz", 0.5)
        f2_median = parselmouth.praat.call(formant, "Get quantile", 2, start, stop, "Hertz", 0.5)
        f3_median = parselmouth.praat.call(formant, "Get quantile", 3, start, stop, "Hertz", 0.5)
        f4_median = parselmouth.praat.call(formant, "Get quantile", 4, start, stop, "Hertz", 0.5)
        f13_List.append(f1_median)
        f14_List.append(f2_median)
        f15_List.append(f3_median)
        f16_List.append(f4_median)
        
#print(mean_pitchList)
    len(mean_pitchList)
    names=s.replace(directory+'/', '')
    import pandas as pd 
    df=pd.DataFrame({"ID":names,"Labels":labelList,"Mean_pitch":mean_pitchList,"F1":f1_List,"F2":f2_List,"F3":f3_List,"F4":f4_List,"Duration":durationList})
    print(df)
    df.to_csv(files[i]+"_mean.csv")
    general.append(df)
    
    df1=pd.DataFrame({"ID":names,"Labels":labelList,"Max_pitch":max_pitchList,"F1":f5_List,"F2":f6_List,"F3":f7_List,"F4":f8_List,"Duration":durationList})
    print(df1)
    df1.to_csv(files[i]+"_max.csv")
    general_max.append(df1)
    
    df2=pd.DataFrame({"ID":names,"Labels":labelList,"Min_pitch":min_pitchList,"F1":f9_List,"F2":f10_List,"F3":f11_List,"F4":f12_List,"Duration":durationList})
    print(df2)
    df2.to_csv(files[i]+"_min.csv")
    general_min.append(df2)

    df3=pd.DataFrame({"ID":names,"Labels":labelList,"Median_pitch":median_pitchList,"F1":f13_List,"F2":f14_List,"F3":f15_List,"F4":f16_List,"Duration":durationList})
    print(df3)
    df3.to_csv(files[i]+"_median.csv")
    general_median.append(df3)
    
import pandas as pd
gen=general[0]
gen1=general_max[0]
gen2=general_min[0]
gen3=general_median[0]
for i in range(1,len(general)):
    gen=pd.concat([gen,general[i]])
    gen1=pd.concat([gen1,general_max[i]])
    gen2=pd.concat([gen2,general_min[i]])
    gen3=pd.concat([gen3,general_median[i]])

gen.to_csv(directory+"/General_mean.csv")
gen1.to_csv(directory+"/General_max.csv")
gen2.to_csv(directory+"/General_min.csv")
gen3.to_csv(directory+"/General_median.csv")

import re
import csv
import nltk
import os
from mychildes import CHILDESCorpusReaderX #modified nltk
import shared_code
import enchant
from nltk.corpus import PlaintextCorpusReader
from nltk.tokenize import word_tokenize
from nltk.probability import FreqDist
import operator

shared_code.initialize()

desired_length = 300
outputFile = "ProvidenceFreq500v2.csv"
corpus_dir =  r'C:\Users\Aaron\AppData\Roaming\nltk_data\corpora\childes\Providence'
corpus_name = 'Providence'
BNC_root = r'C:\Users\Aaron\Desktop\BNCBaby\BNCBaby'
ordered_utterance_list = []
child_utterance_list = []
freq_dict = {}
fdist = {}
BNC_top1000 = []

def initialize(): # clean slates the variables
	global ordered_utterance_list
	global child_utterance_list
	ordered_utterance_list = []
	child_utterance_list = []

def read_BNC_baby(root_local):
	global fdist
	wordlists = PlaintextCorpusReader(root_local, '.*', encoding='latin-1')
	BNC_baby = wordlists.words()
	fdist = FreqDist(word.lower() for word in BNC_baby)
	return(fdist)

def sort_fdist():
	global fdist
	global BNC_top1000
	BNC_temp = list(reversed(sorted(fdist.items(), key=operator.itemgetter(1))))[0:999]
	for i in range(0, 999):
		BNC_top1000.append(BNC_temp[i][0])
	return(BNC_top1000)	

def get_childes_files(root_location, file_name): # fetches the childes file in xml and parses it into utterances with speaker in [0] position
	global ordered_utterance_list
	corpus_root = nltk.data.find(root_location) 
	file_setup = CHILDESCorpusReaderX(corpus_root, file_name) 
	ordered_utterance_list = file_setup.sents()
	return(ordered_utterance_list)

def isolate_CHI(list_of_utterances):
	global child_utterance_list
	for utterance in list_of_utterances:
		if utterance[0] == 'CHI':
			utterance = utterance[1:(len(utterance) - 1)]
			child_utterance_list.append(utterance)
	return(child_utterance_list)

def word_filter(cu_list):
	d = enchant.Dict("en_US")
	for item in cu_list:
		for word in item[1:len(item)-1]:
			if d.check(word) == False:
				item.remove(word)
	return(cu_list)
	
def freq_snatcher(CHI_list):
	global freq_dict
	for utterance in CHI_list:
		for word in utterance:
			if word in freq_dict.keys():
				freq_dict[word] += 1
			else:
				freq_dict[word] = 1
	return(freq_dict)			

def get_freq_e(directory_location, input_file_name):
	global ordered_utterance_list
	global child_utterance_list
	global freq_dict
	initialize()
	get_childes_files(directory_location, input_file_name)
	isolate_CHI(ordered_utterance_list)
	freq_snatcher(child_utterance_list)
	return(freq_dict)

def write_freq(output_file_name, freq_d):
	global BNC_top1000
	output_list = []
	d = enchant.Dict("en_US")
	for w in sorted(freq_d, key=freq_d.get, reverse=True):
		try:
			if d.check(w) == True:
				if w in BNC_top1000:
					if len(w) > 1 or w == 'a' or w == 'i':
						output_list.append([w, freq_d[w]])
		except:
			continue			
	with open(output_file_name, "a", newline='') as f:
		magic_writer = csv.writer(f)
		magic_writer.writerows(output_list[0:(desired_length - 1)])
		f.close()	
	
def writeHeader(output_File):
	header = []
	header.insert(0, ["Word", "Frequency"])
	with open(output_File, 'a', newline='') as f:
		writer = csv.writer(f)
		writer.writerows(header)
	f.close()

read_BNC_baby(BNC_root)
sort_fdist()

for dirName, subdirList, fileList in os.walk(corpus_dir):
	for x in subdirList:
		for fname in os.listdir(dirName + '\\' + x):
			if fname.endswith(".xml"):
				os.path.join(dirName + '\\' + x, fname)
				get_freq_e(dirName + '\\' + x, fname)

writeHeader(outputFile)
write_freq(outputFile, freq_dict)				

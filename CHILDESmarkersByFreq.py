import re
import csv
import nltk
import os
from mychildes import CHILDESCorpusReaderX #modified nltk
import shared_code

shared_code.initialize()

desired_length = 500
outputFile = "Providence_freq_500.csv"
corpus_dir =  r'C:\Users\Aaron\AppData\Roaming\nltk_data\corpora\childes\Providence'
corpus_name = 'Providence'
ordered_utterance_list = []
child_utterance_list = []
freq_dict = {}

def initialize(): # clean slates the variables
	global ordered_utterance_list
	global child_utterance_list
	ordered_utterance_list = []
	child_utterance_list = []


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
	output_list = []
	for w in sorted(freq_d, key=freq_d.get, reverse=True):
		output_list.append([w, freq_d[w]])
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

for dirName, subdirList, fileList in os.walk(corpus_dir):
	for x in subdirList:
		for fname in os.listdir(dirName + '\\' + x):
			if fname.endswith(".xml"):
				os.path.join(dirName + '\\' + x, fname)
				get_freq_e(dirName + '\\' + x, fname)

writeHeader(outputFile)
write_freq(outputFile, freq_dict)				

import nltk
import csv
import os
from mychildes import CHILDESCorpusReaderX #modified nltk

Subdirs = True

info_list = []
corpus_dir =  r'C:\Users\Aaron\AppData\Roaming\nltk_data\corpora\childes\Providence'

def initialize():
	global info_list
	info_list = []

def get_child_info(setup_file):
	global info_list
	temp_list = []
	corpus_participants = setup_file.participants(setup_file.fileids())
	for this_corpus_participants in corpus_participants:
    	for key in sorted(this_corpus_participants.keys()):
        	if key == 'CHI':
        		dct = this_corpus_participants[key]
        		for k in sorted(dct.keys()):
        			temp_list.append(dct[k])
        		for item in temp_list:	
        			info_list.append(item)
    info_list = [info_list]    			   		
	return(info_list)	

def get_childes_files(root_location, file_name): # fetches the childes file in xml and parses it into utterances with speaker in [0] position
	global info_list
	corpus_root = nltk.data.find(root_location) 
	file_setup = CHILDESCorpusReaderX(corpus_root, file_name)
	get_child_info(file_setup)
	return(info_list)

def writeHeader(outputFile, writeType):
	header =[] 
	header.insert(0, ["DocId", "Age", "Group", "Id", "Language", 'role', 'Gender'])
	with open(outputFile, writeType, newline='') as f:
		writer = csv.writer(f)
		writer.writerows(header)
	f.close()

def writeFile(outputFile, writeType, file_name, toWrite):
	with open(outputFile, writeType, newline='') as f:
		writer = csv.writer(f)
		for list in toWrite:
			list.insert(0, file_name)
		writer.writerows(toWrite)
	f.close()	

outfile = 'Providence_userInfo.csv'

writeHeader(outfile, 'a')

if Subdirs == True:
	for dirName, subdirList, fileList in os.walk(corpus_dir):
		for x in subdirList:
			for fname in os.listdir(dirName + '\\' + x):
				if fname.endswith(".xml"):
					os.path.join(dirName + '\\' + x, fname)
					initialize()
					get_childes_files(dirName + '\\' + x, fname)
					writeFile(outfile, 'a', fname, info_list)

if Subdirs == False:
	for fname in os.listdir(corpus_dir):
		if fname.endswith(".xml"):
			os.path.join(corpus_dir, fname)
			initialize()
			get_childes_files(corpus_dir, fname)
			writeFile(outfile, 'a', fname, info_list)
				

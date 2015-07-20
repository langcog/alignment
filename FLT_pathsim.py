import nltk
from nltk.corpus import wordnet as wn
import csv
import os
import re
from mychildes import CHILDESCorpusReaderX #modified nltk

speaker_list = []
utterance_dict = {}
squished_dict = {}
convo_dict = {}
convo_counter = 0
squish_counter = 0
word_count_dict = {}
ordered_utterance_list = []
sparsity_measure = {}
output_list = []
output_almost = {}
final_counter = 0
for_output_list = []
possible_conversation_list = []
BLC_list1= []
BLC_dict1 = {}
master_dict = {}
magic_counter = {}

def initialize(): # clean slates the variables
	global speaker_list
	global utterance_dict
	global squished_dict
	global convo_dict
	global convo_counter
	global squish_counter
	global word_count_dict
	global ordered_utterance_list
	global sparsity_measure
	global output_list
	global output_almost
	global final_counter
	global for_output_list
	global possible_conversation_list
	global master_dict
	global magic_counter
	speaker_list = []
	utterance_dict = {}
	squished_dict = {}
	convo_dict = {}
	convo_counter = 0
	squish_counter = 0
	word_count_dict = {}
	ordered_utterance_list = []
	sparsity_measure = {}
	output_list = []
	output_almost = {}
	final_counter = 0
	for_output_list = []
	possible_conversation_list = []
	master_dict = {}
	magic_counter = {}

def get_childes_files(root_location, file_name): # fetches the childes file in xml and parses it into utterances with speaker in [0] position
	global ordered_utterance_list
	corpus_root = nltk.data.find(root_location) 
	file_setup = CHILDESCorpusReaderX(corpus_root, file_name) 
	ordered_utterance_list = file_setup.sents()
	return(ordered_utterance_list)

def determine_speakers(word_list): # gives a list of all speakers in the file
	global speaker_list
	for lists in word_list:
		if lists[0] not in speaker_list:
			speaker_list.append(lists[0])
	return(speaker_list)	

def determine_possible_conversations(list_of_speakers): # determines the list of possible speaker/replier pairs
	global possible_conversation_list
	global sparsity_measure
	for a in list_of_speakers:
		for b in list_of_speakers:
			trill_homie = (a, b)
			if trill_homie not in possible_conversation_list:
				possible_conversation_list.append(trill_homie)
				sparsity_measure[(a, b)] = [0, 0]
	return(possible_conversation_list, sparsity_measure)			

def squisher(some_utterance_list): # squishes utterances with the same speaker
	global utterance_dict
	global squish_counter
	global squished_dict
	temp_list = []
	for x in some_utterance_list:
		temp_list.append(x)
	for i in range(0, len(some_utterance_list) - 1):
		utterance_dict[i] = some_utterance_list[i]
	for i in range(0, (len(utterance_dict) - 1)):
		if temp_list[i][0] == temp_list[i + 1][0]:
			temp_list[i + 1] = temp_list[i] + temp_list[i + 1]
		else:
			squished_dict[squish_counter] = temp_list[i]
			squish_counter += 1
	return(squished_dict)

def convo_grouper(some_dict): # groups utterances into speaker/replier "conversations" 
	global convo_dict
	global convo_counter
	for i in range(0, len(some_dict) - 1, 2):
		convo_dict[convo_counter] = [some_dict[i], some_dict[i + 1]]
		convo_counter += 1
	return(convo_dict)

def calculate_sparsity(list_of_speakers, a_dictionary):
	global sparsity_measure
	for a in list_of_speakers:
		for b in list_of_speakers:
			sparsity_measure[(a, b)] = [0, 0]
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = a_dictionary[x][0][0]
		speaker2 = a_dictionary[x][1][0] 
		sparsity_measure[(speaker1, speaker2)] = [sparsity_measure[(speaker1, speaker2)][0] + len(a_dictionary[x][0]) - len(re.findall(speaker1, str(a_dictionary[x][0]))), sparsity_measure[(speaker1, speaker2)][1] + len(a_dictionary[x][1]) - len(re.findall(speaker2, str(a_dictionary[x][1])))]

def read_BLC_database(file_name, out_list):
	with open(file_name) as new_file:
			for line in new_file:
				out_list.append(line.strip())
	return(out_list)

def transform_BLC1():
	global BLC_list1
	global BLC_dict1
	for item in BLC_list1:
		BLC_dict1[item.split()[0]] = item.split()[1]	
	return(BLC_dict1)		
		
read_BLC_database(r'C:\Python34\Wordnet-3.0\freqmin50\all\BLCnoun.rel', BLC_list1)
transform_BLC1()
			 
def dict_initialize(list_of_speakers):
	for a in list_of_speakers:
		for b in list_of_speakers:
			master_dict[(a, b)] = [{}, {}]
	return(master_dict)

def noun_counter(conversation_dictionary): # calculates number of nouns and total hypernyms those nouns have 
	global master_dict
	global magic_counter	
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = conversation_dictionary[x][0][0]
		speaker2 = conversation_dictionary[x][1][0] 
		y_tokenized = nltk.pos_tag(conversation_dictionary[x][0])
		for i in range(1, len(y_tokenized) - 1):	
			if y_tokenized[i][1] == 'NN':
				if y_tokenized[i][0] not in master_dict[(speaker1, speaker2)][0]:
					magic_counter[(speaker1, speaker2, y_tokenized[i][0], 0)] = 1
					master_dict[(speaker1, speaker2)][0][y_tokenized[i][0]] = [0, 1]
				else:
					magic_counter[(speaker1, speaker2, y_tokenized[i][0], 0)] = magic_counter[(speaker1, speaker2, y_tokenized[i][0], 0)] + 1
					master_dict[(speaker1, speaker2)][0][y_tokenized[i][0]] = [ 0, magic_counter[(speaker1, speaker2, y_tokenized[i][0], 0)]]
		z_tokenized = nltk.pos_tag(conversation_dictionary[x][1])
		for i in range(1, len(z_tokenized) - 1):
			if z_tokenized[i][1] == 'NN':
				if z_tokenized[i][0] not in master_dict[(speaker1, speaker2)][1]:
					magic_counter[(speaker1, speaker2, 0, z_tokenized[i][0])] = 1
					master_dict[(speaker1, speaker2)][1][z_tokenized[i][0]] = [ 0, 1]
				else:
					magic_counter[(speaker1, speaker2, 0, z_tokenized[i][0])] = magic_counter[(speaker1, speaker2, 0, z_tokenized[i][0])] + 1						
					master_dict[(speaker1, speaker2)][1][z_tokenized[i][0]] = [0, magic_counter[(speaker1, speaker2, 0, z_tokenized[i][0])]]
	return(master_dict)	

def transform_ID(ID):
    #Given a Synset ID (e.g. 01234567-n) return a synset
    return wn._synset_from_pos_and_offset(str(ID[-1:]), int(ID[:8]))

def transform_SS(Synset):
	#Given a Synset, returns a synset ID
	return(str(Synset._offset).zfill(8)+'-n')

def get_similarity(conversation_dictionary):
	global master_dict
	global BLC_dict1
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = conversation_dictionary[x][0][0]
		speaker2 = conversation_dictionary[x][1][0]
		for key in master_dict[(speaker1, speaker2)][0].keys():
			try:	
				checked_item = wn.synset(key + '.n.01')
				hyper = lambda s: s.hypernyms()
				BLC_item = transform_ID(BLC_dict1[transform_SS(checked_item)])
				master_dict[(speaker1, speaker2)][0][key][0] = checked_item.path_similarity(BLC_item)
				if BLC_item in list(checked_item.closure(hyper)): #take away if you want to calculate pure pathsim
					master_dict[(speaker1, speaker2)][0][key][0] = master_dict[(speaker1, speaker2)][0][key][0] * -1
			except:
				master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
		for key in master_dict[(speaker1, speaker2)][1].keys():
			try:	
				checked_item = wn.synset(key + '.n.01')
				hyper = lambda s: s.hypernyms()
				BLC_item = transform_ID(BLC_dict1[transform_SS(checked_item)])
				master_dict[(speaker1, speaker2)][1][key][0] = checked_item.path_similarity(BLC_item)
				if BLC_item in list(checked_item.closure(hyper)): #take away if you want to calculate pure pathsim
					master_dict[(speaker1, speaker2)][1][key][0] = master_dict[(speaker1, speaker2)][1][key][0] * -1		
			except:
				master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
	return(master_dict)			

def document_stuff(directory_location, input_file_name, output_file_name, corpus): # writes the final info to a csv file in this order: [DOC ID, speaker, replier, speaker words to replier total, replier words to speaker total, marker, conditional number, speaker marker number, reply marker number, replier utterance number
	global ordered_utterance_list
	global convo_dict
	global sparsity_measure
	global output_almost
	global final_counter
	global alignment_dict
	global possible_conversation_list
	global speaker_list
	global master_dict
	global BLC_dict1
	initialize()
	get_childes_files(directory_location, input_file_name)
	determine_speakers(ordered_utterance_list)
	determine_possible_conversations(speaker_list)
	squisher(ordered_utterance_list)
	convo_grouper(squished_dict)
	calculate_sparsity(speaker_list, convo_dict)
	dict_initialize(speaker_list)
	noun_counter(convo_dict)
	get_similarity(convo_dict)
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = convo_dict[x][0][0]
		speaker2 = convo_dict[x][1][0]
		for key in master_dict[(speaker1, speaker2)][0]:
			output_almost[final_counter] = [corpus, input_file_name, speaker1, speaker2, key, master_dict[(speaker1, speaker2)][0][key][0], master_dict[(speaker1, speaker2)][0][key][1], 'NA', 'NA', sparsity_measure[(speaker1, speaker2)][0], sparsity_measure[(speaker1, speaker2)][1]]	
			final_counter += 1
		for key in master_dict[(speaker1, speaker2)][1]:
			output_almost[final_counter] = [corpus, input_file_name, speaker1, speaker2, key, 'NA', 'NA', master_dict[(speaker1, speaker2)][1][key][0], master_dict[(speaker1, speaker2)][1][key][1], sparsity_measure[(speaker1, speaker2)][0], sparsity_measure[(speaker1, speaker2)][1]]	
			final_counter += 1	
	for y in range(0, (len(output_almost) - 1)):	
		if output_almost[y] not in for_output_list:
			for_output_list.append(output_almost[y])	
	with open(output_file_name, "a", newline='') as f:
		magic_writer = csv.writer(f)
		magic_writer.writerows(for_output_list)
		f.close()		


corpus_dir =  r'C:\Users\Aaron\AppData\Roaming\nltk_data\corpora\childes\Providence'
corpus_name = 'Providence'

def writeHeader(outputFile, writeType):
	header = []
	header.insert(0, ["Corpus", "DocId", "Speaker", "Replier", 'Word', 'S-Distance', 'S-Word Count', "R-Distance", "R-Word Count", "Sparsity S-R", "Sparsity R-S"])
	with open(outputFile, writeType, newline='') as f:
		writer = csv.writer(f)
		writer.writerows(header)
	f.close()

read_BLC_database(r'C:\Python34\Wordnet-3.0\freqmin50\all\BLCnoun.rel', BLC_list1)
transform_BLC1()

writeHeader('Providence_FLT_pathsim.csv', 'a')


for dirName, subdirList, fileList in os.walk(corpus_dir):
	for x in subdirList:
		for fname in os.listdir(dirName + '\\' + x):
			if fname.endswith(".xml"):
				os.path.join(dirName + '\\' + x, fname)
				document_stuff(dirName + '\\' + x, fname, 'Providence_FLT_pathsim.csv', corpus_name)

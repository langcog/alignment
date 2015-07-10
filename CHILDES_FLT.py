import nltk
from nltk.corpus import wordnet as wn
import re
import csv
import os
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
hypernym_dict = {}
hypernym_count = {}
magic_counter = {}
child_age = 'NA'
child_gender = 'NA'

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
	global hypernym_dict
	global hypernym_count
	global magic_counter
	global child_age
	global child_gender
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
	hypernym_count = {}
	hypernym_dict = {}
	magic_counter = {}
	child_age = 'NA'
	child_gender = 'NA'

def get_child_age(setup_file):
	global child_age
	corpus_participants = setup_file.participants(setup_file.fileids())
	for this_corpus_participants in corpus_participants[:2]:
    	for key in sorted(this_corpus_participants.keys()):
        	dct = this_corpus_participants[key]
			if key == 'CHI':
				child_age = dct['age']
	return child_age

def get_childes_files(root_location, file_name): # fetches the childes file in xml and parses it into utterances with speaker in [0] position
	global ordered_utterance_list
	global child_age
	corpus_root = nltk.data.find(root_location) 
	file_setup = CHILDESCorpusReaderX(corpus_root, file_name) 
	ordered_utterance_list = file_setup.sents()
	get_child_age(file_setup)
	return(ordered_utterance_list, child_age)

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

def hypernym_initialize(list_of_speakers): # initializes hypernym counters
	for a in list_of_speakers:
		for b in list_of_speakers:
			hypernym_count[(a, b)] = [0, 0]
			magic_counter[(a, b)] = [0 , 0]
	return(hypernym_count, magic_counter)

def hypernym_calculator(conversation_dictionary): # calculates number of nouns and total hypernyms those nouns have 
	global hypernym_dict
	global magic_counter	
	for a in speaker_list:
		for b in speaker_list:
			hypernym_dict[(a, b)] = [0, 0]
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = convo_dict[x][0][0]
		speaker2 = convo_dict[x][1][0] 
		for y in convo_dict[x][0]:
			y_tokenized = nltk.pos_tag(y)
			for i in range(1, len(y_tokenized) - 1):
				if y_tokenized[i][1] == 'NN':
					checked_item = wn.synset(y_tokenized[i][0] + '.n.01')
					hypernym_count[(speaker1, speaker2)] = [hypernym_count[(speaker1, speaker2)][0] + len(checked_item.hypernyms()), hypernym_count[(speaker1, speaker2)][1]]
					magic_counter[(speaker1, speaker2)][0] += 1
		for z in convo_dict[x][1]:
			z_tokenized = nltk.pos_tag(z)
			for i in range(1, len(z_tokenized) - 1):
				if z_tokenized[i][1] == 'NN':
					checked_item = wn.synset(z_tokenized[i][0] + '.n.01')
					hypernym_count[(speaker1, speaker2)] = [hypernym_count[(speaker1, speaker2)][0], len(checked_item.hypernyms()) + hypernym_count[(speaker1, speaker2)][1]]
					magic_counter[(speaker1, speaker2)][1] += 1				
	return(hypernym_count, magic_counter)		

def hypernym_average_calculator(total_hypernym_count, total_hypernym_counter): # calculates average number of hypernyms per noun for each speaker/replier
	global speaker_list
	global hypernym_dict
	global magic_counter	
	for a in speaker_list:
		for b in speaker_list:
			if magic_counter[(a, b)][0] == 0:
				if magic_counter[(a, b)][1] == 0:
					hypernym_dict[(a, b)] = ['UD', 'UD']
				else:
					hypernym_dict[(a, b)] = ['UD', hypernym_count[(a, b)][1] / magic_counter[(a, b)][1]]	
			elif magic_counter[(a, b)][1] == 0:
				hypernym_dict[(a, b)] = [hypernym_count[(a, b)][0] / magic_counter[(a, b)][0], 'UD']	
			else:
				hypernym_dict[(a, b)] = [hypernym_count[(a, b)][0] / magic_counter[(a, b)][0], hypernym_count[(a, b)][1] / magic_counter[(a, b)][1]]
	return(hypernym_dict)

def calculate_sparsity(list_of_speakers, a_dictionary):
	global sparsity_measure
	for a in list_of_speakers:
		for b in list_of_speakers:
			sparsity_measure[(a, b)] = [0, 0]
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = a_dictionary[x][0][0]
		speaker2 = a_dictionary[x][1][0] 
		sparsity_measure[(speaker1, speaker2)] = [sparsity_measure[(speaker1, speaker2)][0] + len(a_dictionary[x][0]) - len(re.findall(speaker1, str(a_dictionary[x][0]))), sparsity_measure[(speaker1, speaker2)][1] + len(a_dictionary[x][1]) - len(re.findall(speaker2, str(a_dictionary[x][1])))]

def document_stuff(directory_location, input_file_name, output_file_name, corpus): # writes the final info to a csv file in this order: [DOC ID, speaker, replier, speaker words to replier total, replier words to speaker total, marker, conditional number, speaker marker number, reply marker number, replier utterance number
	global ordered_utterance_list
	global convo_dict
	global sparsity_measure
	global output_almost
	global final_counter
	global alignment_dict
	global possible_conversation_list
	global speaker_list
	global hypernym_count
	global magic_counter
	global hypernym_dict
	global child_age
	global child_gender
	initialize()
	get_childes_files(directory_location, input_file_name)
	determine_speakers(ordered_utterance_list)
	determine_possible_conversations(speaker_list)
	squisher(ordered_utterance_list)
	convo_grouper(squished_dict)
	hypernym_initialize(speaker_list)
	hypernym_calculator(convo_dict)
	hypernym_average_calculator(hypernym_count, magic_counter)
	calculate_sparsity(speaker_list, convo_dict)
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = convo_dict[x][0][0]
		speaker2 = convo_dict[x][1][0]
		output_almost[final_counter] = [corpus, input_file_name, speaker1, speaker2, hypernym_dict[(speaker1, speaker2)][0], hypernym_dict[(speaker1, speaker2)][1], sparsity_measure[(speaker1, speaker2)][0], sparsity_measure[(speaker1, speaker2)][1], child_age, child_gender]	
		final_counter += 1
	for y in range(0, (len(output_almost) - 1)):	
		if output_almost[y] not in for_output_list:
			for_output_list.append(output_almost[y])
		alignment_dict = {}		
	with open(output_file_name, "a") as f:
		magic_writer = csv.writer(f)
		magic_writer.writerows(for_output_list)
		f.close()		

corpus_dir =  r'C:\Users\Aaron\AppData\Roaming\nltk_data\corpora\childes\Providence'
corpus_name = 'Providence'

def writeHeader(outputFile, writeType):
	header = []
	header.insert(0, ["Corpus", "DocId", "SpeakerA", "SpeakerB", 'FLT A -> B', "FLT B -> A", "Sparsity A->B", "Sparsity B->A", "Child Age", "Child Gender"])
	with open(outputFile, writeType, newline='') as f:
		writer = csv.writer(f)
		writer.writerows(header)
	f.close()

writeHeader('Providence_FLT_results.csv', 'a')

for dirName, subdirList, fileList in os.walk(corpus_dir):
	for x in subdirList:
		for fname in os.listdir(dirName + '\\' + x):
			if fname.endswith(".xml"):
				os.path.join(dirName + '\\' + x, fname)
				document_stuff(dirName + '\\' + x, fname, 'Providence_FLT_results.csv', corpus_name)

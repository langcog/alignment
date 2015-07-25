import nltk
from nltk.corpus import wordnet as wn
import csv
import os
import re
from mychildes import CHILDESCorpusReaderX #modified nltk
from nltk.corpus import PlaintextCorpusReader
from nltk.probability import FreqDist

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
fdist = {}
pathsim_avg = {}

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
	global pathsim_avg
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
	pathsim_avg = {}

BNC_root = r'C:\Users\Aaron\Desktop\BNCBaby\BNCBaby'

def read_BNC_baby(root_local):
	global fdist
	wordlists = PlaintextCorpusReader(root_local, '.*', encoding='latin-1')
	BNC_baby = wordlists.words()
	fdist = FreqDist(word.lower() for word in BNC_baby)
	return(fdist)	

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
			 
def dict_initialize(list_of_speakers):
	global master_dict
	global pathsim_avg
	for a in list_of_speakers:
		for b in list_of_speakers:
			master_dict[(a, b)] = [{}, {}]
			pathsim_avg[(a, b)] = ['NA', 'NA']
	return(master_dict, pathsim_avg)

def isolate_nouns(conversation_dictionary):
	global master_dict
	global magic_counter	
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = conversation_dictionary[x][0][0]
		speaker2 = conversation_dictionary[x][1][0] 
		y_tokenized = nltk.pos_tag(conversation_dictionary[x][0])
		for i in range(1, len(y_tokenized) - 1):	
			if y_tokenized[i][1] == 'NN':
				if y_tokenized[i][0] not in master_dict[(speaker1, speaker2)][0]:
					master_dict[(speaker1, speaker2)][0][y_tokenized[i][0]] = 'NA'
		z_tokenized = nltk.pos_tag(conversation_dictionary[x][1])
		for i in range(1, len(z_tokenized) - 1):
			if z_tokenized[i][1] == 'NN':
				if z_tokenized[i][0] not in master_dict[(speaker1, speaker2)][1]:
					master_dict[(speaker1, speaker2)][1][z_tokenized[i][0]] = 'NA'
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
					master_dict[(speaker1, speaker2)][0][y_tokenized[i][0]] = [0, magic_counter[(speaker1, speaker2, y_tokenized[i][0], 0)]]
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

def get_similarity_full_range(conversation_dictionary):
	global master_dict
	global fdist
	temp_list = []
	hyper = lambda s: s.hypernyms()
	hypo = lambda s: s.hyponyms()
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = conversation_dictionary[x][0][0]
		speaker2 = conversation_dictionary[x][1][0]
		for key in master_dict[(speaker1, speaker2)][0].keys():
			try:	
				temp_list = []
				biggest_amount = 0
				biggest_word = 'NA'
				checked_item = wn.synset(key + '.n.01')
				temp_list.append(key)
				for item in list(checked_item.closure(hyper)):
					temp_list.append(item.lemmas()[0].name())
				for item in list(checked_item.closure(hypo)):
					temp_list.append(item.lemmas()[0].name())
				for word in temp_list:
					try:
						if fdist[word] > biggest_amount:
							biggest_amount = fdist[word]
							biggest_word = word
					except:
						biggest_word = biggest_word		
				master_dict[(speaker1, speaker2)][0][key] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))
				if wn.synset(biggest_word + '.n.01') in list(checked_item.closure(hyper)):
					master_dict[(speaker1, speaker2)][0][key] = master_dict[(speaker1, speaker2)][0][key] * -1
			except:
				master_dict[(speaker1, speaker2)][0][key] = 'NA'
			temp_list = []	
		for key in master_dict[(speaker1, speaker2)][1].keys():
			try:	
				temp_list = []
				biggest_amount = 0
				biggest_word = 'NA'
				checked_item = wn.synset(key + '.n.01')
				temp_list.append(key)
				for item in list(checked_item.closure(hyper)):
					temp_list.append(item.lemmas()[0].name())
				for item in list(checked_item.closure(hypo)):
					temp_list.append(item.lemmas()[0].name())
				for word in temp_list:
					try:	
						if fdist[word] > biggest_amount:
							biggest_amount = fdist[word]
							biggest_word = word
					except:
						biggest_word = biggest_word		
				master_dict[(speaker1, speaker2)][1][key] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))
				if wn.synset(biggest_word + '.n.01') in list(checked_item.closure(hyper)):
					master_dict[(speaker1, speaker2)][1][key] = master_dict[(speaker1, speaker2)][1][key] * -1			
			except:
				master_dict[(speaker1, speaker2)][1][key] = 'NA'	
	return(master_dict)		

def get_similarity(conversation_dictionary):
	global master_dict
	global fdist
	temp_list = []
	hyper = lambda s: s.hypernyms()
	hypo = lambda s: s.hyponyms()
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = conversation_dictionary[x][0][0]
		speaker2 = conversation_dictionary[x][1][0]
		for key in master_dict[(speaker1, speaker2)][0].keys():
			try:	
				temp_list = []
				biggest_amount = 0
				biggest_word = 'NA'
				checked_item = wn.synset(key + '.n.01')
				temp_list.append(key)
				for item in list(checked_item.closure(hyper)):
					temp_list.append(item.lemmas()[0].name())
				for item in list(checked_item.closure(hypo)):
					temp_list.append(item.lemmas()[0].name())
				for word in temp_list:
					try:
						if fdist[word] > biggest_amount:
							biggest_amount = fdist[word]
							biggest_word = word
					except:
						biggest_word = biggest_word		
				master_dict[(speaker1, speaker2)][0][key] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))
			except:
				master_dict[(speaker1, speaker2)][0][key] = 'NA'
			temp_list = []	
		for key in master_dict[(speaker1, speaker2)][1].keys():
			try:	
				temp_list = []
				biggest_amount = 0
				biggest_word = 'NA'
				checked_item = wn.synset(key + '.n.01')
				temp_list.append(key)
				for item in checked_item.closure(hyper):
					temp_list.append(item.lemmas()[0].name())
				for item in checked_item.closure(hypo):
					temp_list.append(item.lemmas()[0].name())
				for word in temp_list:
					try:
						if fdist[word] > biggest_amount:
							biggest_amount = fdist[word]
							biggest_word = word
					except:
						biggest_word = biggest_word		
				master_dict[(speaker1, speaker2)][1][key] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))			
			except:
				master_dict[(speaker1, speaker2)][1][key] = 'NA'	
	return(master_dict)

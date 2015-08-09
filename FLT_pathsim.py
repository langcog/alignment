import nltk
from nltk.corpus import wordnet as wn
import csv
import os
import re
from mychildes import CHILDESCorpusReaderX #modified nltk
from nltk.corpus import PlaintextCorpusReader
from nltk.probability import FreqDist
import enchant
from nltk.corpus import brown
import json
from operator import itemgetter

hit_list = ['artifact', 'entity', 'object', 'event', 'quality', 'relation', 'evidence', 'activity', 'action', 'attitdue', 'group', 'condition', 'difference', 'matter', 'part']


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
Subdirs = True
perm_dict = {}
ref_dict = {}

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

COCA_root = r'C:\Users\Aaron\alignment\COCA-ngrams\coca-1grams.json'

def get_Freq_COCA(root):
	global fdist
	global ref_dict
	temp_listn = []
	temp_listv = []
	temp_lista = []
	with open(root) as data_file:
	        v = json.load(data_file)
	        v = sorted(v.items(), key=itemgetter(1), reverse=True)
	        v = [(tuple(word.rsplit('_', 2)), count) for word, count in v]	
	for tp in v:
		if tp[0][2].startswith('n') and tp[1] > 1:
			temp_listn.append((tp[0][0], tp[0][1], tp[1]))
		elif tp[0][2].startswith('v') and tp[1] > 1:
			temp_listv.append((tp[0][0], tp[0][1], tp[1]))
		elif tp[0][2].startswith('j') and tp[1] > 1:
			temp_lista.append((tp[0][0], tp[0][1], tp[1]))	
	for tpl in temp_listn:
		if tpl[1] not in fdist.keys():
			fdist[tpl[1]] = tpl[2]
		elif tpl[1] in fdist.keys():
			fdist[tpl[1]] += tpl[2]
	for item in temp_listn:
		if (item[1], 'n') not in ref_dict.keys():
			ref_dict[(item[1], 'n')] = item[2]
		elif (item[1], 'n') in ref_dict.keys():
			ref_dict[(item[1], 'n')] += item[2]
	for item in temp_listv:
		if (item[1], 'v') not in ref_dict.keys():
			ref_dict[(item[1], 'v')] = item[2]
		elif (item[1], 'v') in ref_dict.keys():
			ref_dict[(item[1], 'v')] += item[2]			
	for item in temp_lista:
		if (item[1], 'a') not in ref_dict.keys():
			ref_dict[(item[1], 'a')] = item[2]
		elif (item[1], 'a') in ref_dict.keys():
			ref_dict[(item[1], 'a')] += item[2]		
	return(ref_dict, fdist)		

def get_Freq_Brown():
	global fdist
	temp_brown = brown.tagged_words()
	fdist_temp = FreqDist(temp_brown)
	for key in fdist_temp.keys():
		if key[1] == 'NN' or key[1] == 'NNS':
			fdist[key[0]] = fdist_temp[key]
	return(fdist)

def read_BNC_baby(root_local):
	global fdist
	wordlists = PlaintextCorpusReader(root_local, '.*', encoding='latin-1')
	BNC_baby = wordlists.words()
	fdist = FreqDist(word.lower() for word in BNC_baby)
	return(fdist)

def read_Freq_File(file_name):
	global fdist
	global ref_dict
	with open(file_name, 'r') as in_file:
		for line in in_file:
			split_line = line.split()
			ref_dict[(split_line[2], split_line[3])] = int(split_line[1])
			if split_line[3] == 'n':
				fdist[split_line[2]] = int(split_line[1])
	return(fdist, ref_dict)					

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
					master_dict[(speaker1, speaker2)][0][y_tokenized[i][0]] = [0, 0, 'NA']
		z_tokenized = nltk.pos_tag(conversation_dictionary[x][1])
		for i in range(1, len(z_tokenized) - 1):
			if z_tokenized[i][1] == 'NN':
				if z_tokenized[i][0] not in master_dict[(speaker1, speaker2)][1]:
					master_dict[(speaker1, speaker2)][1][z_tokenized[i][0]] = [0, 0, 'NA']
	return(master_dict)	


def noun_counter(conversation_dictionary): # calculates number of nouns and total hypernyms those nouns have 
	d = enchant.Dict("en_US")
	global master_dict
	global magic_counter	
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = conversation_dictionary[x][0][0]
		speaker2 = conversation_dictionary[x][1][0] 
		y_tokenized = nltk.pos_tag(conversation_dictionary[x][0])
		for i in range(1, len(y_tokenized) - 1):
			try:
				if len(y_tokenized[i][0]) > 1:	
					if d.check(y_tokenized[i][0]) == True:
						if y_tokenized[i][1] == 'NN':
							trill_homie = wn.synset(y_tokenized[i][0] + '.n.01').lemmas()[0].name()
							if trill_homie not in master_dict[(speaker1, speaker2)][0]:
								magic_counter[(speaker1, speaker2, trill_homie, 0)] = 1
								master_dict[(speaker1, speaker2)][0][trill_homie] = [0, 1, 'NA']
							else:
								magic_counter[(speaker1, speaker2, trill_homie, 0)] = magic_counter[(speaker1, speaker2, trill_homie, 0)] + 1
								master_dict[(speaker1, speaker2)][0][trill_homie] = [0, magic_counter[(speaker1, speaker2, trill_homie, 0)], 'NA']
			except:
				continue					
		z_tokenized = nltk.pos_tag(conversation_dictionary[x][1])
		for i in range(1, len(z_tokenized) - 1):
			try:
				if len(z_tokenized[i][0]) > 1:
					if d.check(z_tokenized[i][0]) == True:
						if z_tokenized[i][1] == 'NN':
							trill_homie = wn.synset(z_tokenized[i][0] + '.n.01').lemmas()[0].name()
							if z_tokenized[i][0] not in master_dict[(speaker1, speaker2)][1]:
								magic_counter[(speaker1, speaker2, 0, trill_homie)] = 1
								master_dict[(speaker1, speaker2)][1][trill_homie] = [0, 1, 'NA']
							else:
								magic_counter[(speaker1, speaker2, 0, trill_homie)] = magic_counter[(speaker1, speaker2, 0, trill_homie)] + 1						
								master_dict[(speaker1, speaker2)][1][trill_homie] = [0, magic_counter[(speaker1, speaker2, 0, trill_homie)], 'NA']
			except:
				continue
	return(master_dict)	

def get_similarity_full_range(conversation_dictionary):
	global master_dict
	global fdist
	global perm_dict
	global ref_dict
	temp_list = []
	hyper = lambda s: s.hypernyms()
	hypo = lambda s: s.hyponyms()
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = conversation_dictionary[x][0][0]
		speaker2 = conversation_dictionary[x][1][0]
		for key in master_dict[(speaker1, speaker2)][0].keys():
			if (key, 'n') not in ref_dict.keys():
				master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
				master_dict[(speaker1, speaker2)][0][key][2] = 'NA'	
				continue
			elif (key, 'v') in ref_dict.keys():
				if (ref_dict[(key, 'v')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][0][key][2] = 'NA'
					continue
			elif (key, 'a') in ref_dict.keys():
				if (ref_dict[(key, 'a')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][0][key][2] = 'NA'
					continue
			elif (key, 'adv') in ref_dict.keys():
				if (ref_dict[(key, 'adv')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][0][key][2] = 'NA'
					continue		
							
			try:	
				if key in perm_dict.keys():
					master_dict[(speaker1, speaker2)][0][key][0] = perm_dict[key][1]
					master_dict[(speaker1, speaker2)][0][key][2] = perm_dict[key][0]
				else:	
					temp_list = []
					perm_dict[key] = ['NA', 'NA']
					biggest_amount = 0
					biggest_word = '$$$'
					checked_item = wn.synset(key + '.n.01')
					ho = list(checked_item.closure(hypo))
					totes = len(list(ho[-1].closure(hyper)))
					if totes < 7:
						master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
						master_dict[(speaker1, speaker2)][0][key][2] = 'NA'
						continue
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
							continue		
					master_dict[(speaker1, speaker2)][0][key][0] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))
					master_dict[(speaker1, speaker2)][0][key][2] = biggest_word
					perm_dict[key][0] = biggest_word
					perm_dict[key][1] = master_dict[(speaker1, speaker2)][0][key][0]
					if wn.synset(biggest_word + '.n.01') in list(checked_item.closure(hyper)):
						master_dict[(speaker1, speaker2)][0][key][0] = master_dict[(speaker1, speaker2)][0][key][0] * -1
						perm_dict[key][1] = perm_dict[key][1] * -1
			except:
				master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
			temp_list = []	
		for key in master_dict[(speaker1, speaker2)][1].keys():
			if (key, 'n') not in ref_dict.keys():
				master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
				master_dict[(speaker1, speaker2)][1][key][2] = 'NA'	
				continue
			elif (key, 'v') in ref_dict.keys():
				if (ref_dict[(key, 'v')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][1][key][2] = 'NA'
					continue
			elif (key, 'a') in ref_dict.keys():
				if (ref_dict[(key, 'a')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][1][key][2] = 'NA'
					continue
			elif (key, 'adv') in ref_dict.keys():
				if (ref_dict[(key, 'adv')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][1][key][2] = 'NA'
					continue	
			try:
				if key in perm_dict.keys():
					master_dict[(speaker1, speaker2)][1][key][0] = perm_dict[key][1]
					master_dict[(speaker1, speaker2)][1][key][2] = perm_dict[key][0]
				else:	
					temp_list = []
					biggest_amount = 0
					perm_dict[key] = ['NA', 'NA']
					biggest_word = '$$$'
					checked_item = wn.synset(key + '.n.01')
					ho = list(checked_item.closure(hypo))
					totes = len(list(ho[-1].closure(hyper)))
					if totes < 7:
						master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
						master_dict[(speaker1, speaker2)][1][key][2] = 'NA'
						continue
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
							continue		
					master_dict[(speaker1, speaker2)][1][key][0] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))
					master_dict[(speaker1, speaker2)][1][key][2] = biggest_word
					perm_dict[key][0] = biggest_word
					perm_dict[key][1] = master_dict[(speaker1, speaker2)][1][key][0]
					if wn.synset(biggest_word + '.n.01') in list(checked_item.closure(hyper)):
						master_dict[(speaker1, speaker2)][1][key][0] = master_dict[(speaker1, speaker2)][1][key][0] * -1
						perm_dict[key][1] = perm_dict[key][1] * -1			
			except:
				master_dict[(speaker1, speaker2)][1][key][0] = 'NA'	
	return(master_dict, perm_dict)		

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
				biggest_word = '$$$'
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
				master_dict[(speaker1, speaker2)][0][key][0] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))
				master_dict[(speaker1, speaker2)][0][key][2] = biggest_word
			except:
				master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
			temp_list = []	
		for key in master_dict[(speaker1, speaker2)][1].keys():
			try:	
				temp_list = []
				biggest_amount = 0
				biggest_word = '$$$'
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
				master_dict[(speaker1, speaker2)][1][key][0] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))
				master_dict[(speaker1, speaker2)][1][key][2] = biggest_word
			except:
				master_dict[(speaker1, speaker2)][1][key][0] = 'NA'	
	return(master_dict)

def get_similarity_full_local(conversation_dictionary):
	global master_dict
	global fdist
	global perm_dict
	global ref_dict
	temp_list = []
	hyper = lambda s: s.hypernyms()
	hypo = lambda s: s.hyponyms()
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = conversation_dictionary[x][0][0]
		speaker2 = conversation_dictionary[x][1][0]
		for key in master_dict[(speaker1, speaker2)][0].keys():
			if (key, 'n') not in ref_dict.keys():
				master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
				master_dict[(speaker1, speaker2)][0][key][2] = 'NA'	
				continue
			elif (key, 'v') in ref_dict.keys():
				if (ref_dict[(key, 'v')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][0][key][2] = 'NA'
					continue
			elif (key, 'a') in ref_dict.keys():
				if (ref_dict[(key, 'a')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][0][key][2] = 'NA'
					continue
			elif (key, 'adv') in ref_dict.keys():
				if (ref_dict[(key, 'adv')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][0][key][2] = 'NA'
					continue	
			if key in perm_dict.keys():
				master_dict[(speaker1, speaker2)][0][key][0] = perm_dict[key][1]
				master_dict[(speaker1, speaker2)][0][key][2] = perm_dict[key][0]
			else:		
				try:	
					temp_list = []
					biggest_amount = 0
					biggest_word = '$$$'
					perm_dict[key] = ['NA', 'NA']
					checked_item = wn.synset(key + '.n.01')
					ho = list(checked_item.closure(hypo))
					totes = len(list(ho[-1].closure(hyper)))
					if totes < 7:
						master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
						master_dict[(speaker1, speaker2)][0][key][2] = 'NA'
						continue
					temp_list.append(key)
					for item in list(checked_item.hypernyms()):
						if len(re.findall('.n.01', str(item))) == 1:
							temp_list.append(item.lemmas()[0].name())	
						for word in list(item.hypernyms()):
							if word.lemmas()[0].name() not in temp_list:
								if len(re.findall('.n.01', str(word))) == 1:
									temp_list.append(word.lemmas()[0].name())
							for sub_word in list(word.hypernyms()):
								if sub_word.lemmas()[0].name() not in temp_list:
									if len(re.findall('.n.01', str(sub_word))) == 1:
										temp_list.append(sub_word.lemmas()[0].name())
							
					for item in list(checked_item.hyponyms()):
						if len(re.findall('.n.01', str(item))) == 1:
							temp_list.append(item.lemmas()[0].name())	
						for word in list(item.hyponyms()):
							if word.lemmas()[0].name() not in temp_list:
								if len(re.findall('.n.01', str(word))) == 1:
									temp_list.append(word.lemmas()[0].name())
							for sub_word in list(word.hyponyms()):
								if sub_word.lemmas()[0].name() not in temp_list:
									if len(re.findall('.n.01', str(sub_word))) == 1:
										temp_list.append(sub_word.lemmas()[0].name())
			
					for word in temp_list:
						try:
							if fdist[word] > biggest_amount:
								if word not in hit_list:
									biggest_amount = fdist[word]
									biggest_word = word
						except:
							continue		
					master_dict[(speaker1, speaker2)][0][key][0] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))
					master_dict[(speaker1, speaker2)][0][key][2] = biggest_word
					perm_dict[key][0] = biggest_word
					perm_dict[key][1] = master_dict[(speaker1, speaker2)][0][key][0]
					if wn.synset(biggest_word + '.n.01') in list(checked_item.closure(hyper)):
						master_dict[(speaker1, speaker2)][0][key][0] = master_dict[(speaker1, speaker2)][0][key][0] * -1
						perm_dict[key][1] = perm_dict[key][1] * -1
				except:
					master_dict[(speaker1, speaker2)][0][key][0] = 'NA'
			temp_list = []	
		for key in master_dict[(speaker1, speaker2)][1].keys():
			if (key, 'n') not in ref_dict.keys():
				master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
				master_dict[(speaker1, speaker2)][1][key][2] = 'NA'	
				continue
			if (key, 'v') in ref_dict.keys():
				if (ref_dict[(key, 'v')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][1][key][2] = 'NA'
					continue
			elif (key, 'a') in ref_dict.keys():
				if (ref_dict[(key, 'a')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][1][key][2] = 'NA'
					continue
			elif (key, 'adv') in ref_dict.keys():
				if (ref_dict[(key, 'adv')] > ref_dict[(key, 'n')]):
					master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
					master_dict[(speaker1, speaker2)][1][key][2] = 'NA'
					continue	
			
			if key in perm_dict.keys():
				master_dict[(speaker1, speaker2)][1][key][0] = perm_dict[key][1]
				master_dict[(speaker1, speaker2)][1][key][2] = perm_dict[key][0]
		
			else:		
				try:	
					temp_list = []
					biggest_amount = 0
					biggest_word = '$$$'
					perm_dict[key] = ['NA', 'NA']
					checked_item = wn.synset(key + '.n.01')
					ho = list(checked_item.closure(hypo))
					totes = len(list(ho[-1].closure(hyper)))
					if totes < 7:
						master_dict[(speaker1, speaker2)][1][key][0] = 'NA'
						master_dict[(speaker1, speaker2)][1][key][2] = 'NA'
						continue
					temp_list.append(key)
					for item in list(checked_item.hypernyms()):
						if len(re.findall('.n.01', str(item))) == 1:
							temp_list.append(item.lemmas()[0].name())	
						for word in list(item.hypernyms()):
							if word.lemmas()[0].name() not in temp_list:
								if len(re.findall('.n.01', str(word))) == 1:
									temp_list.append(word.lemmas()[0].name())
							for sub_word in list(word.hypernyms()):
								if sub_word.lemmas()[0].name() not in temp_list:
									if len(re.findall('.n.01', str(sub_word))) == 1:
										temp_list.append(sub_word.lemmas()[0].name())
							
					for item in list(checked_item.hyponyms()):
						if len(re.findall('.n.01', str(item))) == 1:
							temp_list.append(item.lemmas()[0].name())	
						for word in list(item.hyponyms()):
							if word.lemmas()[0].name() not in temp_list:
								if len(re.findall('.n.01', str(word))) == 1:
									temp_list.append(word.lemmas()[0].name())
							for sub_word in list(word.hyponyms()):
								if sub_word.lemmas()[0].name() not in temp_list:
									if len(re.findall('.n.01', str(sub_word))) == 1:
										temp_list.append(sub_word.lemmas()[0].name())
					for word in temp_list:
						try:	
							if fdist[word] > biggest_amount:
								if word not in hit_list:
									biggest_amount = fdist[word]
									biggest_word = word
						except:
							continue		
					master_dict[(speaker1, speaker2)][1][key][0] = checked_item.path_similarity(wn.synset(biggest_word + '.n.01'))
					master_dict[(speaker1, speaker2)][1][key][2] = biggest_word
					perm_dict[key][0] = biggest_word
					perm_dict[key][1] = master_dict[(speaker1, speaker2)][1][key][0]
					if wn.synset(biggest_word + '.n.01') in list(checked_item.closure(hyper)):
						master_dict[(speaker1, speaker2)][1][key][0] = master_dict[(speaker1, speaker2)][1][key][0] * -1
						perm_dict[key][1] = perm_dict[key][1] * -1			
				except:
					master_dict[(speaker1, speaker2)][1][key][0] = 'NA'	
	return(master_dict, perm_dict)			

def get_hyp_avg(conversation_dictionary):
	global master_dict
	global convo_dict
	global pathsim_avg
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = conversation_dictionary[x][0][0]
		speaker2 = conversation_dictionary[x][1][0]
		trill_ct = 0
		trill_total = 0
		for key in master_dict[(speaker1, speaker2)][0].keys():
			if master_dict[(speaker1, speaker2)][0][key][0] != 'NA':
				trill_total = trill_total + master_dict[(speaker1, speaker2)][0][key][0]
				trill_ct += 1
		if trill_ct > 0:
			pathsim_avg[(speaker1, speaker2)][0] = trill_total / trill_ct
		else:
			pathsim_avg[(speaker1, speaker2)][0] = 'NA'
		trill_ct = 0
		trill_total = 0
		for key in master_dict[(speaker1, speaker2)][1].keys():			
			if master_dict[(speaker1, speaker2)][1][key][0] != 'NA':
				trill_total = trill_total + master_dict[(speaker1, speaker2)][1][key][0]
				trill_ct += 1
		if trill_ct > 0:
			pathsim_avg[(speaker1, speaker2)][1] = trill_total / trill_ct
		else:
			pathsim_avg[(speaker1, speaker2)][1] = 'NA'
	return(pathsim_avg)		

def document_stuff(directory_location, input_file_name, output_file_name): # writes the final info to a csv file in this order: [DOC ID, speaker, replier, speaker words to replier total, replier words to speaker total, marker, conditional number, speaker marker number, reply marker number, replier utterance number
	global ordered_utterance_list
	global convo_dict
	global sparsity_measure
	global output_almost
	global final_counter
	global alignment_dict
	global possible_conversation_list
	global speaker_list
	global master_dict
	global fdist
	global pathsim_avg
	global perm_dict
	global ref_dict
	initialize()
	get_childes_files(directory_location, input_file_name)
	determine_speakers(ordered_utterance_list)
	determine_possible_conversations(speaker_list)
	squisher(ordered_utterance_list)
	convo_grouper(squished_dict)
	calculate_sparsity(speaker_list, convo_dict)
	dict_initialize(speaker_list)
	noun_counter(convo_dict)
	get_similarity_full_local(convo_dict)
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = convo_dict[x][0][0]
		speaker2 = convo_dict[x][1][0]
		for key in master_dict[(speaker1, speaker2)][0].keys():
			output_almost[final_counter] = [input_file_name, speaker1, speaker2, key, master_dict[speaker1, speaker2][0][key][2], master_dict[speaker1, speaker2][0][key][0], master_dict[speaker1, speaker2][0][key][1], 'NA', 'NA', 'NA', 'NA', sparsity_measure[(speaker1, speaker2)][0], sparsity_measure[(speaker1, speaker2)][1]]	
			final_counter += 1
		for key in master_dict[(speaker1, speaker2)][1].keys():
			output_almost[final_counter] = [input_file_name, speaker1, speaker2, 'NA', 'NA', 'NA', 'NA', key, master_dict[speaker1, speaker2][1][key][2], master_dict[speaker1, speaker2][1][key][0], master_dict[speaker1, speaker2][1][key][1], sparsity_measure[(speaker1, speaker2)][0], sparsity_measure[(speaker1, speaker2)][1]]	
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
	header.insert(0, ["DocId", "Speaker", "Replier", 'S Word', 'S BLC', 'S Path Similarity', 'S Frequency', 'R word', 'R BLC', 'R Path Similarity', 'R Frequency', "Sparsity S-R", "Sparsity R-S"])
	with open(outputFile, writeType, newline='') as f:
		writer = csv.writer(f)
		writer.writerows(header)
	f.close()

outfile = 'ProvidenceFLTPathsimLocalCOCA.csv'

freq_list_location = r'C:\Users\Aaron\alignment\lemma.num'

get_Freq_COCA(COCA_root)
writeHeader(outfile, 'a')

if Subdirs == True:
	for dirName, subdirList, fileList in os.walk(corpus_dir):
		for x in subdirList:
			for fname in os.listdir(dirName + '\\' + x):
				if fname.endswith(".xml"):
					os.path.join(dirName + '\\' + x, fname)
					document_stuff(dirName + '\\' + x, fname, outfile)
if Subdirs == False:
	for fname in os.listdir(corpus_dir):
			if fname.endswith(".xml"):
				os.path.join(corpus_dir, fname)
				document_stuff(corpus_dir, fname, outfile)

				

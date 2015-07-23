import re
import csv
import nltk
import os
from mychildes import CHILDESCorpusReaderX #modified nltk
import shared_code
import logger

logger.initialize()

corpus = 'Providence'
smoothing_values = [0, 1]
outputFile = "ProvFreq300Results.csv"
markersFile = "PF300.csv"
corpus_dir =  r'C:\Users\Aaron\AppData\Roaming\nltk_data\corpora\childes\Providence'
corpus_name = 'Providence'
marker_list = shared_code.readMarkers(markersFile)
speaker_list = []
utterance_dict = {}
squished_dict = {}
convo_dict = {}
convo_counter = 0
squish_counter = 0
total_utterance_reply_dict = {}
total_marker_speaker_dict = {}
total_marker_reply_dict = {}
conditional_conversation_dict = {}
alignment_dict = {}
word_count_dict = {}
squished_dict = {}
ordered_utterance_list = []
sparsity_measure = {}
output_list = []
output_almost = {}
final_counter = 0
for_output_list = []
possible_conversation_list = []
project_x = []
super_var = True

def initialize(): # clean slates the variables
	global speaker_list
	global utterance_dict
	global squished_dict
	global convo_dict
	global convo_counter
	global squish_counter
	global total_utterance_reply_dict
	global total_marker_speaker_dict
	global total_marker_reply_dict
	global conditional_conversation_dict
	global alignment_dict
	global word_count_dict
	global ordered_utterance_list
	global sparsity_measure
	global output_list
	global output_almost
	global final_counter
	global for_output_list
	global possible_conversation_list
	global project_x
	speaker_list = []
	utterance_dict = {}
	squished_dict = {}
	convo_dict = {}
	convo_counter = 0
	squish_counter = 0
	total_utterance_reply_dict = {}
	total_marker_speaker_dict = {}
	total_marker_reply_dict = {}
	conditional_conversation_dict = {}
	alignment_dict = {}
	word_count_dict = {}
	ordered_utterance_list = []
	sparsity_measure = {}
	output_list = []
	output_almost = {}
	final_counter = 0
	for_output_list = []
	possible_conversation_list = []
	project_x = []

def get_childes_files(root_location, file_name): # fetches the childes file in xml and parses it into utterances with speaker in [0] position
	global ordered_utterance_list
	global child_age
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
	global total_utterance_reply_dict
	global total_marker_speaker_dict
	global total_marker_reply_dict
	global conditional_conversation_dict
	global alignment_dict
	global sparsity_measure
	for a in list_of_speakers:
		for b in list_of_speakers:
			if (a, b) not in possible_conversation_list:
				possible_conversation_list.append((a, b))
				conditional_conversation_dict[(a, b)] = 0
				total_marker_speaker_dict[(a, b)] = 0
				total_marker_reply_dict[(a, b)] = 0
				total_utterance_reply_dict[(a, b)] = 0
				alignment_dict[(a, b)] = 0
				sparsity_measure[(a, b)] = [0, 0]
	return(possible_conversation_list, conditional_conversation_dict, total_marker_speaker_dict, total_utterance_reply_dict, total_utterance_reply_dict, alignment_dict, sparsity_measure)			

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
		
def convo_converter(corpusname, filename, conversation_dictionary, marker_list):
	global project_x
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = convo_dict[x][0][0]
		speaker2 = convo_dict[x][1][0]

		toAppend = ({'corpus': corpusname, 'docId': filename, 'convId': (speaker1, speaker2), 'msgUserId': speaker1, 'msg': convo_dict[x][0], 'replyUserId': speaker2, 'reply': convo_dict[x][1], 'msgMarkers': [], 'replyMarkers': [], 'msgTokens': convo_dict[x][0], 'replyTokens': convo_dict[x][1]})
		for marker in marker_list:
			if marker["marker"] in convo_dict[x][0]:
				toAppend["msgMarkers"].append(marker["marker"])
			if marker["marker"] in convo_dict[x][1]:
				toAppend["replyMarkers"].append(marker["marker"])
		project_x.append(toAppend)
	return project_x
	
def calculate_sparsity(list_of_speakers, a_dictionary): # calculates number of words speaker has said to replier/replier to speaker total
	global sparsity_measure
	for a in list_of_speakers:
		for b in list_of_speakers:
			sparsity_measure[(a, b)] = [0, 0]
	for x in range(0, (len(a_dictionary) - 1)):
		speaker1 = a_dictionary[x][0][0]
		speaker2 = a_dictionary[x][1][0] 
		sparsity_measure[(speaker1, speaker2)] = [sparsity_measure[(speaker1, speaker2)][0] + len(a_dictionary[x][0]) - len(re.findall(speaker1, str(a_dictionary[x][0]))), sparsity_measure[(speaker1, speaker2)][1] + len(a_dictionary[x][1]) - len(re.findall(speaker2, str(a_dictionary[x][1])))]
	return sparsity_measure	

def document_stuff(directory_location, input_file_name, marker_list, output_file_name, var_x): # writes the final info to a csv file in this order: [DOC ID, speaker, replier, speaker words to replier total, replier words to speaker total, marker, conditional number, speaker marker number, reply marker number, replier utterance number]
	global ordered_utterance_list
	global convo_dict
	global sparsity_measure
	global output_almost
	global final_counter
	global alignment_dict
	global possible_conversation_list
	global speaker_list
	initialize()
	get_childes_files(directory_location, input_file_name)
	determine_speakers(ordered_utterance_list)
	determine_possible_conversations(speaker_list)
	squisher(ordered_utterance_list)
	convo_grouper(squished_dict)
	calculate_sparsity(speaker_list, convo_dict)
	
	utterances = convo_converter(corpus, input_file_name, convo_dict, marker_list)
	results = shared_code.calculateAlignments(utterances, marker_list, 1, 'TRUE_POWER', output_file_name, var_x)	

for dirName, subdirList, fileList in os.walk(corpus_dir):
	for x in subdirList:
		for fname in os.listdir(dirName + '\\' + x):
			if fname.endswith(".xml"):
				os.path.join(dirName + '\\' + x, fname)
				document_stuff(dirName + '\\' + x, fname, marker_list, outputFile, super_var)
				super_var = False

import re
import csv
import nltk
import os
from mychildes import CHILDESCorpusReaderX #modified nltk
marker_list = ['the', 'a'] 
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
project_x = {}

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
		
def conditional_calculator(word): # counts number of time a replier utters a marker given that the speaker utters the marker
	global convo_dict
	global marker_list
	global conditional_conversation_dict
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = convo_dict[x][0][0]
		speaker2 = convo_dict[x][1][0]	
		if word in convo_dict[x][0] and convo_dict[x][1]:
			conditional_conversation_dict[(speaker1, speaker2)] += 1	
	return(conditional_conversation_dict)			

def convo_converter(conversation_dictionary):
	global project_x
	for x in range(0, (len(conversation_dictionary) - 1)):
		speaker1 = convo_dict[x][0][0]
		speaker2 = convo_dict[x][1][0]
		project_x.append({'conv#': (speaker1, speaker2), 'msgUserId': speaker1, 'msg': convo_dict[x][0], 'replyUserId': speaker2, 'reply': convo_dict[x][1], 'msgMarkers': 'msgMarkers', 'replyMarkers': 'replyMarkers', 'msgTokens': 'msgTokens', 'replyTokens': 'replyTokens']
	return project_x		

def meta_data_extractor(word): #gets the rest of the values needed to calculate alignment
	global total_marker_speaker_dict
	global	total_marker_reply_dict
	global	total_utterance_reply_dict
	global convo_dict	
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = convo_dict[x][0][0]
		speaker2 = convo_dict[x][1][0]	
		if word in convo_dict[x][0]:
			total_marker_speaker_dict[(speaker1, speaker2)] += 1
		elif word in convo_dict[x][1]:
			total_marker_speaker_dict[(speaker1, speaker2)] += 1
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = convo_dict[x][0][0]
		speaker2 = convo_dict[x][1][0]
		total_utterance_reply_dict[(speaker1, speaker2)] += 1
	return(total_marker_speaker_dict, total_marker_reply_dict, total_utterance_reply_dict)			

def calculate_alignment(word): # calculates the alignment for each speaker replier pair given some marker; is not used in the final doc, but architecture exists so that it can be included
	global total_marker_speaker_dict
	global total_marker_reply_dict
	global total_utterance_reply_dict
	global conditional_conversation_dict
	global alignment_dict
	global possible_conversation_list
	for x in possible_conversation_list:
		if total_marker_speaker_dict[(x)] != 0 and total_utterance_reply_dict[(x)] != 0:	
			alignment_dict[(x)] = float((conditional_conversation_dict[(x)]/ total_marker_speaker_dict[(x)]) - (total_marker_reply_dict[(x)]/ total_utterance_reply_dict[(x)]))	
		else:
			alignment_dict[(x)] = 'undefined'	
	return(alignment_dict)

def calculate_sparsity(list_of_speakers, a_dictionary): # calculates number of words speaker has said to replier/replier to speaker total
	global sparsity_measure
	for a in list_of_speakers:
		for b in list_of_speakers:
			sparsity_measure[(a, b)] = [0, 0]
	for x in range(0, (len(convo_dict) - 1)):
		speaker1 = a_dictionary[x][0][0]
		speaker2 = a_dictionary[x][1][0] 
		sparsity_measure[(speaker1, speaker2)] = [sparsity_measure[(speaker1, speaker2)][0] + len(a_dictionary[x][0]) - len(re.findall(speaker1, str(a_dictionary[x][0]))), sparsity_measure[(speaker1, speaker2)][1] + len(a_dictionary[x][1]) - len(re.findall(speaker2, str(a_dictionary[x][1])))]

def document_stuff(directory_location, input_file_name, list_of_markers, output_file_name): # writes the final info to a csv file in this order: [DOC ID, speaker, replier, speaker words to replier total, replier words to speaker total, marker, conditional number, speaker marker number, reply marker number, replier utterance number]
	global marker_list
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
	for word in marker_list:
		conditional_calculator(word)
		meta_data_extractor(word)
		calculate_alignment(word)				
		for x in range(0, (len(convo_dict) - 1)):
			speaker1 = convo_dict[x][0][0]
			speaker2 = convo_dict[x][1][0]
			output_almost[final_counter] = [input_file_name, speaker1, speaker2, sparsity_measure[(speaker1, speaker2)][0], sparsity_measure[(speaker1, speaker2)][1], word, conditional_conversation_dict[(speaker1, speaker2)], total_marker_speaker_dict[(speaker1, speaker2)], total_marker_reply_dict[(speaker1, speaker2)], total_utterance_reply_dict[(speaker1, speaker2)]]	
			final_counter += 1
		for y in range(0, (len(output_almost) - 1)):
			if output_almost[y] not in for_output_list:
				for_output_list.append(output_almost[y])	
	with open(output_file_name, "a") as f:
		magic_writer = csv.writer(f)
		magic_writer.writerows(for_output_list)
		f.close()		

corpus_dir =  r'C:\Users\Aaron\AppData\Roaming\nltk_data\corpora\childes\Providence'
rootDir = r'C:\Users\Aaron\AppData\Roaming\nltk_data\corpora\childes\Providence\Alex'

for dirName, subdirList, fileList in os.walk(corpus_dir):
	for subdir in subdirList:
		for dirName, subdirList, fileList in os.walk(subdir):
    		for fname in fileList:
				if fname.endswith(".xml"):
					os.path.join(dirName, fname)
            		document_stuff('corpora\childes\Providence\Alex', fname , marker_list, 'test_run1.csv')


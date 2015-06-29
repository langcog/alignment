import re
import nltk
from nltk.corpus.reader import CHILDESCorpusReader
speaker = 'CHI'
repy = 'MOT'
sp_list = [] # list of markers we are using
del_list = ['*CHI:', '*MOT:', '\t', '\n'] #example delete list
sp_dict1 = dict() # (corresponds to speaker) keys will be markers, values will be their total frequency
sp_dict2 = dict() # (corresponds to replier)
word_list1 = [] # list of all of the child's words
word_list2 = [] # list of all of the replier's words
mega_list = [] # stores lines from file as values in a list
magic_dict = dict() # stores values form mega_list by line numbber
total_mark_count_dict = dict() # stores the conditional marker probability for each marker
conv_dict = dict() # groups adjacent utterances from same speaker into one item and groups speaker - child pairs 
conv_numb = 1 # conversation numbber counter
conditional_marker = 0 # numbber of times *CHI said marker given that speaker said it in previous utterance
marker_count_dict = dict()
magic_dict_counter = 0
aggregate_dict = dict() # stores the alignment values for each marker

def clean_slate(): # cleans all variables/dictionaries/lists
	global sp_dict1
	global sp_dict2
	global word_list1
	global word_list2
	global mega_list
	global magic_dict
	global total_mark_count_dict
	global conv_dict
	global conv_numb
	global conditional_marker
	global marker_count_dict
	global magic_dict_counter
	global aggregate_dict
	sp_dict1 = dict() # (corresponds to speaker) keys will be markers, values will be their total frequency
	sp_dict2 = dict() # (corresponds to replier)
	word_list1 = [] # list of all of the child's words
	word_list2 = [] # list of all of the replier's words
	mega_list = [] # stores lines from file as values in a list
	magic_dict = dict() # stores values form mega_list by line numbber
	total_mark_count_dict = dict() # stores the conditional marker probability for each marker
	conv_dict = dict() # groups adjacent utterances from same speaker into one item and groups speaker - child pairs 
	conv_numb = 1 # conversation numbber counter
	conditional_marker = 0 # numbber of times *CHI said marker given that speaker said it in previous utterance
	marker_count_dict = dict()
	magic_dict_counter = 0
	aggregate_dict = dict() # stores the alignment values for each marker
	for item in sp_list:
		sp_dict1[item] = 0 #initialize dictionaries
		sp_dict2[item] = 0
	return(sp_dict1, sp_dict2, word_list1, word_list2, mega_list, magic_dict, total_mark_count_dict, conv_dict, conv_numb, conditional_marker, marker_count_dict, magic_dict_counter, aggregate_dict)	

def meta_data_collector(speaker1, speaker2, workingfile, corpusreader): # collects things like total word count from CHILDES xml files using nltk
	global sp_list
	global word_list1
	global word_list2
	global sp_dict1
	global sp_dict2
	word_list1 = corpusreader.words(workingfile, speaker=[speaker1])
	for word in word_list1:
		if word in sp_list:
			sp_dict1[word] = sp_dict1[word] + 1 
	word_list2 = corpusreader.words(workingfile, speaker=[speaker2]) 
	for word in word_list2:	
		if word in sp_list:
			sp_dict2[word] = sp_dict2[word] + 1
	return(word_list1, word_list2, sp_dict1, sp_dict2)				 

def nltk_file_caller(speaker, reply, file_location, file_directory, file_number, file_format): # calls CHILDES xml files and invokes meta data collection
	global word_list1
	global word_list2
	global sp_dict1
	global sp_dict2
	corpus_root = nltk.data.find(file_location) #childes file directoryfor item in sp_list:
	corpus_reader = CHILDESCorpusReader(corpus_root, file_directory + '.*.' + file_format) #specific corpus we're using
	current_file = file_directory + file_number + file_format
	meta_data_collector(speaker, reply, current_file, corpus_reader)
	return(word_list1, word_list2, sp_dict1, sp_dict2)

# sp_dict[item] is the total numbber of times the marker appears in the file for a certain speaker
# len(word_list) is the total numbber of tokens in the file for a certain speaker
# base_prob is ratio of sp_dict[item] ot len(word_list)	

def mega_list_maker(fhand, speaker_identifier, reply_identifier): # puts all the lines from CHA file into a list
	global sp_list
	global total_mark_count_dict
	global mega_list
	for word in sp_list:
		total_mark_count_dict[word] = 0
	with open(fhand, 'r') as r_file:
		for line in r_file:
			if line.startswith(speaker_identifier or reply_identifier):
				line.rstrip()
				mega_list.append(line)
	return(total_mark_count_dict, mega_list)				

def convo_squisher(speaker_search, reply_search): # organizes list of cleaned lines into squished 1 speaker utterance, 1 reply utterance conversations
	global mega_list
	global magic_dict
	global magic_dict_counter
	global conv_numb
	global conv_dict
	for numb in range(0, (len(mega_list) - 2)): 
		if re.search(speaker_search, str(mega_list[numb])) and re.search(speaker_search, str(mega_list[numb + 1])):
			magic_dict[magic_dict_counter] = mega_list[numb] + mega_list[numb + 1]
			magic_dict_counter += 1
		elif re.search(reply_search, str(mega_list[numb])) and re.search(reply_search, str(mega_list[numb + 1])):
			magic_dict[magic_dict_counter] = mega_list[numb] + mega_list[numb + 1]
			magic_dict_counter += 1
		else:
			magic_dict[magic_dict_counter] = mega_list[numb]
			magic_dict_counter += 1	
	for numb in range(0, (len(magic_dict) - 1)):
		if re.search(speaker_search, str(magic_dict[numb])) and re.search(reply_search, str(magic_dict[numb + 1])):
			conv_dict[conv_numb] = [magic_dict[numb], magic_dict[numb + 1]]
			conv_numb += 1
	return(conv_numb, conv_dict)		

def cleanup(hit_list): # deletes annoying recurring CHAT idiosyncracies that don't parse well
	global conv_dict
	for numb in range(0, (len(conv_dict) - 1)):
		for item in conv_dict[numb]:
			for thing in hit_list:
				item = item.replace(thing, " ")
	return(conv_dict)				

def calculate_cond_marker(): # calculates the number of times reply says the marker given the child said it
	global marker_count_dict
	global conv_dict
	global conditional_marker
	global total_mark_count_dict
	for numb in range(0, (conv_numb - 1)):
		marker_count_dict[numb] = 0
	for thing in sp_list:	
		for numb in range(1, (conv_numb - 1)):
			if re.search(thing, conv_dict[numb][0]):
				marker_count_dict[numb] = (len(re.findall(thing, conv_dict[numb][1])) - len(re.findall('\S' + thing + '\S', conv_dict[numb][1])) - len(re.findall('\S' + thing + '\s', conv_dict[numb][1])) - len(re.findall('\s' + thing + '\S', conv_dict[numb][1])))
				if marker_count_dict[numb] < 0:
					marker_count_dict[numb] = 0
			elif re.search(thing, conv_dict[numb][0]) == False:
				marker_count_dict[numb] = 0
		for numb in range(1, (conv_numb - 1)):
			conditional_marker += marker_count_dict[numb]
		total_mark_count_dict[thing] = conditional_marker
	return(total_mark_count_dict)	

def calculate_alignment(): # calculates alignment
	global aggregate_dict
	global total_mark_count_dict
	global word_list1
	global word_list2
	global sp_dict1
	global sp_dict2
	for thing in sp_list:
		aggregate_dict[thing] = 0
	for thing in sp_list:
		aggregate_dict[thing] = float((((total_mark_count_dict[thing] * len(word_list1)) / (len(word_list2) * sp_dict1[thing])) - (sp_dict2[thing] / len(word_list2))))			
	return(aggregate_dict)	

# runs all functions in correct order and calculates alignment
def meta_function(speaker_, reply_, file_location_, file_directory_, file_number_, file_format_, fhand_, speaker_identifier_, reply_identifier_, speaker_search_, reply_search_, hit_list_):
	clean_slate()
	nltk_file_caller(speaker_, reply_, file_location_, file_directory_, file_number_, file_format_)
	mega_list_maker(fhand_, speaker_identifier_, reply_identifier_)
	convo_squisher(speaker_search_, reply_search_)
	cleanup(hit_list_)
	calculate_cond_marker()
	calculate_alignment()
	return(aggregate_dict)

meta_function('CHI', 'MOT', 'corpora/childes', 'Valian/', '01a', '.xml', 'Valian/01a.CHA', '*CHI:', '*MOT', 'CHI:', 'MOT:', del_list) #example arguments
for thing in sp_list:
	print(aggregate_dict[thing])

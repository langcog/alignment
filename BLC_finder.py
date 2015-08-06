import nltk
from nltk.corpus import wordnet as wn
import csv
import re
import enchant

fdist = {}
perm_dict = {}
ref_dict = {}
branch_list = []
branch_dict = {}

freq_file_name = r'C:\Users\Aaron\alignment\lemma.num'
outfilename = r'C:\Users\Aaron\alignment\BLCListViaBNC.csv'

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

def branch_finder():
	global fdist
	global branch_list
	global ref_dict
	d = enchant.Dict("en_US")
	hypo = lambda s: s.hyponyms()
	hyper = lambda s: s.hypernyms()
	for key in fdist.keys():
		if (key, 'n') not in ref_dict.keys():
				continue
		elif (key, 'v') in ref_dict.keys():
			if (ref_dict[(key, 'v')] > ref_dict[(key, 'n')]):
				continue
		elif (key, 'a') in ref_dict.keys():
			if (ref_dict[(key, 'a')] > ref_dict[(key, 'n')]):
				continue
		elif (key, 'adv') in ref_dict.keys():
			if (ref_dict[(key, 'adv')] > ref_dict[(key, 'n')]):
				continue
		else:
			try:
				trill_homie = -1
				sp_counter = True
				checked_item = wn.synset(key + '.n.01')
				if len(list(checked_item.closure(hypo))) == 0:
					hypo_list = [checked_item]
				else:	
					hypo_list = list(checked_item.closure(hypo))
				while sp_counter == True:
					if d.check(hypo_list[trill_homie]) == True:
						sp_item = hypo_list[trill_homie]
						sp_counter = False
					else:
						trill_homie = trill_homie - 1	

				sp_branch = [sp_item.lemmas()[0].name()]
				for item in list(sp_item.closure(hyper)):
					if len(re.findall('.n.01', str(item))) == 1:
						sp_branch.append(item.lemmas()[0].name())
				if sp_branch not in branch_list:
					branch_list.append(sp_branch)
			except:
				continue		
	return(branch_list)
	
def get_branch_values():
	global ref_dict
	global fdist			
	global branch_list
	global branch_dict
	temp_dict = {}
	for lst in branch_list:
		if len(lst) > 3:
			temp_dict[lst[0]] = lst
			#1
			branch_dict[lst[0]] = [lst[0]]
			#2
			branch_dict[lst[0]].append(len(lst))
	for key in temp_dict.keys():
		max1 = 'NA'
		max2 = 'NA'
		max3 = 'NA'
		max1a = 0
		max2a = 0
		max3a = 0
		total_count = 0
		for item in temp_dict[key]:
			try:
				total_count = total_count + fdist[item]
				if fdist[item] >= max1a:
					max3a = max2a
					max3 = max2
					max2a = max1a
					max2 = max1
					max1a = fdist[item]
					max1 = item
				elif fdist[item] >= max2a:
					max3a = max2a
					max3 = max2
					max2a = fdist[item]
					max2 = item
				elif fdist[item] >= max3a:
					max3a = fdist[item]
					max3 = item
			except:
				continue		
		#3			
		branch_dict[key].append(total_count)		
		#4
		branch_dict[key].append(max1)
		#5
		branch_dict[key].append(max1a)
		#6
		branch_dict[key].append(float(max1a / total_count))
		#7
		if max1 != 'NA':
			branch_dict[key].append((len(list(wn.synset(max1 + '.n.01').closure(hyper))) / branch_dict[key][0]))
		else:
			branch_dict[key].append('NA')	
		#8
		branch_dict[key].append(max2)
		#9
		branch_dict[key].append(max2a)
		#10
		branch_dict[key].append(float(max2a / total_count))
		#11
		if max2 != 'NA':
			branch_dict[key].append((len(list(wn.synset(max2 + '.n.01').closure(hyper))) / branch_dict[key][0]))
		else:
			branch_dict[key].append('NA')
		#12
		branch_dict[key].append(max3)
		#13
		branch_dict[key].append(max3a)
		#14
		branch_dict[key].append(float(max3a / total_count))
		#15
		if max3 != 'NA':
			branch_dict[key].append((len(list(wn.synset(max3 + '.n.01').closure(hyper))) / branch_dict[key][0]))
		else:
			branch_dict[key].append('NA')	
	return(branch_dict)			

def writeHeader(outputFile):
	header = []
	header.insert(0, ["Bottom Level Word", "Approx Branch Length", "Total Branch Frequency", "BLC 1", "BLC 1 Frequency", "BLC 1 Branch Freq Proportion", 'BLC 1 Location', "BLC 2", "BLC 2 Frequency", "BLC 2 Branch Freq Proportion", 'BLC 2 Location', "BLC 3", "BLC 3 Frequency", "BLC 3 Branch Freq Proportion", 'BLC 3 Location'])
	with open(outputFile, 'a', newline='') as f:
		writer = csv.writer(f)
		writer.writerows(header)
	f.close()

def write_file(output_file_name):
	global branch_dict
	output_list = []
	for key in branch_dict.keys():
		output_list.append(branch_dict[key])
	with open(output_file_name, "a", newline='') as f:
		magic_writer = csv.writer(f)
		magic_writer.writerows(output_list)
		f.close()

read_Freq_File(freq_file_name)
branch_finder()
get_branch_values()
writeHeader(outfilename)
write_file(outfilename)




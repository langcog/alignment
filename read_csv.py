
# https://www.daniweb.com/software-development/python/code/216750/group-a-list-of-dictionaries-python

import csv
import operator
import itertools
import pprint 
import re

def readCSV():
	csvFile = "pairedtweets1000.txt"
	reader=csv.reader(open(csvFile))
	utterances = []
	for row in reader:
		row = re.split('\t+', row[0])
		print(row)
		toAppend = {}
		toAppend["conv#"] = row[0]
		toAppend["msgUserId"] = row[1]
		toAppend["msg"] = row[2]
		toAppend["replyId"] = row[3]
		toAppend["replyUserId"] = row[4]
		toAppend["reply"] = row[5]
		utterances.append(toAppend)
	return utterances

def sort(utterances):
	utterances.sort(key=operator.itemgetter('conv#'))
	return utterances

def groupByConvos(seq):
	seq.sort(key = itemgetter(0))
	groups = groupby(seq, itemgetter(0))
	print([[item[0] for item in data] for (key, data) in groups])


utterances = readCSV()
utterances = sort(utterances)

list1 = []
for key, items in itertools.groupby(utterances, operator.itemgetter('conv#')):
    list1.append(list(items))
print(len(list1))
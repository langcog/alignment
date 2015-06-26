
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
		toAppend = {}
		toAppend["conv#"] = row[0]
		toAppend["msgUserId"] = row[1]
		toAppend["msg"] = row[2]
		toAppend["replyId"] = row[3]
		toAppend["replyUserId"] = row[4]
		toAppend["reply"] = row[5]
		utterances.append(toAppend)
	return utterances

def group(utterances):
	utterances.sort(key=operator.itemgetter('conv#'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('conv#')):
		list1.append(list(items))
	return utterances

def getUserUtterances(utterances):
	users = {}
	for utterance in utterances:
		users[utterance["msgUserId"]] = []
		users[utterance["replyUserId"]] = []
	for utterance in utterances:
		msgSplit = utterance["msg"].split(" ")
		replySplit = utterance["reply"].split(" ")
		users[utterance["msgUserId"]].append(msgSplit)
		users[utterance["replyUserId"]].append(replySplit)

	for key, value in users.iteritems():
		chain = itertools.chain(*value)
		users[key] = list(chain)
	return users

utterances = readCSV()
groupedUtterances = group(utterances)
users = getUserUtterances(utterances)



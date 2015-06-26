import csv
import operator
import itertools
import pprint 
import re
import random

def readCSV(markers):
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
		toAppend["msgMarkers"] = []
		toAppend["replyMarkers"] = []
		messages = row[2].split(" ")
		replies = row[5].split(" ")
		for marker in markers:
			if marker in messages:
				toAppend["msgMarkers"].append(marker)
			if marker in replies:
				toAppend["replyMarkers"].append(marker)
		utterances.append(toAppend)
	return utterances

def group(utterances):
	utterances.sort(key=operator.itemgetter('conv#'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('conv#')):
		list1.append(list(items))
	return list1

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
		users[key] = {}
		users[key]["tokens"] = list(chain)
	return users

def readMarkers():
	csvFile = "test.csv"
	reader=csv.reader(open(csvFile))
	markers = []
	for row in reader:
		row = re.split('\t+', row[0])
		markers.append(row[1])
		markers.append(row[3])
	return markers

def calculateProbabilities(users, markers):
	for key in users:
		users[key]["markers"] = {}
		users[key]["markerProbs"] = {}
		for marker in markers:
			
			users[key]["markers"][marker] = 0
			tokens = users[key]["tokens"]

			for token in tokens:
				
				if token == marker:
					users[key]["markers"][marker] = users[key]["markers"][marker] + 1
			for marker in markers:
				if(marker in users[key]["markers"]):
					users[key]["markerProbs"][marker] = users[key]["markers"][marker]/len(tokens)

	return users

def getRandUser(users):
	return users[random.choice(users.keys())]

def bayesProbs(groupedUtterances, users, markers):
	for convo in groupedUtterances:
		counts = {}
		bCounts = {}
		wordCounts = {}
		a = convo[0]["msgUserId"]
		b = convo[0]["replyUserId"]
		wordCounts[a] = 0
		wordCounts[b] = 0
		for marker in markers:
			counts[marker] = 0
			bCounts[marker] = 0
		for utterance in convo:
			if((not (utterance["msgUserId"] in wordCounts)) or (not (utterance["replyUserId"]) in wordCounts)):
				continue
			wordCounts[utterance["msgUserId"]] = wordCounts[utterance["msgUserId"]] + len(utterance["msg"])
			wordCounts[utterance["replyUserId"]] = wordCounts[utterance["replyUserId"]] + len(utterance["reply"])
			msgMarkers = utterance["msgMarkers"]
			replyMarkers = utterance["replyMarkers"]
			if(utterance["msgUserId"] == b):
				for marker in msgMarkers:
					bCounts[marker] = bCounts[marker] + 1
			else:
				for marker in replyMarkers:
					bCounts[marker] = bCounts[marker] + 1
			intersect = list(set(replyMarkers).intersection(msgMarkers))
			for marker in intersect:
				counts[marker] = counts[marker] + 1
		usersIds = wordCounts.keys()
		if(len(usersIds) != 2):
			continue
		for marker in markers:
			if(counts[marker] > 0):
				powerProb = (counts[marker]*wordCounts[a])/(wordCounts[b]*bCounts[marker])
				baseProb = bCounts[marker]/float(wordCounts[b])
				if(powerProb > 0):
					print(powerProb)
					print(baseProb)
					prob = powerProb-baseProb
					print(prob)
					print("----------------")
		
markers = readMarkers()
utterances = readCSV(markers)
groupedUtterances = group(utterances)

users = getUserUtterances(utterances)
users = calculateProbabilities(users, markers)
bayesProbs(groupedUtterances, users, markers)
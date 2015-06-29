import csv
import operator
import itertools
import pprint 
import re
import random
import tokenize

inputFile = "pairedtweets1000.txt"
markersFile = "test.csv"
outputFile = "results.csv"

# Reads in tweets
def readCSV(markers, inputFile):
	reader=csv.reader(open(inputFile))
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

# Groups tweets by conversation numbers
def group(utterances):
	utterances.sort(key=operator.itemgetter('conv#'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('conv#')):
		list1.append(list(items))
	return list1

# Adds information about individual users to the users dictionary
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

# Reads a list of markers from the markersFile
def readMarkers(markersFile):
	reader=csv.reader(open(markersFile))
	markers = []
	for row in reader:
		markers.append(row[0])
	return markers

# Calculates the probabilities that a given user uses a marker
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

# Returns a random user - useful for debugging
def getRandUser(users):
	return users[random.choice(users.keys())]

# Computers the power probabilities
def bayesProbs(groupedUtterances, users, markers):
	results = []
	for convo in groupedUtterances:
		counts = {}
		bCounts = {}
		aCounts = {}
		wordCounts = {}
		a = convo[0]["msgUserId"]
		b = convo[0]["replyUserId"]
		wordCounts[a] = 0
		wordCounts[b] = 0
		for marker in markers:
			counts[marker] = 0
			bCounts[marker] = 0
			aCounts[marker] = 0
		for utterance in convo:
			if((not (utterance["msgUserId"] in wordCounts)) or (not (utterance["replyUserId"]) in wordCounts)):
				continue
			wordCounts[utterance["msgUserId"]] = wordCounts[utterance["msgUserId"]] + len(utterance["msg"].split(" "))
			wordCounts[utterance["replyUserId"]] = wordCounts[utterance["replyUserId"]] + len(utterance["reply"].split(" "))
			msgMarkers = utterance["msgMarkers"]
			replyMarkers = utterance["replyMarkers"]
			if(utterance["msgUserId"] == b):
				for marker in msgMarkers:
					bCounts[marker] = bCounts[marker] + 1
			else:
				for marker in replyMarkers:
					bCounts[marker] = bCounts[marker] + 1
			if(utterance["msgUserId"] == a):
				for marker in msgMarkers:
					aCounts[marker] = aCounts[marker] + 1
			else:
				for marker in replyMarkers:
					aCounts[marker] = aCounts[marker] + 1
			intersect = list(set(replyMarkers).intersection(msgMarkers))
			for marker in intersect:
				counts[marker] = counts[marker] + 1
		usersIds = wordCounts.keys()
		if(len(usersIds) != 2):
			continue
		for marker in utterance["msgMarkers"]:
			if(counts[marker] > 0):
				powerProb = (counts[marker]*wordCounts[a])/(wordCounts[b]*aCounts[marker])
				baseProb = bCounts[marker]/float(wordCounts[b])
				if(powerProb > 0):
					prob = powerProb-baseProb
					results.append([convo[0]["conv#"], marker, prob])
	results = sorted(results, key=lambda k: -k[2])
	return results

# Writes probabilities to the output file
def writeFile(toWrite, outputFile):
	with open(outputFile, "wb") as f:
		writer = csv.writer(f)
		writer.writerows(toWrite)
	f.close()

# Prints the conversations with the max and least powers
def testResults(results, groupedUtterances):
	results = sorted(results, key=lambda k: -k[2])
	maxPower = results[0][0]
	maxPower = findConvo(maxPower, groupedUtterances)
	print(maxPower)
	leastPower = results[len(results)-1][0]
	leastPower = findConvo(leastPower, groupedUtterances)
	print(leastPower)

# Finds a conversation given it's conversation #
def findConvo(convo, groupedUtterances):
	for groupedUtterance in groupedUtterances:
		if groupedUtterance[0]["conv#"] == convo:
			return groupedUtterance
	return False

# Just outputs lines to help when debugging
def initialize():
	print("--------------")
	print("--------------")
	print("--------------")

initialize()
markers = readMarkers(markersFile)
utterances = readCSV(markers, inputFile)
groupedUtterances = group(utterances)
users = getUserUtterances(utterances)
users = calculateProbabilities(users, markers)
results = bayesProbs(groupedUtterances, users, markers)
writeFile(results, outputFile)
testResults(results, groupedUtterances)
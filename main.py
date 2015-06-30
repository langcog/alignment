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
def setUp(groupedUtterances, users, markers):
	results = []
	for i, convo in enumerate(groupedUtterances):
		toPush = {}
		both = {}
		a = convo[0]["msgUserId"]
		b = convo[0]["replyUserId"]
		numUtterances = len(convo)
		if(a == b): # No self aligning stuff
			continue
		for marker in markers:
			toPush[a + marker] = 0
			toPush[b + marker] = 0
			both[marker] = 0
		for j, marker in enumerate(markers):
			for utterance in convo:
				if(utterance["msgUserId"] != a and utterance["replyUserId"] != b):
					continue
				elif (utterance["msgUserId"] != b and utterance["replyUserId"] != b):
					continue
				
				if marker in utterance["msgMarkers"]:
					toPush[utterance["msgUserId"] + marker] = toPush[utterance["msgUserId"] + marker] + 1
				if marker in utterance["replyMarkers"]:
					toPush[utterance["replyUserId"] + marker] = toPush[utterance["replyUserId"] + marker] + 1
				if marker in utterance["msgMarkers"] and marker in utterance["replyMarkers"]:
					both[marker] = both[marker] + 1
		results.append({"numUtterances": numUtterances,  "both": both, "userMarkers": toPush, "a": a, "b": b, "conv": convo[0]["conv#"]})

	return results

def bayesProbs(results, markers):
	toReturn = []
	for result in results:
		for marker in markers:
			if(result["userMarkers"][result["a"]+marker] == 0):
				continue
			powerProb = float(result["both"][marker])/float(result["userMarkers"][result["a"]+marker])
			baseProb = float(result["userMarkers"][result["b"]+marker])/float(result["numUtterances"])
			prob = powerProb - baseProb
			if(prob == -1.5 and result["b"] == "2825905798"):
				print(powerProb)
				print(baseProb)
				print(prob)
				print(marker)
				print(float(result["userMarkers"][result["b"]+marker]))
				print(result["b"])
				print(result["a"])
				print(float(result["numUtterances"]))
				print("------------")
			toReturn.append([result["conv"], marker, prob])
	toReturn = sorted(toReturn, key=lambda k: -k[2])
	return toReturn

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
results = setUp(groupedUtterances, users, markers)
results = bayesProbs(results, markers)
writeFile(results, outputFile)
#testResults(results, groupedUtterances)
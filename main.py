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

# Just outputs lines to help when debugging
def initialize():
	print("--------------")
	print("--------------")
	print("--------------")

# Reads a list of markers from the markersFile
def readMarkers(markersFile):
	reader=csv.reader(open(markersFile))
	markers = []
	for row in reader:
		markers.append(row[0])
	return markers

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

# Computers the power probabilities
def setUp(groupedUtterances, markers):
	results = []
	for i, convo in enumerate(groupedUtterances):
		toPush = {}
		intersect = {} # Number of times Person A and person B says the marker
		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		numUtterances = len(convo) # Number of total utterances in the conversation
		if(a == b): # No self aligning stuff
			continue
		for marker in markers:
			toPush[a + marker] = 0 # Set all markers to uttered by a to 0
			toPush[b + marker] = 0 # Same as above but with b
			intersect[marker] = 0 # Same as above but with intersect a and b
		for j, marker in enumerate(markers):
			for utterance in convo:
				# If there's a third person in the conversation, ignore the convo
				if(utterance["msgUserId"] != a and utterance["replyUserId"] != a): 
					continue
				elif (utterance["msgUserId"] != b and utterance["replyUserId"] != b):
					continue

				# Increments values of toPush and intersect depending on whether a marker is in the current utterance
				if marker in utterance["msgMarkers"]:
					toPush[utterance["msgUserId"] + marker] = toPush[utterance["msgUserId"] + marker] + 1
				if marker in utterance["replyMarkers"]:
					toPush[utterance["replyUserId"] + marker] = toPush[utterance["replyUserId"] + marker] + 1
				if marker in utterance["msgMarkers"] and marker in utterance["replyMarkers"]:
					intersect[marker] = intersect[marker] + 1

		results.append({"numUtterances": numUtterances,  "intersect": intersect, "userMarkers": toPush, "a": a, "b": b, "conv": convo[0]["conv#"]})
	return results

# Formula = (utterances that A and B have said with the marker)/(utterances that A has said with marker) - (utterances B has said with marker)/(total utterances)
def bayesProbs(results, markers):
	toReturn = []
	for result in results:
		for marker in markers:
			# If a doesn't say the marker, ignore
			# (Otherwise we get a divide by 0 error)
			if(result["userMarkers"][result["a"]+marker] == 0):
				continue
			powerProb = float(result["intersect"][marker])/float(result["userMarkers"][result["a"]+marker])
			baseProb = float(result["userMarkers"][result["b"]+marker])/float(result["numUtterances"])
			prob = powerProb - baseProb
			toReturn.append([result["conv"], marker, prob])
	
	toReturn = sorted(toReturn, key=lambda k: -k[2])
	return toReturn

# Writes stuff to the output file
def writeFile(toWrite, outputFile):
	with open(outputFile, "wb") as f:
		writer = csv.writer(f)
		writer.writerows(toWrite)
	f.close()

# Finds a conversation given it's conversation #
def findConvo(convo, groupedUtterances):
	for groupedUtterance in groupedUtterances:
		if groupedUtterance[0]["conv#"] == convo:
			return groupedUtterance
	return False

# Prints the conversations with the max and least powers
def testBoundaries(results, groupedUtterances):
	results = sorted(results, key=lambda k: -k[2])
	maxPower = results[0]
	maxConvo = findConvo(maxPower[0], groupedUtterances)
	leastPower = results[len(results)-1]
	leastConvo = findConvo(leastPower[0], groupedUtterances)
	print(maxPower)
	print(leastPower)

def testNumResults(results, groupedUtterances, markers):
	allCount = 0
	for result in results:
		for marker in markers:
			if(result["intersect"][marker] > 0):
				allCount = allCount + 1
	print("Conversations in which both A and B say a marker: " + str(allCount))

initialize()
markers = readMarkers(markersFile)
utterances = readCSV(markers, inputFile)
groupedUtterances = group(utterances)
setUppedResults = setUp(groupedUtterances, markers)
results = bayesProbs(setUppedResults, markers)
writeFile(results, outputFile)
testBoundaries(results, groupedUtterances)
testNumResults(setUppedResults, groupedUtterances, markers)
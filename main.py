import csv
import operator
import itertools
import re
import traceback
inputFile = "toy.users"
#inputFile = "pairedtweets1000.txt"
markersFile = "test.csv"
outputFile = "results.csv"

# Just outputs lines to help when debugging
def initialize():
	print("--------------")
	print("--------------")
	print("--------------")

# Prints the name of the function that called log and prints the line number
# Useful for debugging
def log(toPrint):
	print(traceback.extract_stack()[1][2] + " line " + str(traceback.extract_stack()[1][1]))
	print(toPrint)
	print("---------")

# Reads a list of markers from the markersFile
def readMarkers(markersFile):
	reader = csv.reader(open(markersFile))
	markers = []
	for row in reader:
		markers.append(row[0])
	return markers

# Reads in tweets
def readCSV(markers, inputFile):
	reader=csv.reader(open(inputFile),dialect="excel-tab")
	utterances = []
	linenum = 0
	header=True
	for row in reader:
		if header:
			header=False
			continue
		toAppend = {}
		toAppend["conv#"] = row[1]+"_"+row[4]
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
		userMarkers = {}
		intersect = {} # Number of times Person A and person B says the marker
		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		numUtterances = len(convo) # Number of total utterances in the conversation
		if(a == b): # No self aligning stuff
			continue
		#for marker in markers:
		#	userMarkers[a + marker] = 0 # Set all markers to uttered by a to 0
		#	userMarkers[b + marker] = 0 # Same as above but with b
		#	intersect[marker] = 0 # Same as above but with intersect a and b
		for j, marker in enumerate(markers):
			for utterance in convo:
				# If there's a third person in the conversation, ignore the convo
				if(utterance["msgUserId"] != a and utterance["replyUserId"] != a): 
					continue
				elif (utterance["msgUserId"] != b and utterance["replyUserId"] != b):
					continue
				# Increments values of userMarkers and intersect depending on whether a marker is in the current utterance
				if marker in utterance["msgMarkers"]:
					userMarkers[utterance["msgUserId"] + marker] = userMarkers.get(utterance["msgUserId"] + marker ,0) + 1
				if marker in utterance["replyMarkers"]:
					userMarkers[utterance["replyUserId"] + marker] = userMarkers.get(utterance["replyUserId"] + marker ,0) + 1
				if marker in utterance["msgMarkers"] and marker in utterance["replyMarkers"]:
					intersect[marker] = intersect.get(marker,0) + 1

		results.append({"numUtterances": numUtterances,  "intersect": intersect, "userMarkers": userMarkers, "a": a, "b": b, "conv": convo[0]["conv#"]})

	return results

# Formula = (utterances that A and B have said with the marker)/(utterances that A has said with marker) - (utterances B has said with marker)/(total utterances)
def bayesProbs(results, markers):
	toReturn = []
	for result in results:
		for marker in markers:
			# If a doesn't say the marker, ignore
			# (Otherwise we get a divide by 0 error)
			if((result["a"]+marker) not in result["userMarkers"]):
				continue

			powerProb = float(result["intersect"].get(marker, 0))/float(result["userMarkers"][result["a"]+marker])
			baseProb = float(result["userMarkers"].get(result["b"]+marker, 0))/float(result["numUtterances"])
			prob = powerProb - baseProb
			if(result["a"] == "4" and result["b"] == "3" and marker == "the"):
				log("Power Prob: " + str(powerProb))
				log("Base Prob: " + str(baseProb))
				log("Intersection: " + str(result["intersect"].get(marker, 0)))
				log("A marker: " + str(result["userMarkers"][result["a"]+marker]))
				log("B marker: " + str(result["userMarkers"][result["b"]+marker]))
				log("B total: " + str(result["numUtterances"]))
			toReturn.append([result["conv"], marker, prob])
	
	toReturn = sorted(toReturn, key=lambda k: -k[2])
	#toReturn.insert(0, ["speakerID_replierID", "Marker", "Alignment"])
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
	log("Max Alignment: " + str(maxPower))
	log("Min Alignment: " + str(leastPower))

# Finds out the number of conversations in which both A and B say the same marker
def testNumResults(results, groupedUtterances, markers):
	allCount = 0
	for result in results:
		for marker in markers:
			if(marker in result["intersect"]):
				allCount = allCount + 1
	log("Conversations in which both A and B say a marker: " + str(allCount))


def testSetUp(groupedUtterances, markers, results, debug):
	for index in range(1, 10):
		current = results[index]
		intersect = current["intersect"]
		convo = findConvo(current["conv"], groupedUtterances)
		a = current["a"]
		b = current["b"]
		userMarkers = current["userMarkers"]

		# Tests that the markers in intersect are actually said by A and B
		for key, value in intersect.iteritems():
			if(value > 0):
				replyMarkers = []
				msgMarkers = []
				for utterance in convo:
					replyMarkers.append(utterance["replyMarkers"])
					msgMarkers.append(utterance["msgMarkers"])
				replyMarkers = list(itertools.chain(*replyMarkers))
				msgMarkers = list(itertools.chain(*msgMarkers))
				replyMarkers = list(set(replyMarkers))
				msgMarkers = list(set(msgMarkers))
				if(debug):
					log("Intersection: " + key)
					log("Reply Markers: " + str(sorted(replyMarkers)))
					log("Message Markers: " + str(sorted(msgMarkers)))
				if(not (key in msgMarkers and key in replyMarkers)):
					log("Something went wrong...")
		
		# Tests if the markers that a or b says are actually in the conversation
		for marker in markers:
			if((a+marker) in userMarkers):
				if(not (marker in str(convo))):
					log("Something went wrong...")
			if((b + marker) in userMarkers):
				if(not (marker in str(convo))):
					log("Something went wrong...")

	return

def testBayes(results, groupedUtterances):
	for index in range(1, 10):
		current = results[index]
		log(current)
		conv = findConvo(current[0], groupedUtterances)
		log(conv)
		intersect = current["intersect"]
initialize()
markers = readMarkers(markersFile)
utterances = readCSV(markers, inputFile)
groupedUtterances = group(utterances)
setUppedResults = setUp(groupedUtterances, markers)
results = bayesProbs(setUppedResults, markers)
#testSetUp(groupedUtterances, markers, setUppedResults, False)
#testBayes(results, groupedUtterances)
writeFile(results, outputFile)
testBoundaries(results, groupedUtterances)
testNumResults(setUppedResults, groupedUtterances, markers)

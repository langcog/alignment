import csv
import traceback
import operator
import itertools

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


# Writes stuff to the output file
def writeFile(toWrite, outputFile, writeType):
	toWrite.insert(0, ["Corpus", "DocId", "ConvId", "SpeakerA", "SpeakerB", "Marker", "Alignment", "Utterances that A and B have said with the marker", "Utterances that A has said with marker", "Utterances B has said with marker", "Total utterances", "Sparsity A->B", "Sparsity B->A"])
	with open(outputFile, writeType) as f:
		writer = csv.writer(f)
		writer.writerows(toWrite)
	f.close()

# Reads a list of markers from the markersFile
def readMarkers(markersFile):
	reader = csv.reader(open(markersFile))
	markers = []
	for row in reader:
		markers.append(row[0])
	return markers

# Groups tweets by conversation numbers
def group(utterances):
	utterances.sort(key=operator.itemgetter('convId'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('convId')):
		list1.append(list(items))
	return list1

# Computers the power probabilities
def metaDataExtractor(groupedUtterances, markers):
	results = []
	for i, convo in enumerate(groupedUtterances):
		userMarkers = {}
		intersect = {} # Number of times Person A and person B says the marker
		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		numUtterances = len(convo) # Number of total utterances in the conversation
		if(a == b): # No self aligning stuff
			continue
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
		results.append({"numUtterances": numUtterances,  "intersect": intersect, "userMarkers": userMarkers, "a": a, "b": b, "conv": convo[0]["convId"], "corpus": utterance["corpus"], "docId": utterance["docId"]})
	return results

# Formula = (utterances that A and B have said with the marker)/(utterances that A has said with marker) - (utterances B has said with marker)/(total utterances)
def calculateAlignment(results, markers, sparsities):
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
			sparsity = sparsities[(result["a"], result["b"])]
			toReturn.append([result["corpus"], result["docId"], result["conv"], result["a"], result["b"], marker, prob, float(result["intersect"].get(marker, 0)), float(result["userMarkers"][result["a"]+marker]), float(result["userMarkers"].get(result["b"]+marker, 0)), float(result["numUtterances"]), sparsity[0], sparsity[1]])
	toReturn = sorted(toReturn, key=lambda k: -k[6])
	#toReturn.insert(0, ["speakerID_replierID", "Marker", "Alignment"])
	return toReturn

# Finds a conversation given it's conversation #
def findConvo(convo, groupedUtterances):
	for groupedUtterance in groupedUtterances:
		if groupedUtterance[0]["convId"] == convo:
			return groupedUtterance
	return False

def calculateSparsity(groupedUtterances): # calculates number of words speaker has said to replier/replier to speaker total
	sparsity_measure = {}
	for convo in groupedUtterances:
		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		sparsity_measure[(a, b)] = [0, 0]
	 	for utterance in convo:
	 		sparsity_measure[(a, b)] = [sparsity_measure[(a, b)][0] + len(utterance["msgTokens"]), sparsity_measure[(a, b)][1] + len(utterance["replyTokens"])]
	return sparsity_measure

# Prints the conversations with the max and least powers
def testBoundaries(results, groupedUtterances):
	results.pop(0)
	results = sorted(results, key=lambda k: -k[6])
	maxPower = results[4]
	maxConvo = findConvo(maxPower[0], groupedUtterances)
	leastPower = results[len(results)-1]
	leastConvo = findConvo(leastPower[0], groupedUtterances)
	log("Max Alignment: " + str(maxPower))
	log("Min Alignment: " + str(leastPower))
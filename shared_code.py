import csv
import traceback
import operator
import itertools
import datetime
from random import randint
import math
from multiprocessing import Pool

# Just outputs lines to help when debugging
def initialize():
	print("--------------")
	print(datetime.datetime.now().time())
	print("--------------")

# Prints the name of the function that called log and prints the line number
# Useful for debugging
def log(toPrint):
	print(traceback.extract_stack()[1][2] + " line " + str(traceback.extract_stack()[1][1]) + " at " + str(datetime.datetime.now().time()))
	print(toPrint)
	print("---------")

def writeHeader(outputFile, writeType):
	header = []
	header.insert(0, ["Corpus", "DocId", "ConvId", "SpeakerA", "SpeakerB", "Marker", "Alignment", "Utterances that A and B have said with the marker", "Utterances that A has said with marker", "Utterances B has said with marker", "Total utterances", "Sparsity A->B", "Sparsity B->A", "Child Age", "Child Gender"])
	with open(outputFile, writeType, newline='') as f:
		writer = csv.writer(f)
		writer.writerows(header)
	f.close()


# Writes stuff to the output file
def writeFile(results, outputFile, shouldWriteHeader):
	header = list(results[0].keys())
	toWrite = []
	for row in results:
		toAppend = []
		for key in header:
			toAppend.append(row[key])
		toWrite.append(toAppend)
	if(shouldWriteHeader):
		with open(outputFile, "w", newline='') as f:
			writer = csv.writer(f)
			writer.writerows([header])
		f.close()

	with open(outputFile, "a", newline='') as f:
		writer = csv.writer(f)
		writer.writerows(toWrite)
	f.close()

# Reads a list of markers from the markersFile
def readMarkers(markersFile):
	reader = csv.reader(open(markersFile))
	markers = []
	for i, row in enumerate(reader):
		if(i > 50):
			break
		toAppend = {}
		toAppend["marker"] = row[0]
		if(len(row) > 1):
			toAppend["category"] = row[1]
		else:
			toAppend["category"] = row[0]
		markers.append(toAppend)
	return markers

# Groups tweets by conversation numbers
def group(utterances):
	utterances.sort(key=operator.itemgetter('convId'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('convId')):
		list1.append(list(items))
	return list1

def tester(i):
	for i in range(0, 100000000):
		continue
	return

def parallelizer(function, args):
	with Pool(8) as p:
		return p.starmap(function, args)


# Computers the power probabilities
def metaDataExtractor(groupedUtterances, markers):
	results = []
	for i, convo in enumerate(groupedUtterances):
		#if(i % 1000 is 0):
		#	log("On " + str(i) + " of " + str(len(groupedUtterances)))
		userMarkers = {}
		intersect = {} # Number of times Person A and person B says the marker["marker"]
		base = {}
		notBNotA = {}
		notBA = {}
		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		numUtterances = len(convo) # Number of total utterances in the conversation
		if(a == b): # No self aligning stuff
			continue
		for utterance in convo:
			completedCategories = {}
			for j, marker in enumerate(markers):
				#print(marker["marker"] + " " + str(marker["marker"] is "want"))
			
				if(marker["category"] in completedCategories):
					continue

				# Increments values of userMarkers and intersect depending on whether a marker["marker"] is in the current utterance
				if marker["marker"] in utterance["msgMarkers"]:
					userMarkers[utterance["msgUserId"] + marker["category"]] = userMarkers.get(utterance["msgUserId"] + marker["category"] ,0) + 1#/(len(utterance["msgTokens"]))
				if marker["marker"] in utterance["replyMarkers"]:
					userMarkers[utterance["replyUserId"] + marker["category"]] = userMarkers.get(utterance["replyUserId"] + marker["category"] ,0) + 1#/len(utterance["replyTokens"])
				if marker["marker"] in utterance["msgMarkers"] and marker["marker"] in utterance["replyMarkers"]:
					intersect[marker["category"]] = intersect.get(marker["category"],0) + 1


				if (marker["marker"] in utterance["replyMarkers"]) and (marker["marker"] not in utterance["msgMarkers"]):
					base[marker["category"]] = base.get(marker["category"], 0) + 1



				if(marker["marker"] not in utterance["replyMarkers"] and marker["marker"] not in utterance["msgMarkers"]):
					notBNotA[marker["category"]] = notBNotA.get(marker["category"], 0) + 1
				if(marker["marker"] not in utterance["replyMarkers"] and marker["marker"] in utterance["msgMarkers"]):
					notBA[marker["category"]] = notBA.get(marker["category"], 0) + 1
				completedCategories[marker["category"]] = True
		convoUtterances = []
		for utterance in convo:
			convoUtterances.append(utterance["msg"])
			convoUtterances.append(utterance["reply"])
		toAppend = {"notBNotA": notBNotA, "notBA": notBA, "base": base, "utterances": convoUtterances, "numUtterances": numUtterances,  "intersect": intersect, "userMarkers": userMarkers, "a": a, "b": b, "conv": convo[0]["convId"]}
		if("verifiedSpeaker" in convo[0]):
			toAppend["verifiedSpeaker"] = bool(convo[0]["verifiedSpeaker"])
			toAppend["verifiedReplier"] = bool(convo[0]["verifiedReplier"])
			toAppend["replySentiment"] = utterance["replySentiment"]
			toAppend["msgSentiment"] = utterance["msgSentiment"]
		else:
			toAppend["corpus"] = utterance["corpus"]
			toAppend["docId"] = utterance["docId"]

		results.append(toAppend)
	return results



def allMarkers(markers):
	categories = []
	for marker in markers:
		categories.append(marker["category"])

	return list(set(categories))

# Formula = (utterances that A and B have said with the marker)/(utterances that A has said with marker) - (utterances B has said with marker)/(total utterances)
def calculateAlignment(results, markers, sparsities, age, gender):
	toReturn = []
	categories = allMarkers(markers)
	for i, result in enumerate(results):
		#if(i % 1000 is 0):
		#	log("On result " + str(i) + " of " + str(len(results)))

		for j, category in enumerate(categories):
				
			
			if((result["a"]+category) not in result["userMarkers"]):
				continue
			powerNum = float(result["intersect"].get(category, 0))
			
			powerDenom = float(result["userMarkers"][result["a"]+category])
			if(powerDenom == 0):
				continue
			
			

			baseDenom = result["numUtterances"]-float(result["userMarkers"][result["a"]+category])
			baseNum = float(result["base"].get(category, 0))
			if(baseDenom == 0):
				continue
			if(result["a"]+category == "15923226want"):
				log(result["userMarkers"])
				log(powerNum)
				log(baseNum)
				log(float(result["base"].get(category, 0)))
				#raise ValueError('test')
			if(baseNum == 0 and powerNum == 0):
				continue
			sparsity = sparsities[(result["a"], result["b"])]

			toAppend = {}
			toAppend["B&A"] = float(result["intersect"].get(category, 0))
			toAppend["B&NotA"] = float(result["base"].get(category, 0))
			#log(toAppend["B&NotBA"])
			toAppend["NotBNotA"] = float(result["notBNotA"].get(category, 0))
			toAppend["NotBA"] = float(result["notBA"].get(category, 0))

			
			toAppend["conv"] = result["conv"]
			toAppend["speakerId"] = result["a"]
			toAppend["replierId"] = result["b"]
			toAppend["category"] = category
			toAppend["numUtterances"] = result["numUtterances"]
			toAppend["sparsityA"] = sparsity[0]
			toAppend["sparsityB"] = sparsity[1]

			
			
			if("verifiedSpeaker" in result):
				toAppend["verifiedSpeaker"] = result["verifiedSpeaker"]
				toAppend["verifiedReplier"] = result["verifiedReplier"]
				toAppend["msgSentiment"] = result["msgSentiment"]
				toAppend["replySentiment"] = result["replySentiment"]
			else:
				toAppend["age"] = age
				toAppend["gender"] = gender
				toAppend["corpus"] = result["corpus"]
				toAppend["docId"] = result["docId"]

			smoothings = [1, 0.000001, 0.001]
			for smoothing in smoothings:
				powerProb = math.log((powerNum+smoothing)/(powerDenom+2*smoothing))
				baseProb = math.log((baseNum+smoothing)/(baseDenom+2*smoothing))
				
				alignment = powerProb - baseProb

				
				toAppend["alignment"+str(smoothing)] = alignment
				toAppend["powerNum"+str(smoothing)] = float(result["intersect"].get(category, 0))
				toAppend["powerDenom"+str(smoothing)] = float(result["userMarkers"][result["a"]+category])
				toAppend["baseNum"+str(smoothing)] = baseNum
				toAppend["baseDenom"+str(smoothing)] = baseDenom
				
				
			toReturn.append(toAppend)
	toReturn = sorted(toReturn, key=lambda k: -k["alignment1"])
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
	maxPower = results[1]
	maxConvo = findConvo(maxPower[0], groupedUtterances)
	leastPower = results[len(results)-1]
	leastConvo = findConvo(leastPower[0], groupedUtterances)
	log("Max Alignment: " + str(maxPower))
	log("Min Alignment: " + str(leastPower))

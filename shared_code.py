import csv
import operator
import itertools
import math
import logger

def ngrams(input, n):
  output = []
  for i in range(len(input)-n+1):
    output.append(tuple(input[i:i+n]))
  return output

def calculateAlignments(utterances, markers, smoothing, formulaType, outputFile, shouldWriteHeader):
	groupedUtterances = group(utterances)
	sparsities = calculateSparsity(groupedUtterances)
	metaData = metaDataExtractor(groupedUtterances, markers)
	results = runFormula(metaData, markers, sparsities, smoothing, formulaType)
	writeFile(results, outputFile, shouldWriteHeader)
	return results

# Groups tweets by conversation numbers
def group(utterances):
	utterances.sort(key=operator.itemgetter('convId'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('convId')):
		list1.append(list(items))
	return list1

def calculateSparsity(groupedUtterances): # calculates number of words speaker has said to replier/replier to speaker total
	sparsity_measure = {}
	for convo in groupedUtterances:
		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		sparsity_measure[(a, b)] = [0, 0]
		for utterance in convo:
			sparsity_measure[(a, b)] = [sparsity_measure[(a, b)][0] + len(utterance["msgTokens"]), sparsity_measure[(a, b)][1] + len(utterance["replyTokens"])]
	return sparsity_measure

# Computers the power probabilities
def metaDataExtractor(groupedUtterances, markers):
	results = []
	for i, convo in enumerate(groupedUtterances):
		if(i % 1000 is 0):
			logger.log("On " + str(i) + " of " + str(len(groupedUtterances)))
		userMarkers = {}
		intersect = {} # Number of times Person A and person B says the marker["marker"]
		base = {}
		notBNotA = {}
		notBA = {}
		for marker in markers:
			intersect[marker["category"]] = 0
			base[marker["category"]] = 0
			notBNotA[marker["category"]] = 0
			notBA[marker["category"]] = 0
		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		numUtterances = len(convo) # Number of total utterances in the conversation
		convoUtterances = []
		for utterance in convo:
			maxNgram = 1
			ngramLengths = [2,3,4,5]
			ngramPercent = 0
			for ngramLength in ngramLengths:
				msgTrigrams = set(ngrams(utterance["msgTokens"], ngramLength))
				replyTrigrams = set(ngrams(utterance["replyTokens"], ngramLength))
				quoted = set(msgTrigrams).intersection(set(replyTrigrams))
				if len(quoted) == 0:
					maxNgram = ngramLength - 1
					ngramPercent = maxNgram/len(utterance["msgTokens"])
					break
			if(maxNgram == 1):
				ngramPercent = 0
			convoUtterances.append(utterance["msg"])
			convoUtterances.append(utterance["reply"])
			completedCategories = {}
			for j, marker in enumerate(markers):
				category = marker["category"]
				if(category in completedCategories):
					continue
				msgMarker = False
				replyMarker = False
				# Increments values of userMarkers and intersect depending on whether a marker["marker"] is in the current utterance
				if category in utterance["msgMarkers"]:
					msgMarker = True
					userMarkers[utterance["msgUserId"] + category] = userMarkers.get(utterance["msgUserId"] + category ,0) + 1
				if category in utterance["replyMarkers"]:
					replyMarker = True
					userMarkers[utterance["replyUserId"] + category] = userMarkers.get(utterance["replyUserId"] + category,0) + 1
				
				if msgMarker and replyMarker:
					intersect[category] += 1
				elif replyMarker and not msgMarker:
					base[category] += 1
				elif not replyMarker and msgMarker:
					notBNotA[category] += 1
				else:
					notBA[category] += 1
				completedCategories[category] = True
			
		toAppend = {}
		toAppend["ngramPercent"] = ngramPercent
		toAppend["maxNgram"] = maxNgram
		toAppend["notBNotA"] = notBNotA
		toAppend["notBA"] = notBA
		toAppend["base"] = base
		toAppend["utterances"] = convoUtterances
		toAppend["numUtterances"] = numUtterances
		toAppend["intersect"] = intersect
		toAppend["userMarkers"] = userMarkers
		toAppend["a"] = a
		toAppend["b"] = b
		toAppend["conv"] =convo[0]["convId"]

		if("verifiedSpeaker" in convo[0]):
			toAppend["verifiedSpeaker"] = bool(convo[0]["verifiedSpeaker"])
			toAppend["verifiedReplier"] = bool(convo[0]["verifiedReplier"])
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
def runFormula(results, markers, sparsities, smoothing, formula):
	toReturn = []
	categories = allMarkers(markers)
	for i, result in enumerate(results):
		if(i % 1000 is 0):
			logger.log("On result " + str(i) + " of " + str(len(results)))
		for j, category in enumerate(categories):
			if((result["a"]+category) not in result["userMarkers"]):
				continue
			sparsity = sparsities[(result["a"], result["b"])]
			toAppend = {}
			toAppend["BAndA"] = float(result["intersect"].get(category, 0))
			toAppend["BAndNotA"] = float(result["base"].get(category, 0))
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
				toAppend["maxNgram"] = result["maxNgram"]
				toAppend["ngramPercent"] = result["ngramPercent"]
			else:
				toAppend["age"] = result["age"]
				toAppend["gender"] = result["gender"]
				toAppend["corpus"] = result["corpus"]
				toAppend["docId"] = result["docId"]
			if(formula == "DNM"):
				powerNum = toAppend["BAndA"]
				powerDenom = (toAppend["BAndA"]+toAppend["NotBA"])
				baseNum = (toAppend["BAndA"]+toAppend["BAndNotA"])
				baseDenom = toAppend["numUtterances"]
				if(baseDenom == 0 or powerDenom == 0):
					continue
				alignment = powerNum/powerDenom - baseNum/baseDenom
				toAppend["alignment"] = alignment
				toAppend["powerNum"] = powerNum
				toAppend["powerDenom"] = powerDenom
				toAppend["baseNum"] = baseNum
				toAppend["baseDenom"] = baseDenom
			elif(formula == "TRUE_POWER"):
				powerNum = float(result["intersect"].get(category, 0))
				powerDenom = float(result["userMarkers"][result["a"]+category])
				if(powerDenom == 0):
					continue
				baseDenom = result["numUtterances"]-float(result["userMarkers"][result["a"]+category])
				baseNum = float(result["base"].get(category, 0))
				if(baseDenom == 0):
					continue
				if(baseNum == 0 and powerNum == 0):
					continue
				powerProb = math.log((powerNum+smoothing)/(powerDenom+2*smoothing))
				baseProb = math.log((baseNum+smoothing)/(baseDenom+2*smoothing))
				alignment = powerProb - baseProb
				toAppend["alignment"] = alignment
				toAppend["powerNum"] = powerNum
				toAppend["powerDenom"] = powerDenom
				toAppend["baseNum"] = baseNum
				toAppend["baseDenom"] = baseDenom
			toReturn.append(toAppend)
	toReturn = sorted(toReturn, key=lambda k: -k["alignment"])
	return toReturn

# Writes stuff to the output file
def writeFile(results, outputFile, shouldWriteHeader):
<<<<<<< HEAD
=======
	header = sorted(list(results[0].keys()))
>>>>>>> 08feeed932d6d12efb2195a6929e08df274f331f
	toWrite = []
	try:
		header = list(results[0].keys())
		for row in results:
			toAppend = []
			for key in header:
				toAppend.append(row[key])
			toWrite.append(toAppend)
		if shouldWriteHeader:
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

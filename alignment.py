import csv
import operator
import itertools
import math
import logger1

def ngrams(input, n):
  output = []
  for i in range(len(input)-n+1):
    output.append(tuple(input[i:i+n]))
  return output

def calculateAlignments(utterances, markers, smoothing, outputFile, shouldWriteHeader, extras):
	markers = checkMarkers(markers)
	groupedUtterances = group(utterances)
	#sparsities = calculateSparsity(groupedUtterances)
	results = metaDataExtractor(groupedUtterances, markers, extras)
	#results = runFormula(metaData, markers, sparsities, smoothing, extras)
	writeFile(results, outputFile, shouldWriteHeader)
	#return results

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

def bayesCalc(aTokens, bTokens, marker):
	toReturn = {"ba": 0, "nba": 0, "bna": 0, "nbna": 0}
	if(marker in aTokens and marker in bTokens):
		toReturn["ba"] = 1
	elif(marker in aTokens and marker not in bTokens):
		toReturn["nba"] = 1
	elif(marker not in aTokens and marker in bTokens):
		toReturn["bna"] = 1
	else:
		toReturn["nbna"] = 1

	return toReturn


# Computers the power probabilities
def metaDataExtractor(groupedUtterances, markers, extras):
	results = []
	recips = {}
	for i, convo in enumerate(groupedUtterances):
		alignments = []
		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		numUtterances = len(convo) # Number of total utterances in the conversation
		for category in markers:
			marker = category["marker"]
			ba = 0.0
			nba = 0.0
			bna = 0.0
			nbna = 0.0
			numMarkers = 0
			for utterance in convo:
				msgTokens = utterance["msgTokens"]
				replyTokens = utterance["replyTokens"]
				msgMarkers = utterance["msgMarkers"]
				replyMarkers = utterance["replyMarkers"]
				if(utterance["msgUserId"] == a):
					aTokens = utterance["msgTokens"]
					bTokens = utterance["replyTokens"]
				else:
					aTokens = utterance["replyTokens"]
					bTokens = utterance["msgTokens"]
				result = bayesCalc(aTokens, bTokens, marker)
				ba += (result["ba"])
				nba += (result["nba"])
				bna += (result["bna"])
				nbna += (result["nbna"])

				if(marker in msgTokens):
					numMarkers += 1
				elif(marker in replyTokens):
					numMarkers += 1
			
			if((ba+nba) == 0): # A never says the marker
				continue
			if((bna+nbna) == 0): # A always says the marker 
				continue


			alignment = ba/(ba+nba) - bna/(bna+nbna)
			toAppend = {}
			toAppend["verifiedSpeaker"] = bool(convo[0]["verifiedSpeaker"])
			toAppend["verifiedReplier"] = bool(convo[0]["verifiedReplier"])
			toAppend["alignment"] = alignment
			toAppend["category"] = marker
			toAppend["msgUserId"] = a
			toAppend["replyUserId"] = b
			toAppend["ba"] = ba
			toAppend["nba"] = nba
			toAppend["bna"] = bna
			toAppend["nbna"] = nbna
			toAppend["numMarkers"] = numMarkers
			if((b,a) in recips):
				recips[(b,a)].append(toAppend)
			else:
				recips[(a,b)] = [toAppend]
	for key in recips:
		value = recips[key]
		if(len(value) < 2):
			continue
		toAppend = {}
		toAppend["verifiedSpeaker"] = bool(value[0]["verifiedSpeaker"])
		toAppend["verifiedReplier"] = bool(value[0]["verifiedReplier"])
		toAppend["alignment1"] = value[0]["alignment"]
		toAppend["alignment2"] = value[1]["alignment"]
		toAppend["category"] = value[0]["category"]
		toAppend["msgUserId"] = value[0]["msgUserId"]
		toAppend["replyUserId"] = value[0]["replyUserId"]
		if toAppend["alignment1"] + toAppend["alignment2"] == 0:
			continue
		logger1.log(toAppend["alignment1"]/(abs(toAppend["alignment1"])+abs(toAppend["alignment2"])))
		toAppend["alignment"] = toAppend["alignment1"]/(abs(toAppend["alignment1"])+abs(toAppend["alignment2"]))
		results.append(toAppend)
	return results












def allMarkers(markers):
	categories = []
	for marker in markers:
		categories.append(marker["category"])
	return list(set(categories))

# Formula = (utterances that A and B have said with the marker)/(utterances that A has said with marker) - (utterances B has said with marker)/(total utterances)
def runFormula(results, markers, sparsities, smoothing, extras):
	toReturn = []
	categories = allMarkers(markers)
	for i, result in enumerate(results):
		if(i % 1000 is 0):
			logger1.log("On result " + str(i) + " of " + str(len(results)))
		for j, category in enumerate(categories):
			if((result["a"]+category) not in result["userMarkers"]):
				continue
			sparsity = sparsities[(result["a"], result["b"])]
			toAppend = {}
			toAppend["ba"] = float(result["ba"].get(category, 0))
			toAppend["bna"] = float(result["bna"].get(category, 0))
			toAppend["nbna"] = float(result["nbna"].get(category, 0))
			toAppend["nba"] = float(result["nba"].get(category, 0))
			toAppend["speakerId"] = result["a"]
			toAppend["replierId"] = result["b"]
			toAppend["category"] = category
			toAppend["numUtterances"] = result["numUtterances"]
			
			if("verifiedSpeaker" in result):
				toAppend["verifiedSpeaker"] = result["verifiedSpeaker"]
				toAppend["verifiedReplier"] = result["verifiedReplier"]
				
				toAppend["speakerFollowers"] = result["speakerFollowers"]
				toAppend["replierFollowers"] = result["replierFollowers"]
				if("reciprocity" in result):
					toAppend["reciprocity"] = result["reciprocity"]
					toAppend["msgSentiment"] = result["msgSentiment"]
					toAppend['replySentiment'] = result["replySentiment"]
					toAppend["ngramPercent"] = result["ngramPercent"]
			else:
				toAppend["corpus"] = result["corpus"]
				toAppend["docId"] = result["docId"]
				toAppend["sparsityA"] = sparsity[0]
				toAppend["sparsityB"] = sparsity[1]
			
			powerNum = toAppend["ba"]
			powerDenom = (toAppend["ba"]+toAppend["nba"])
			baseNum = (toAppend["ba"]+toAppend["bna"])
			baseDenom = toAppend["numUtterances"]

			if(powerDenom != 0 and baseDenom != 0):
				dnmalignment = powerNum/powerDenom - baseNum/baseDenom
				toAppend["dnmalignment"] = dnmalignment
			else:
				toAppend["dnmalignment"] = False




			powerNum = toAppend["ba"]
			powerDenom = toAppend["ba"]+toAppend["nba"]
			baseDenom = toAppend["bna"]+toAppend["nbna"]
			baseNum = toAppend["bna"]
			powerProb = math.log(float((powerNum+smoothing)/(powerDenom+2*smoothing)))
			baseProb = math.log(float((baseNum+smoothing)/(baseDenom+2*smoothing)))
			alignment = powerProb - baseProb
			toAppend["alignment"] = alignment

			toReturn.append(toAppend)
	toReturn = sorted(toReturn, key=lambda k: -k["alignment"])
	return toReturn

# Writes stuff to the output file
def writeFile(results, outputFile, shouldWriteHeader):
	logger1.log("Writing to " + outputFile)
	if len(results) == 0:
		logger1.log("No results to write =(")
		return
	toWrite = []
	header = sorted(list(results[0].keys()))
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

def checkMarkers(markers):
	toReturn = []
	for marker in markers:
		if isinstance(marker, str):
			toReturn.append({"marker": marker, "category": marker})
		toReturn.append(marker)
	return toReturn
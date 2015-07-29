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

def calculateAlignments(utterances, markers, smoothing, outputFile, shouldWriteHeader):
	markers = checkMarkers(markers)
	groupedUtterances = group(utterances)
	sparsities = calculateSparsity(groupedUtterances)
	metaData = metaDataExtractor(groupedUtterances, markers)
	results = runFormula(metaData, markers, sparsities, smoothing)
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
	percentages = []
	for i, convo in enumerate(groupedUtterances):
		if(i % 1000 is 0):
			logger1.log("On " + str(i) + " of " + str(len(groupedUtterances)))
		userMarkers = {}
		ba = {} # Number of times Person A and person B says the marker["marker"]
		bna = {}
		nbna = {}
		nba = {}

		for marker in markers:
			ba[marker["category"]] = 0
			bna[marker["category"]] = 0
			nbna[marker["category"]] = 0
			nba[marker["category"]] = 0

		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		numUtterances = len(convo) # Number of total utterances in the conversation
		convoUtterances = []
		loveMsg = 0
		for utterance in convo:
			if("love" in utterance["msgMarkers"]):
				loveMsg += 1
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
				# Increments values of userMarkers and ba depending on whether a marker["marker"] is in the current utterance
				if category in utterance["msgMarkers"]:
					msgMarker = True
					userMarkers[utterance["msgUserId"] + category] = userMarkers.get(utterance["msgUserId"] + category ,0) + 1
				if category in utterance["replyMarkers"]:
					replyMarker = True
					userMarkers[utterance["replyUserId"] + category] = userMarkers.get(utterance["replyUserId"] + category,0) + 1
				
				if msgMarker and replyMarker:
					ba[category] += 1
				elif replyMarker and not msgMarker:
					bna[category] += 1
				elif not replyMarker and msgMarker:
					nba[category] += 1
				else:
					nbna[category] += 1
				completedCategories[category] = True
		toAppend = {}
		toAppend["ngramPercent"] = ngramPercent
		toAppend["maxNgram"] = maxNgram
		toAppend["nbna"] = nbna
		toAppend["nba"] = nba
		toAppend["bna"] = bna
		toAppend["utterances"] = convoUtterances
		toAppend["numUtterances"] = numUtterances
		toAppend["ba"] = ba
		toAppend["userMarkers"] = userMarkers
		toAppend["a"] = a
		toAppend["b"] = b
		toAppend["conv"] =convo[0]["convId"]
		
		toAppend["lovePercent"] = loveMsg/float(numUtterances)

		if("verifiedSpeaker" in convo[0]):
			toAppend["reciprocity"] = convo[0]["reciprocity"]
			toAppend["verifiedSpeaker"] = bool(convo[0]["verifiedSpeaker"])
			toAppend["verifiedReplier"] = bool(convo[0]["verifiedReplier"])
			toAppend["speakerFollowers"] = convo[0]["speakerFollowers"]
			toAppend["replierFollowers"] = convo[0]["replierFollowers"]
			if((convo[0]["replierFollowers"] + convo[0]["speakerFollowers"] > 0) and (convo[0]["speakerFollowers"] != 0) and (convo[0]["replierFollowers"] > 0)):
				toAppend["percentDiff"] = convo[0]["speakerFollowers"]/(convo[0]["replierFollowers"] + convo[0]["speakerFollowers"])
			
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
def runFormula(results, markers, sparsities, smoothing):
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
				toAppend["reciprocity"] = result["reciprocity"]
				toAppend["verifiedSpeaker"] = result["verifiedSpeaker"]
				toAppend["verifiedReplier"] = result["verifiedReplier"]
				toAppend["speakerFollowers"] = result["speakerFollowers"]
				toAppend["replierFollowers"] = result["replierFollowers"]
				if("percentDiff" in result):
					toAppend["percentDiff"] = result["percentDiff"]
				else:
					continue
			else:
				toAppend["corpus"] = result["corpus"]
				toAppend["docId"] = result["docId"]
				toAppend["sparsityA"] = sparsity[0]
				toAppend["sparsityB"] = sparsity[1]
			
			powerNum = toAppend["ba"]
			powerDenom = (toAppend["ba"]+toAppend["nba"])
			baseNum = (toAppend["ba"]+toAppend["bna"])
			baseDenom = toAppend["numUtterances"]
			if(baseDenom == 0 or powerDenom == 0):
				continue
			toAppend["bMarkerPercent"] = (toAppend["ba"] + toAppend["bna"])/(toAppend["ba"] + toAppend["bna"] + toAppend["nba"]+ toAppend["nbna"])
			toAppend["aMarkerPercent"] = powerDenom/(baseDenom+powerDenom)
			alignment = powerNum/powerDenom - baseNum/baseDenom

			toAppend["alignment"] = alignment
			toAppend["baseDenom"] = baseDenom
			toAppend["powerDenom"] = powerDenom
			if(powerNum != 0 and baseNum != 0):
				toAppend["logdnmalignment"] = math.log(float(powerNum)/float(powerDenom)) - math.log(float(baseNum)/float(baseDenom))
			else:
				toAppend["logdnmalignment"] = False
			toAppend["dnmalignment"] = alignment

			powerNum = toAppend["ba"]
			powerDenom = float(result["userMarkers"][result["a"]+category])
			if(powerDenom == 0):
				continue
			baseDenom = toAppend["numUtterances"]-float(result["userMarkers"][result["a"]+category])
			baseNum = toAppend["bna"]
			if(baseDenom == 0):
				continue
			if(baseNum == 0 or powerNum == 0):
				continue
			powerProb = math.log(float((powerNum+smoothing)/(powerDenom+2*smoothing)))
			baseProb = math.log(float((baseNum+smoothing)/(baseDenom+2*smoothing)))
			alignment = powerProb - baseProb
			toAppend["alignment"] = alignment

			powerProb = ((powerNum+smoothing)/(powerDenom+2*smoothing))
			baseProb = ((baseNum+smoothing)/(baseDenom+2*smoothing))
			alignment = powerProb - baseProb
			toAppend["noLogAlign"] = alignment

			toReturn.append(toAppend)
	toReturn = sorted(toReturn, key=lambda k: -k["alignment"])
	return toReturn

# Writes stuff to the output file
def writeFile(results, outputFile, shouldWriteHeader):
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

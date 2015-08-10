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
	aList = []
	naList = []
	aMarkerCount = 0.0
	for i, aToken in enumerate(aTokens):
		if(marker == aToken):
			aMarkerCount += 1
		aList.append(aMarkerCount/(i+1))
		naList.append((i+1-aMarkerCount)/(i+1))
	a = sum(aList)/len(aList)
	na = sum(naList)/len(naList)

	baList = []
	nbaList = []
	bnaList = []
	nbnaList = []

	bMarkerCount = 0.0
	for i, bToken in enumerate(bTokens):
		if(marker == bToken):
			bMarkerCount += 1
		baList.append(a*bMarkerCount/(i+1))
		nbaList.append(a*(i+1-bMarkerCount)/(i+1))
		bnaList.append(na*bMarkerCount/(i+1))
		nbnaList.append(na*(i+1-bMarkerCount)/(i+1))

	ba = sum(baList)/len(baList)
	nba = sum(nbaList)/len(nbaList)
	bna = sum(bnaList)/len(bnaList)
	nbna = sum(nbnaList)/len(nbnaList)

	return {"ba": ba, "nba": nba, "bna": bna, "nbna": nbna}


# Computers the power probabilities
def metaDataExtractor(groupedUtterances, markers, extras):
	results = []
	for i, convo in enumerate(groupedUtterances):
		alignments = []
		a = convo[0]["msgUserId"] # Id of person A
		b = convo[0]["replyUserId"] # Id of person B
		numUtterances = len(convo) # Number of total utterances in the conversation
		for category in markers:
			marker = category["marker"]
			baList = []
			nbaList = []
			bnaList = []
			nbnaList = []
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
				baList.append(result["ba"])
				nbaList.append(result["nba"])
				bnaList.append(result["bna"])
				nbnaList.append(result["nbna"])

				if(marker in msgTokens):
					numMarkers += 1
				elif(marker in replyTokens):
					numMarkers += 1
			

			ba = sum(baList)/len(baList)
			nba = sum(nbaList)/len(nbaList)
			bna = sum(bnaList)/len(bnaList)
			nbna = sum(nbnaList)/len(nbnaList)

			

			if(len(marker) > 1 and (ba+nba) == 0 and (ba+bna) != 0):
				print("After loop")
				for utterance in convo:
					print(utterance["msg"])
					print(utterance["reply"])
					print("-----")
				print(nbaList)
				print("Message")
				print(marker)
				print("-----------")

			
			if((ba+nba) == 0): # A never says the marker
				continue
			if((ba+bna) == 0): # B never says the marker
				continue


			aInfb = abs(ba/(ba+nba) - bna/(bna+nbna))
			bInfa = abs(ba/(ba+bna) - nba/(nba+nbna))

			#if(aInfb == 0): # A does not influence B
			#	aInfb = 0
			#else:
			#	aInfb = math.log(aInfb)

			#if(bInfa == 0): # B does not influence A
			#	bInfa = 0
			#else:
			#	bInfa = math.log(bInfa)

			if(aInfb + bInfa == 0):
				alignment = 0
			else:
				alignment = (aInfb - bInfa)/((aInfb + bInfa)/2)
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
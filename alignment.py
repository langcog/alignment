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
	metaData = metaDataExtractor(groupedUtterances, markers, extras)
	results = runFormula(metaData, markers, smoothing, extras)
	writeFile(results, outputFile, shouldWriteHeader)
	return results

# Groups tweets by conversation numbers
def group(utterances):
	utterances.sort(key=operator.itemgetter('convId'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('convId')):
		list1.append(list(items))
	return list1

#def calculateSparsity(groupedUtterances): # calculates number of words speaker has said to replier/replier to speaker total
#	sparsity_measure = {}
#	for convo in groupedUtterances:
#		a = convo[0]["msgUserId"] # Id of person A
#		b = convo[0]["replyUserId"] # Id of person B
#		sparsity_measure[(a, b)] = [0, 0]
#		for utterance in convo:
#			sparsity_measure[(a, b)] = [sparsity_measure[(a, b)][0] + len(utterance["msgTokens"]), sparsity_measure[(a, b)][1] + len(utterance["replyTokens"])]
#	return sparsity_measure

# Computers the power probabilities
def metaDataExtractor(groupedUtterances, markers, extras):
	results = []
	if("positives" in extras):
		positives = extras["positives"]
		negatives = extras["negatives"]
	for i, convo in enumerate(groupedUtterances):
		if(i % 1000 is 0):
			logger1.log("On " + str(i) + " of " + str(len(groupedUtterances)))
		userMarkers = {}
		ba = {} # Number of times Person A and person B says the marker["marker"]
		bna = {}
		nbna = {}
		nba = {}

		#for marker in markers:
		#	ba[marker["category"]] = 0
		#	bna[marker["category"]] = 0
		#	nbna[marker["category"]] = 0
		#	nba[marker["category"]] = 0

		#numUtterances = len(convo) # Number of total utterances in the conversation
		#convoUtterances = []
		#averageReplySentiment = 0
		#averageMessageSeniment = 0
		for utterance in convo:
			#if("positives" in extras):
			#	msgSentiment = 0
			#	for token in utterance["msgTokens"]:
			#		if(token in positives):
			#			msgSentiment += 1
			#		elif token in negatives:
			#			msgSentiment -= 1
                  #
			#	replySentiment = 0
			#	for token in utterance["replyTokens"]:
			#		if(token in positives):
			#			replySentiment += 1
			#		elif token in negatives:
			#			replySentiment -= 1
                  #
			#	averageMessageSeniment += msgSentiment
			#	averageReplySentiment += replySentiment
				
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
					ba[category] = ba.get(category,0) + 1
				elif replyMarker and not msgMarker:
					bna[category] = bna.get(category,0) + 1
				elif not replyMarker and msgMarker:
					nba[category] = nba.get(category,0) + 1
				else:
					nbna[category] = nbna.get(category,0) + 1
				completedCategories[category] = True
		toAppend = {}
		toAppend["nbna"] = nbna
		toAppend["nba"] = nba
		toAppend["bna"] = bna
		#toAppend["utterances"] = convoUtterances
		#toAppend["numUtterances"] = numUtterances
		toAppend["ba"] = ba
		#toAppend["userMarkers"] = userMarkers
		toAppend["msgUserId"] = utterance["msgUserId"]
		toAppend["replyUserId"] = utterance["replyUserId"]
		toAppend["conv"] = utterance["convId"]
		toAppend["reciprocity"] = utterance["reciprocity"]


		if("verifiedSpeaker" in convo[0]):
			#if("positives" in extras):
			#	if(i%1000 == 0):
			#		logger1.log("Adding sentiment")
			#	toAppend["msgSentiment"] = averageMessageSeniment/numUtterances
			#	toAppend["replySentiment"] = averageReplySentiment/numUtterances
			#	toAppend["ngramPercent"] = ngramPercent

			toAppend["verifiedSpeaker"] = bool(convo[0]["verifiedSpeaker"])
			toAppend["verifiedReplier"] = bool(convo[0]["verifiedReplier"])
			toAppend["speakerFollowers"] = convo[0]["speakerFollowers"]
			toAppend["replierFollowers"] = convo[0]["replierFollowers"]
			#if((convo[0]["replierFollowers"] + convo[0]["speakerFollowers"] > 0) and (convo[0]["speakerFollowers"] != 0) and (convo[0]["replierFollowers"] > 0)):
			#	toAppend["percentDiff"] = convo[0]["speakerFollowers"]/(convo[0]["replierFollowers"] + convo[0]["speakerFollowers"])
			
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
def runFormula(results, markers, smoothing, extras):
	toReturn = []
	categories = allMarkers(markers)
	for i, result in enumerate(results):
		if(i % 1000 is 0):
			logger1.log("On result " + str(i) + " of " + str(len(results)))
		for j, category in enumerate(categories):
			#if((result["msgUserId"]+category) not in result["userMarkers"]):
			#	continue
			#sparsity = sparsities[(result["a"], result["b"])]
			toAppend = {}
			ba = int(result["ba"].get(category, 0))
			bna = int(result["bna"].get(category, 0))
			nbna = int(result["nbna"].get(category, 0))
			nba = int(result["nba"].get(category, 0))
			
			#Calculating alignment only makes sense if we've seen messages with and without the marker
			#if ((ba+nba)==0 or (bna+nbna)==0):
			#	continue
			toAppend["speakerId"] = result["msgUserId"]
			toAppend["replierId"] = result["replyUserId"]
			toAppend["category"] = category
			#toAppend["numUtterances"] = result["numUtterances"]
			
			if("reciprocity" in result):
				toAppend["reciprocity"] = result["reciprocity"]

			
			if("verifiedSpeaker" in result):
				toAppend["verifiedSpeaker"] = result["verifiedSpeaker"]
				toAppend["verifiedReplier"] = result["verifiedReplier"]
				
				toAppend["speakerFollowers"] = result["speakerFollowers"]
				toAppend["replierFollowers"] = result["replierFollowers"]
				#if("msgSentiment" in result):
				#	toAppend["msgSentiment"] = result["msgSentiment"]
				#	toAppend['replySentiment'] = result["replySentiment"]
				#	toAppend["ngramPercent"] = result["ngramPercent"]
			else:
				toAppend["corpus"] = result["corpus"]
				toAppend["docId"] = result["docId"]
				#toAppend["sparsityA"] = sparsity[0]
				#toAppend["sparsityB"] = sparsity[1]
			
			#Calculating Echoes of Power alignment 
			powerNum = ba
			powerDenom = ba+nba
			baseNum = ba+bna
			baseDenom = ba+nba+bna+nbna

			if(powerDenom != 0 and baseDenom != 0):
				dnmalignment = powerNum/powerDenom - baseNum/baseDenom
				toAppend["dnmalignment"] = dnmalignment
			else:
				toAppend["dnmalignment"] = False




			powerNum = ba
			powerDenom = ba+nba
			baseDenom = bna+nbna
			baseNum = bna
			powerProb = math.log(float((powerNum+smoothing)/(powerDenom+2*smoothing)))
			baseProb = math.log(float((baseNum+smoothing)/(baseDenom+2*smoothing)))
			alignment = powerProb - baseProb
			toAppend["alignment"] = alignment
			
			toAppend["ba"] = ba
			toAppend["bna"] = bna
			toAppend["nba"] = nba
			toAppend["nbna"] = nbna

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
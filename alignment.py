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
	groupedUtterances = group(utterances)
	results = metaDataExtractor(groupedUtterances, markers, extras)
	writeFile(results, outputFile, shouldWriteHeader)

# Groups tweets by conversation numbers
def group(utterances):
	utterances.sort(key=operator.itemgetter('convId'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('convId')):
		list1.append(list(items))
	return list1


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
	if("positives" in extras):
		positives = extras["positives"]
		negatives = extras["negatives"]
	for i, convo in enumerate(groupedUtterances):
		if(i%10000 == 0):
			logger1.log("On convo " + str(i) + " of " + str(len(groupedUtterances)))
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
			averageReplySentiment = 0
			averageMessageSeniment = 0
			for utterance in convo:
				if("positives" in extras):
					msgSentiment = 0
					for token in utterance["msgTokens"]:
						if(token in positives):
							msgSentiment += 1
						elif token in negatives:
							msgSentiment -= 1

					replySentiment = 0
					for token in utterance["replyTokens"]:
						if(token in positives):
							replySentiment += 1
						elif token in negatives:
							replySentiment -= 1

					if(utterance["msgUserId"] == a):
						averageMessageSeniment += msgSentiment
						averageReplySentiment += replySentiment
					else:
						averageReplySentiment += msgSentiment
						averageMsgSentiment += replySentiment

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

			

			if((ba+nba) == 0): # A never says the marker
				continue
			if((bna+nbna) == 0): # A always says the marker 
				continue


			alignment = ba/(ba+nba) - bna/(bna+nbna)
			toAppend = {}

			if("positives" in extras):
				toAppend["msgSentiment"] = averageMessageSeniment/numUtterances
				toAppend["replySentiment"] = averageReplySentiment/numUtterances
			toAppend["verifiedSpeaker"] = bool(convo[0]["verifiedSpeaker"])
			toAppend["verifiedReplier"] = bool(convo[0]["verifiedReplier"])
			toAppend["alignment"] = alignment
			toAppend["category"] = marker
			toAppend["msgUserId"] = a
			toAppend["replyUserId"] = b
			toAppend["msgScreenname"] = convo[0]["msgScreenname"]
			toAppend["msgScreenname"] = convo[0]["replyScreenname"]
			toAppend["ba"] = ba
			toAppend["nba"] = nba
			toAppend["bna"] = bna
			toAppend["nbna"] = nbna
			results.append(toAppend)
		
	return results


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
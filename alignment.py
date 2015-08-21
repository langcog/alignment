import csv
import operator
import itertools
import math
import logger1
from copy import copy

from pprint import pprint

def ngrams(input, n):
  output = []
  for i in range(len(input)-n+1):
    output.append(tuple(input[i:i+n]))
  return output

def calculateAlignments(utterances, markers, smoothing, outputFile, shouldWriteHeader, extras={}):
	markers = checkMarkers(markers)
	groupedUtterances = group(utterances)
	metaData = metaDataExtractor(groupedUtterances, markers)
	results = runFormula(metaData, markers, smoothing, extras)
	#writeFile(results, outputFile, shouldWriteHeader)
	return results

# Groups tweets by conversation ids
def group(utterances):
	utterances.sort(key=operator.itemgetter('convId'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('convId')):
		list1.append(list(items))
	return list1

def makeMarkerDict(markers):
	mdict = {}
	for m in markers:
		mdict[m["marker"]] = m["category"]
	return(mdict)

def makeMarkerDict2(markers):
	mdict = {}
	for m in markers:
		mdict[m["category"]] = mdict.get(m["category"],[]).append(m["marker"])
	return(mdict)

def findMarkerInMessage(marker,utterance):
	# Increments values of userMarkers and ba depending on whether a marker["marker"] is in the current utterance
	if marker in utterance["replyMarkers"]:
		result = 'b'
	else:
		result = 'nb'

	if marker in utterance["msgMarkers"]:
		result += 'a'
	else:
		result += 'na'
	
	return(result)

def findMarkersInMessage(d,markers,utterance):
	for j,marker in enumerate(markers.keys()):
		condition = findMarkerInMessage(marker,utterance)
		d[condition][marker] = d[condition].get(marker,0) + 1
	return(d)

def findMarkersInConvo(markers,convo):
	ba = {} # Number of times Person A and person B says the marker["marker"]
	bna = {}
	nbna = {}
	nba = {}
	for utterance in convo:				
		for j, marker in enumerate(markers):
			category = marker["category"]
			msgMarker = category in utterance["msgMarkers"]
			replyMarker = category in utterance["replyMarkers"]
			
			if msgMarker and replyMarker:
				ba[category] = ba.get(category,0) + 1
			elif replyMarker and not msgMarker:
				bna[category] = bna.get(category,0) + 1
			elif not replyMarker and msgMarker:
				nba[category] = nba.get(category,0) + 1
			else:
				nbna[category] = nbna.get(category,0) + 1
	return({'ba': ba,'bna': bna,'nba': nba,'nbna': nbna})

def addFeats(toAppend,utterance):
	toAppend["speakerId"] = utterance["msgUserId"]
	toAppend["replierId"] = utterance["replyUserId"]
	#toAppend["conv"] = utterance["convId"]
	toAppend["reciprocity"] = utterance["reciprocity"]
	if("verifiedSpeaker" in utterance):
		toAppend["verifiedSpeaker"] = bool(utterance["verifiedSpeaker"])
		toAppend["verifiedReplier"] = bool(utterance["verifiedReplier"])
		toAppend["speakerFollowers"] = utterance["speakerFollowers"]
		toAppend["replierFollowers"] = utterance["replierFollowers"]			
	else:
		toAppend["corpus"] = utterance["corpus"]
		toAppend["docId"] = utterance["docId"]
	return(toAppend)


# Computers the power probabilities
def metaDataExtractor(groupedUtterances, markers, extras={}):
	results = []
	#mdict = makeMarkerDict(markers)
	for i, convo in enumerate(groupedUtterances):
		if(i % 2500 is 0):
			logger1.log("On " + str(i) + " of " + str(len(groupedUtterances)))
				
		toAppend = findMarkersInConvo(markers,convo)		
		toAppend = addFeats(toAppend,convo[0])
		results.append(toAppend)
	return results

def allMarkers(markers):
	categories = []
	for marker in markers:
		categories.append(marker["category"])
	return list(set(categories))

#def temp2(category,result,smoothing):
#	toAppend = initializeAlignmentDict(result)
#	ba = int(result["ba"].get(category, 0))
#	bna = int(result["bna"].get(category, 0))
#	nbna = int(result["nbna"].get(category, 0))
#	nba = int(result["nba"].get(category, 0))
#	
#	#Calculating alignment only makes sense if we've seen messages with and without the marker
#	if (((ba+nba)==0 or (bna+nbna)==0)):
#		return(None)
#		
#	#Calculating Echoes of Power alignment 
#	powerNum = ba
#	powerDenom = ba+nba
#	baseNum = ba+bna
#	baseDenom = ba+nba+bna+nbna
#      
#	if(powerDenom != 0 and baseDenom != 0):
#		dnmalignment = powerNum/powerDenom - baseNum/baseDenom
#		toAppend["dnmalignment"] = dnmalignment
#	else:
#		raise NameError('DNM incalculable')
#	
#	#Calculating log-odds alignment
#	baseNum = bna
#	baseDenom = bna+nbna
#	powerProb = math.log(float((powerNum+smoothing)/(powerDenom+2*smoothing)))
#	baseProb = math.log(float((baseNum+smoothing)/(baseDenom+2*smoothing)))
#	toAppend["alignment"] = powerProb - baseProb
#	
#	toAppend["category"] = category
#	toAppend["ba"] = ba
#	toAppend["bna"] = bna
#	toAppend["nba"] = nba
#	toAppend["nbna"] = nbna
#	
#	return(toAppend)

def createAlignmentDict(category,result,smoothing):
	toAppend = {}
	ba = int(result["ba"].get(category, 0))
	bna = int(result["bna"].get(category, 0))
	nbna = int(result["nbna"].get(category, 0))
	nba = int(result["nba"].get(category, 0))
	
	#Calculating alignment only makes sense if we've seen messages with and without the marker
	if (((ba+nba)==0 or (bna+nbna)==0)):
		return(None)
	toAppend["speakerId"] = result["speakerId"]
	toAppend["replierId"] = result["replierId"]
	
	if("reciprocity" in result):
		toAppend["reciprocity"] = result["reciprocity"]
	if("verifiedSpeaker" in result):
		toAppend["verifiedSpeaker"] = result["verifiedSpeaker"]
		toAppend["verifiedReplier"] = result["verifiedReplier"]				
		toAppend["speakerFollowers"] = result["speakerFollowers"]
		toAppend["replierFollowers"] = result["replierFollowers"]
	else:
		toAppend["corpus"] = result["corpus"]
		toAppend["docId"] = result["docId"]
	
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
	return(toAppend)

#def initializeAlignmentDict(d):
#	return({key:d[key] for key in d.keys() if key not in ['ba','bna','nba','nbna']})

# Formula = (utterances that A and B have said with the marker)/(utterances that A has said with marker) - (utterances B has said with marker)/(total utterances)
def runFormula(results, markers, smoothing, extras):
	toReturn = []
	categories = allMarkers(markers)
	for i, result in enumerate(results):
		#d = initializeAlignmentDict(result)
		if(i % 1000 is 0):
			logger1.log("On result " + str(i) + " of " + str(len(results)))
		for j, category in enumerate(categories):
			toAppend = createAlignmentDict(category,result,smoothing)
			if toAppend is not None:
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
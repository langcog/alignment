import csv
import operator
import itertools
import math
import logger1

#main piece of code for calculating & wwriting alignments from processed data
def calculateAlignments(utterances, markers, smoothing, outputFile, shouldWriteHeader, extras={}):
	markers = checkMarkers(markers)
	groupedUtterances = group(utterances)
	metaData = metaDataExtractor(groupedUtterances, markers)          
	results = runFormula(metaData, markers, smoothing, extras)
	writeFile(results, outputFile, shouldWriteHeader)
	return results

# Groups tweets by conversation ids
def group(utterances):
	utterances.sort(key=operator.itemgetter('convId'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('convId')):
		list1.append(list(items))
	return list1

#code to revise marker list structure (not currently used)
def makeMarkerDict(markers):
	mdict = {}
	for m in markers:
		mdict[m["marker"]] = m["category"]
	return(mdict)

#alternate code to revise marker list structure (not currently used)
def makeMarkerDict2(markers):
	mdict = {}
	for m in markers:
		mdict[m["category"]] = mdict.get(m["category"],[]).append(m["marker"])
	return(mdict)

#Given a conversation & the list of markers, extract counts of speaker & replier using each marker 
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

#Copying portions of one dictionary to another (faster than copy(), if you can believe it!)
def addFeats(toAppend,utterance,renameIds=True):
	if renameIds:
		toAppend["speakerId"] = utterance["msgUserId"]
		toAppend["replierId"] = utterance["replyUserId"]
	else:
		toAppend["speakerId"] = utterance["speakerId"]
		toAppend["replierId"] = utterance["replierId"]		
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


# calculates the marker usage counts from conversations
def metaDataExtractor(groupedUtterances, markers, extras={}):
	results = []
	for i, convo in enumerate(groupedUtterances):
		if(i % 2500 is 0):
			logger1.log("On " + str(i) + " of " + str(len(groupedUtterances)))
				
		toAppend = findMarkersInConvo(markers,convo)		
		toAppend = addFeats(toAppend,convo[0])
		results.append(toAppend)
	return results

# extracts a list of markers from the marker dictionary
def allMarkers(markers):
	categories = []
	for marker in markers:
		categories.append(marker["category"])
	return list(set(categories))

# creates a dictionary corresponding to a single row of the final output (speaker-replier-marker triplet)
def createAlignmentDict(category,result,smoothing):
	toAppend = {}
	ba = int(result["ba"].get(category, 0))
	bna = int(result["bna"].get(category, 0))
	nbna = int(result["nbna"].get(category, 0))
	nba = int(result["nba"].get(category, 0))
	
	#Calculating alignment only makes sense if we've seen messages with and without the marker
	if (((ba+nba)==0 or (bna+nbna)==0)):
		return(None)
	
	toAppend = addFeats(toAppend,result,False)
		
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

# Gets us from the meta-data to the final output file
def runFormula(results, markers, smoothing, extras):
	toReturn = []
	categories = allMarkers(markers)
	for i, result in enumerate(results):
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

# checks & adapts the structure of the marker list to the appropriate one 
def checkMarkers(markers):
	toReturn = []
	for marker in markers:
		if isinstance(marker, str):
			toReturn.append({"marker": marker, "category": marker})
		toReturn.append(marker)
	return toReturn
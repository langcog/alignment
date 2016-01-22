import csv
import operator
import itertools
import math
import logger1
import re

#main piece of code for calculating & wwriting alignments from processed data
def calculateAlignments(utterances, markers, smoothing, outputFile, shouldWriteHeader, corpusType='CHILDES'):
	markers = checkMarkers(markers)
	groupedUtterances = group(utterances)
	metaData = metaDataExtractor(groupedUtterances,markers,corpusType)          
	results = runFormula(metaData, markers, smoothing,corpusType)
	writeFile(results, outputFile, shouldWriteHeader)
	return results

# Converts list of markers in a message to categories
def determineCategories(msgMarkers,catdict,useREs=False):
	msgCats = []
	#iterate over catdict items {category: [words/REs]}
	for cd in catdict.items():
		if useREs:
			if any(any(wordre.match(marker) for marker in msgMarkers) for wordre in cd[1]):	#if REs, see if any tokens match each RE
				msgCats.append(cd[0])
		else:
			if any(word in msgMarkers for word in cd[1]):			#if just words, see if any word in category also in msg
				msgCats.append(cd[0])
	return msgCats

# Groups tweets by conversation ids
def group(utterances):
	utterances.sort(key=operator.itemgetter('convId'))
	list1 = []
	for key, items in itertools.groupby(utterances, operator.itemgetter('convId')):
		list1.append(list(items))
	return list1

#code to convert marker list structure to {category: [words]} structure
def makeCatDict(markers,useREs=False):
	mdict = {}
	for m in markers:
		marker = re.compile(''.join([m["marker"], '$'])) if useREs else m["marker"]
		if m["category"] in mdict:
			mdict[m["category"]].append(marker)
		else:
			mdict[m["category"]] = [marker]
		#mdict[m["category"]] = mdict.get(m["category"],[]).append(m["marker"])	#Need to swap marker and category labels
		#mdict[m["marker"]] = mdict.get(m["marker"],[]).append(m["category"])
	return(mdict)

#Given a conversation & the list of markers, extract counts of speaker & replier using each marker 
def findMarkersInConvo(markers,convo):
	ba = {} # Number of times Person A and person B says the marker["marker"]
	bna = {}
	nbna = {}
	nba = {}
	for utterance in convo:				
		for j, marker in enumerate(markers):
			word = marker["marker"]
			msgMarker = word in utterance["msgMarkers"]
			replyMarker = word in utterance["replyMarkers"]
			
			if msgMarker and replyMarker:
				ba[word] = ba.get(word,0) + 1
			elif replyMarker and not msgMarker:
				bna[word] = bna.get(word,0) + 1
			elif not replyMarker and msgMarker:
				nba[word] = nba.get(word,0) + 1
			else:
				nbna[word] = nbna.get(word,0) + 1
	return({'ba': ba,'bna': bna,'nba': nba,'nbna': nbna})

#Copying portions of one dictionary to another (faster than copy(), if you can believe it!)
def addFeats(toAppend,utterance,renameIds=True,corpusType=''):
	if renameIds:
		toAppend["speakerId"] = utterance["msgUserId"]
		toAppend["replierId"] = utterance["replyUserId"]
	else:
		toAppend["speakerId"] = utterance["speakerId"]
		toAppend["replierId"] = utterance["replierId"]		
	if(corpusType=='Twitter'):
		toAppend["reciprocity"] = utterance["reciprocity"]
		toAppend["verifiedSpeaker"] = bool(utterance["verifiedSpeaker"])
		toAppend["verifiedReplier"] = bool(utterance["verifiedReplier"])
		toAppend["speakerFollowers"] = utterance["speakerFollowers"]
		toAppend["replierFollowers"] = utterance["replierFollowers"]			
	elif(corpusType=='CHILDES'):
		toAppend["corpus"] = utterance["corpus"]
		toAppend["docId"] = utterance["docId"]
	return(toAppend)


# calculates the marker usage counts from conversations
def metaDataExtractor(groupedUtterances, markers,corpusType=''):
	results = []
	for i, convo in enumerate(groupedUtterances):
		if(i % 2500 is 10):
			logger1.log("On " + str(i) + " of " + str(len(groupedUtterances)))
				
		toAppend = findMarkersInConvo(markers,convo)		
		toAppend = addFeats(toAppend,convo[0],True,corpusType)
		results.append(toAppend)
	return results

# extracts a list of markers from the marker dictionary
def allMarkers(markers):
	categories = []
	for marker in markers:
		categories.append(marker["marker"])
	return list(set(categories))

# creates a dictionary corresponding to a single row of the final output (speaker-replier-marker triplet)
def createAlignmentDict(category,result,smoothing,corpusType=''):
	toAppend = {}
	ba = int(result["ba"].get(category, 0))
	bna = int(result["bna"].get(category, 0))
	nbna = int(result["nbna"].get(category, 0))
	nba = int(result["nba"].get(category, 0))
	
	#Calculating alignment only makes sense if we've seen messages with and without the marker
	if (((ba+nba)==0 or (bna+nbna)==0)):
		return(None)
	
	toAppend = addFeats(toAppend,result,False,corpusType)
	toAppend["category"] = category
		
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
	powerProb = math.log((powerNum+smoothing)/float(powerDenom+2*smoothing))
	baseProb = math.log((baseNum+smoothing)/float(baseDenom+2*smoothing))
	alignment = powerProb - baseProb
	toAppend["alignment"] = alignment
	
	toAppend["ba"] = ba
	toAppend["bna"] = bna
	toAppend["nba"] = nba
	toAppend["nbna"] = nbna
	return(toAppend)

# Gets us from the meta-data to the final output file
def runFormula(results, markers, smoothing,corpusType):
	toReturn = []
	categories = allMarkers(markers)
	for i, result in enumerate(results):
		if(i % 1000 is 10):
			logger1.log("On result " + str(i) + " of " + str(len(results)))
		for j, category in enumerate(categories):
			toAppend = createAlignmentDict(category,result,smoothing,corpusType)
			if toAppend is not None:
				toReturn.append(toAppend)
	toReturn = sorted(toReturn, key=lambda k: (k["speakerId"],k["replierId"],k["category"]))
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
def readMarkers(markersFile,dialect=None):
	if dialect is None:
		reader = csv.reader(open(markersFile))
	else:
		reader = csv.reader(open(markersFile),dialect=dialect)
	markers = []
	#print('marker\tcategory')
	for i, row in enumerate(reader):
		toAppend = {}
		toAppend["marker"] = row[0]
		if(len(row) > 1):
			toAppend["category"] = row[1]
		else:
			toAppend["category"] = row[0]
		markers.append(toAppend)
		#print(toAppend["marker"]+'\t'+toAppend["category"])
	return markers

# checks & adapts the structure of the marker list to the appropriate one 
def checkMarkers(markers):
	toReturn = []
	for marker in markers:
		if isinstance(marker, str):
			toReturn.append({"marker": marker, "category": marker})
		else:
			toReturn.append(marker)
	return toReturn
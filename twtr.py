import csv
import operator
import itertools
import re
import traceback
import shared_code
import datetime
from ast import literal_eval
from pprint import pprint

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"
testOutputFile = "debug/test_results.csv"

inputFile = "data/pairedtweets2.txt"
markersFile = "wordlists/markers_worldenglish.csv"
outputFile = "debug/results.csv"

userFile = "data/pairedtweets.txt.userinfo"

markerFrequency = 0

def test(testFile, testMarkersFile, testOutputFile):
	markers = shared_code.readMarkers(testMarkersFile)
	result = readCSV(markers, testFile, False, [], [])
	utterances = result["utterances"]
	utterancesById = result["utterancesById"]
	groupedUtterances = shared_code.group(utterances)
	sparsities = shared_code.calculateSparsity(groupedUtterances)
	setUppedResults = shared_code.metaDataExtractor(groupedUtterances, markers)
	
	results = shared_code.calculateAlignment(setUppedResults, markers, sparsities, utterances, markerFrequency, utterancesById, 0, 0)
	
	header = [list(results[0].keys())]
	shared_code.writeFile(header, testOutputFile, "w")
	toWrite = []
	for row in results:
		toWrite.append(list(row.values()))
	shared_code.writeFile(toWrite, testOutputFile, "a")

	results.pop(0)
	results = sorted(results, key=lambda k: -k["alignment"])
	leastPower = results[len(results)-1]
	shared_code.log(leastPower)
	shared_code.log(leastPower["alignment"])
	if (abs(leastPower["alignment"] - -1.701344340796773) < 0.01):
		return True
	else:
		return False

def getCommonMarkers(utterances):
	freqs = {}
	for utterance in utterances:
		message = utterance["msg"].split(" ")
		for word in message:
			freqs[word] = freqs.get(word, 0) + 1
		reply = utterance["reply"].split(" ")
		for word in reply:
			freqs[word] = freqs.get(word, 0) + 1
	freqs = [(k, freqs[k]) for k in sorted(freqs, key=freqs.get, reverse=True)]
	toReturn = []
	shared_code.log(len(freqs))
	subset = freqs[0:50]
	
	for subsetTuple in subset:
		if(subsetTuple[0] == "[mention]" or subsetTuple[0] == "[url]"):
			continue
		toReturn.append({"marker": subsetTuple[0], "category": subsetTuple[0]})
	shared_code.log(toReturn)
	return toReturn

def findUser(users, uId):
	for user in users:
		if(user["uid"] == uId):
			return user
	return False

def findUser2(udict,uid):
	return udict.get(uid, False)

def makeUserDict(users):
	udict = {}
	for user in users:
		udict[user["uid"]] = user
	return udict

def makeMarkerDict(markers):
	mdict = {}
	for marker in markers:
		mdict[marker["marker"]] = marker
	return mdict

def transformCSV(markers, users, row):
	#utterancesById = {}
	#averageSentiment = 0
	
	#for i, row in enumerate(rows):
	#if(i % 1000 is 0):
	#	shared_code.log("On " + str(i) + " of " + str(len(rows))) 
	toAppend = {}
	toAppend["docId"] = "TWITTER"
	toAppend["corpus"] = "TWITTER"
	
	if(len(row) < 6):
		return
	if(int(row[6])%1000 == 0):

		shared_code.log("On " + str(row[6]))
	toAppend["convId"] = (row[1], row[4])
	toAppend["msgUserId"] = row[1]
	toAppend["msg"] = row[2].lower()
	toAppend["replyUserId"] = row[4]
	toAppend["reply"] = row[5].lower()
	toAppend["msgMarkers"] = []
	toAppend["replyMarkers"] = []
	toAppend["msgTokens"] = row[2].split(" ")
	toAppend["replyTokens"] = row[5].split(" ")
	#msgSentiment = 0
	#for token in toAppend["msgTokens"]:
	#	if(token in positives):
	#		msgSentiment += 1
	#	elif token in negatives:
	#		msgSentiment -= 1
	#toAppend["msgSentiment"] = msgSentiment

	#replySentiment = 0
	#for token in toAppend["replyTokens"]:
	#	if(token in positives):
	#		replySentiment += 1
	#	elif token in negatives:
	#		replySentiment -= 1
	#toAppend["replySentiment"] = replySentiment
	#if (replySentiment < 0):
	#	return
	#averageSentiment += replySentiment
	allTokens = []
	allTokens.append(toAppend["msgTokens"])
	allTokens.append(toAppend["replyTokens"])
	allTokens = [item for sublist in allTokens for item in sublist]
	duplicates = list(set(toAppend["msgTokens"]) & set(toAppend["replyTokens"]))
	#if(len(list(set(allTokens))) < 5):
	#	return
	if(users is not False):
		msgUser = findUser(users, row[1])
		if(msgUser != False):
			toAppend["verifiedSpeaker"] = msgUser["verified"]
		else:
			toAppend["verifiedSpeaker"] = False
		replyUser = findUser(users, row[4])
		if(replyUser != False):
			toAppend["verifiedReplier"] = replyUser["verified"]
		else:
			toAppend["verifiedReplier"] = False
	messages = toAppend["msg"].split(" ")
	replies = toAppend["reply"].split(" ")
	for marker in markers:
		if marker["marker"] in messages:
			toAppend["msgMarkers"].append(marker["marker"])
		if marker["marker"] in replies:
			toAppend["replyMarkers"].append(marker["marker"])
	toAppend["msgTokens"] = messages
	toAppend["replyTokens"] = replies
	return toAppend
		#userUtterances = utterancesById.get(toAppend["replyUserId"], [])
		#userUtterances.append(toAppend["reply"])
		#utterancesById[toAppend["replyUserId"]] = userUtterances
	#shared_code.log("averageSentiment: " + str(float(averageSentiment)/float(len(utterances))))
	#return utterances #{"utterances": utterances, "utterancesById": utterancesById}

def analyzeSentiment(tokens,sentiments):
	msgSentiment = 0
	for token in tokens:
		if(token in sentiments):
			msgSentiment += sentiments[token]
	return msgSentiment 

#testing whether a reply has enough words not in common with the original message to merit being treated as its own message
def testReplySimilarity(toAppend,cutoff=5):
	allTokens = []
	allTokens.append(toAppend["msgTokens"])
	allTokens.append(toAppend["replyTokens"])
	allTokens = [item for sublist in allTokens for item in sublist]
	duplicates = list(set(toAppend["msgTokens"]) & set(toAppend["replyTokens"]))
	if(len(list(set(allTokens))) < cutoff):
		return True					#if too few unique words, return True (i.e., too similar)
	return False					#if enough unique words, return False (i.e., not too similar)

#Processing the main information in a single row of the tweet TSV file & putting it into a dictionary
def processTweetCSVRow(row):
	toAppend = {}
	toAppend["docId"] = "TWITTER"
	toAppend["corpus"] = "TWITTER"
	toAppend["convId"] = (row[1], row[4])
	toAppend["msgUserId"] = row[1]
	toAppend["msg"] = row[2].lower()
	toAppend["replyUserId"] = row[4]
	toAppend["reply"] = row[5].lower()
	toAppend["msgMarkers"] = []
	toAppend["replyMarkers"] = []
	toAppend["msgTokens"] = toAppend["msg"].split(" ")
	toAppend["replyTokens"] = toAppend["reply"].split(" ")
	return toAppend

#Code to take in the user dictionary & a user ID and return if that user is verified
#	Note: users with missing data are considered unverified
def verifySpeaker(udict,uid):
	msgUser = findUser2(udict,uid)
	if(msgUser != False):
		return msgUser["verified"]
	else:
		return False

def countMarkers(toAppend,markers):
	toAppend["msgMarkers"] = []
	toAppend["replyMarkers"] = []
	for marker in markers:
		if marker["marker"] in toAppend["msgTokens"]:
			toAppend["msgMarkers"].append(marker["marker"])
		if marker["marker"] in toAppend["replyTokens"]:
			toAppend["replyMarkers"].append(marker["marker"])
	return toAppend

def countMarkers2(tokens,markers):
	return [val for val in tokens if val in markers.keys()]

def countMarkers3(tokens,markers):
	return list(set(tokens) & set(markers.keys()))

def compareCountMarkers(t,m,md):
	t2a = countMarkers2(t["msgTokens"],md)
	t3a = countMarkers3(t["msgTokens"],md)
	t2b = countMarkers2(t["replyTokens"],md)
	t3b = countMarkers3(t["replyTokens"],md)
	t = countMarkers(t,m)
	
	if set(t["msgMarkers"]) != set(t2a):
		shared_code.log("1 != 2")
	if set(t["replyMarkers"]) != set(t3b):
		shared_code.log("1 != 3")
	
	return
		

def transformCSVnonP(markers, users, sentiments, rows):
	utterancesById = {}
	utterances = []
	udict = makeUserDict(users)
	mdict = makeMarkerDict(markers)
	vcounts = {}
	#averageSentiment = 0
	
	for i, row in enumerate(rows):
		if(i % 10000 is 0):
			shared_code.log("On " + str(i) + " of " + str(len(rows))) 
		
		toAppend = processTweetCSVRow(row)
		
		toAppend["msgSentiment"] = analyzeSentiment(toAppend["msgTokens"],sentiments)
		toAppend["replySentiment"] = analyzeSentiment(toAppend["replyTokens"],sentiments)
		
		#if testReplySimilarity(toAppend):
		#	continue
		
		if(users is not False):
			toAppend["verifiedSpeaker"] = verifySpeaker(udict,row[1])
			toAppend["verifiedReplier"] = verifySpeaker(udict,row[4])
		
		#compareCountMarkers(toAppend,markers,mdict)
		toAppend["msgMarkers"] = countMarkers2(toAppend["msgTokens"],mdict)
		toAppend["replyMarkers"] = countMarkers2(toAppend["replyTokens"],mdict)
		
		#for marker in markers:
		#	if marker["marker"] in toAppend["msgTokens"]:
		#		toAppend["msgMarkers"].append(marker["marker"])
		#	if marker["marker"] in toAppend["replyTokens"]:
		#		toAppend["replyMarkers"].append(marker["marker"])

		userUtterances = utterancesById.get(toAppend["replyUserId"], [])
		userUtterances.append(toAppend["reply"])
		utterancesById[toAppend["replyUserId"]] = userUtterances
		utterances.append(toAppend)
	#shared_code.log("averageSentiment: " + str(float(averageSentiment)/float(len(utterances))))
	#pprint(vcounts)					#code to print counts of verification of speaker and replier to sanity check
	return {"utterances": utterances, "utterancesById": utterancesById}


def getUtterancesById(utterances):
	utterancesById = {}
	for utterance in utterances:
		userUtterances = utterancesById.get(utterance["replyUserId"], [])
		userUtterances.append(utterance["reply"])
		utterancesById[utterance["replyUserId"]] = userUtterances
	return utterancesById

# Reads in tweets
def readCSV(markers, inputFile, users):
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	utterances = []
	header=True
	utterancesById = {}
	continues = 0
	averageSentiment = 0
	toReturn = []
	for i, row in enumerate(reader):
		if(i % 10000 is 0):
			shared_code.log("On line " + str(i) + " of 230000")
		row.append(i)
		toReturn.append(row)
		
		#if(i > 10000):
		#	shared_code.log("Continuing")
		#	break
		if header:
			header=False
			continue
	return toReturn
		

def readUserInfo():
	reader=csv.reader(open(userFile),dialect="excel-tab")
	users = []
	header=True
	for row in reader:
		if header:
			header=False
			continue
		toAppend = {}
		toAppend["uid"] = row[0]
		toAppend["screenname"] = row[1]
		toAppend["verified"] = literal_eval(row[2])
		toAppend["numtweets"] = row[3]
		toAppend["numfriends"] = row[4]
		toAppend["numfollowers"] = row[5]
		toAppend["numlistsin"] = row[6]
		toAppend["numfavoritesgiven"] = row[7]

		users.append(toAppend)
	return users

def read(inputFile):
	reader=csv.reader(open(inputFile),dialect="excel-tab")
	toReturn = []
	for i, row in enumerate(reader):
		toReturn.append(row[0])
	return toReturn

def logInfo(results, markers):
	averages = {}
	markerFreqRange = 15
	categories = shared_code.allMarkers(markers)
	types = ["..truetrue", ".truefalse", ".falsetrue", "falsefalse"]
	for verifiedType in types:
		for i in range(0, markerFreqRange):
			iStr = str(i)
			if i < 10:
				iStr = "0"+iStr
			averages[verifiedType+iStr] = []
	for result in results:
		for k in range(0, markerFreqRange):
			if result["powerDenom"] < k:
				continue
			kStr = str(k)
			if k < 10:
				kStr = "0"+kStr

			if("verifiedSpeaker" in result):
				if(result["verifiedSpeaker"] and result["verifiedReplier"]):
					averages["..truetrue"+kStr].append(result["alignment"])
				elif(result["verifiedSpeaker"] and not result["verifiedReplier"]):
					averages[".truefalse"+kStr].append(result["alignment"])
				elif((not result["verifiedSpeaker"]) and result["verifiedReplier"]):
					averages[".falsetrue"+kStr].append(result["alignment"])
				else:
					averages["falsefalse"+kStr].append(result["alignment"])
	toLog = []
	for key in averages:
		toAppend = {}
		toAppend["freq"] = int(key[-2:])
		toAppend["verif"] = key[:10]
		value = averages[key]
		if(len(value) == 0):
			continue
		average =  sum(value) / float(len(value))
		toAppend["average"] = average
		toAppend["alignments"] = str(len(value))
		toLog.append(toAppend)
	toLog = sorted(toLog, key=lambda k: k["freq"])
	for logging in toLog:
		shared_code.log(str(logging["freq"]) + ": " + str(logging["average"]) + " - for " + logging["alignments"] + " alignments " + logging["verif"])

def combineSentiments(positives, negatives):
	toReturn = {}
	for positive in positives:
		toReturn[positive] = 1
	for negative in negatives:
		toReturn[negative] = -1
	return toReturn
shared_code.initialize()
#shared_code.parallelizer(shared_code.printer)
#exit()
positives = read("data/positive.txt")
negatives = read("data/negative.txt")
sentiments = combineSentiments(positives, negatives)
#testResult = test(testFile, testMarkers, testOutputFile)
#if(not testResult):
#	shared_code.log("DIDN'T PASS TEST")
#	#exit()

users = readUserInfo()
markers = shared_code.readMarkers(markersFile)

rows = readCSV(markers, inputFile, users)
#constants = (markers, users, positives, negatives)
#variables = rows
#toParallelize = []
#for row in rows:
#	toParallelize.append((markers, users, positives, negatives, row))
#utterances = shared_code.parallelizer(transformCSV, toParallelize)
#utterances = [x for x in utterances if x != None]

def preprocessingCSV(markers, inputFile, sentiments):
	users = readUserInfo()
	rows = readCSV(markers, inputFile, users)
	utterancedict = transformCSVnonP(markers, users, sentiments,rows)
	utterances = utterancedict["utterances"]
	utterancesById = utterancedict["utterancesById"]
	return utterancedict

def preprocessingCSVOld(markers, inputFile, sentiments):
	users = readUserInfo()
	rows = readCSV(markers, inputFile, users, positives, negatives)
	constants = (markers, users, sentiments)
	variables = rows
	toParallelize = []
	for row in rows:
		toParallelize.append((markers, users, positives, negatives, row))
	utterances = shared_code.parallelizer(transformCSV, toParallelize)
	utterances = [x for x in utterances if x != None]
	return utterances

import cProfile
#cProfile.run('utterancedict = preprocessingCSV(markers, inputFile, positives, negatives)','nstats.tmp')
#cProfile.run('utterances = preprocessingCSVOld(markers, inputFile, positives, negatives)','ostats.tmp')
import pstats

#n = pstats.Stats('nstats.tmp')
#n.strip_dirs().sort_stats('cumulative').print_stats(25)
#
#o = pstats.Stats('ostats.tmp')
#o.strip_dirs().sort_stats('cumulative').print_stats(25)

utterancedict = preprocessingCSV(markers, inputFile, sentiments)
utterances = utterancedict["utterances"]
utterancesById = utterancedict["utterancesById"]
#utterancesById = getUtterancesById(utterances)

markers = getCommonMarkers(utterances)
#shared_code.log(markers)
groupedUtterances = shared_code.group(utterances)
shared_code.log("Grouped utterances")
sparsities = shared_code.calculateSparsity(groupedUtterances)
shared_code.log("Calculated Sparsities")
setUppedResults = shared_code.metaDataExtractor(groupedUtterances, markers)
shared_code.log("Setted up Results")

results = shared_code.calculateAlignment(setUppedResults, markers, sparsities, 0, 0)
#logInfo(results, markers)
shared_code.writeFile(results, outputFile, True)
shared_code.initialize()
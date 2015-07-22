import csv
import operator
import itertools
import re
import traceback
import shared_code
from ast import literal_eval
from pprint import pprint
import cProfile
import pstats
import logger

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"
testOutputFile = "debug/test_results.csv"

inputFile = "data/pairedtweets2.txt"
markersFile = "wordlists/markers_worldenglish.csv"
outputFile = "debug/results.csv"

userFile = "data/pairedtweets.txt.userinfo"

numMarkers = 50
smoothing = 1
formulaType = "TRUE_POWER" # alternative is DMN
shouldWriteHeader = True

# Reads in info about users
# Need this function for power proxy
def readUserInfo():
	reader=csv.reader(open(userFile),dialect="excel-tab")
	next(reader, None)
	users = []
	for row in reader:
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

# Reads in tweets
def readCSV(inputFile, users, numOfMarkers):
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	next(reader, None)
	utterances = []
	toReturn = []
	freqs = {}
	for i, row in enumerate(reader):
		if(i % 10000 is 0):
			logger.log("On line " + str(i) + " of 230000")
		row = processTweetCSVRow(row)
		if(row["msgUserId"] == row["replyUserId"]):
			continue
		for word in row["msgTokens"]:
			freqs[word] = freqs.get(word, 0) + 1
		for word in row["replyTokens"]:
			freqs[word] = freqs.get(word, 0) + 1
		toReturn.append(row)
	markers = []
	freqs = [(k, freqs[k]) for k in sorted(freqs, key=freqs.get, reverse=True)]
	subset = freqs[0:numOfMarkers]
	for subsetTuple in subset:
		if(subsetTuple[0] == "[mention]" or subsetTuple[0] == "[url]"):
			continue
		markers.append({"marker": subsetTuple[0], "category": subsetTuple[0]})
	logger.log(markers)
	return {"rows": toReturn, "markers": markers}

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

def getCommonMarkers(utterances, numOfMarkers):
	freqs = {}
	for utterance in utterances:
		message = utterance["msg"].split(" ")
		for word in message:
			freqs[word] = freqs.get(word, 0) + 1
		reply = utterance["reply"].split(" ")
		for word in reply:
			freqs[word] = freqs.get(word, 0) + 1
	
	toReturn = []
	logger.log(len(freqs))
	
	return toReturn

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

def findUser(udict,uid):
	return udict.get(uid, False)

#Code to take in the user dictionary & a user ID and return if that user is verified
#	Note: users with missing data are considered unverified
def verifySpeaker(udict,uid):
	msgUser = findUser(udict,uid)
	if(msgUser != False):
		return msgUser["verified"]
	else:
		return False

def countMarkers(tokens,markers):
	return [val for val in tokens if val in markers.keys()]

def transformCSVnonP(markers, users, rows):
	utterances = []
	udict = makeUserDict(users)
	mdict = makeMarkerDict(markers)
	tests = {"TrueTrue": 0, "TrueFalse": 0, "FalseTrue": 0, "FalseFalse": 0}
	for i, row in enumerate(rows):
		if(i % 10000 is 0):
			logger.log("On " + str(i) + " of " + str(len(rows))) 
		if(users is not False):
			row["verifiedSpeaker"] = verifySpeaker(udict,row["msgUserId"])
			row["verifiedReplier"] = verifySpeaker(udict,row["replyUserId"])
			tests[str(row["verifiedSpeaker"]) + str(row["verifiedReplier"])] += 1
		row["msgMarkers"] = countMarkers(row["msgTokens"],mdict)
		row["replyMarkers"] = countMarkers(row["replyTokens"],mdict)

		utterances.append(row)
	logger.log(tests)
	return utterances

def logInfo(results, markers):
	averages = {}
	markerFreqRange = 15
	categories = shared_code.allMarkers(markers)
	types = ["..truetrue", ".truefalse", ".falsetrue", "falsefalse"]
	for verifiedType in types:
		for markerFreq in range(0, markerFreqRange):
			markerFreqStr = str(markerFreq)
			if markerFreq < 10:
				markerFreqStr = "0"+markerFreqStr
			averages[verifiedType+markerFreqStr] = []
	for result in results:
		for markerFreq in range(0, markerFreqRange):
			if result["powerDenom"] < markerFreq or result["baseDenom"] < markerFreq:
				continue
			markerFreqStr = str(markerFreq)
			if markerFreq < 10:
				markerFreqStr = "0"+markerFreqStr
			if("verifiedSpeaker" in result):
				if(result["verifiedSpeaker"] and result["verifiedReplier"]):
					averages["..truetrue"+markerFreqStr].append(result["alignment"])
				elif(result["verifiedSpeaker"] and not result["verifiedReplier"]):
					averages[".truefalse"+markerFreqStr].append(result["alignment"])
				elif((not result["verifiedSpeaker"]) and result["verifiedReplier"]):
					averages[".falsetrue"+markerFreqStr].append(result["alignment"])
				else:
					averages["falsefalse"+markerFreqStr].append(result["alignment"])
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
		logger.log(str(logging["freq"]) + ": " + str(logging["average"]) + " - for " + logging["alignments"] + " alignments " + logging["verif"])

start = logger.initialize()
users = readUserInfo()
result = readCSV(inputFile, users, numMarkers)
rows = result["rows"]
markers = result["markers"]
utterances = transformCSVnonP(markers, users,rows)
results = shared_code.calculateAlignments(utterances, markers, smoothing, formulaType, outputFile, shouldWriteHeader)
logInfo(results, markers)
logger.finish(start)
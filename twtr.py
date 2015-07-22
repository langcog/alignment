import csv
import operator
import itertools
import re
import traceback
import shared_code
import datetime
from ast import literal_eval
from pprint import pprint
import cProfile
import pstats
import time

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"
testOutputFile = "debug/test_results.csv"

inputFile = "data/pairedtweets2.txt"
markersFile = "wordlists/markers_worldenglish.csv"
outputFile = "debug/results.csv"

userFile = "data/pairedtweets.txt.userinfo"

# Reads in info about users
# Need this function for power proxy
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

# Reads in tweets
def readCSV(inputFile, users):
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	utterances = []
	header=True
	toReturn = []
	for i, row in enumerate(reader):
		if(i % 10000 is 0):
			shared_code.log("On line " + str(i) + " of 230000")
		row.append(i)
		toReturn.append(row)
		if header:
			header=False
			continue
	return toReturn

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
	freqs = [(k, freqs[k]) for k in sorted(freqs, key=freqs.get, reverse=True)]
	toReturn = []
	shared_code.log(len(freqs))
	subset = freqs[0:numOfMarkers]
	
	for subsetTuple in subset:
		if(subsetTuple[0] == "[mention]" or subsetTuple[0] == "[url]"):
			continue
		toReturn.append({"marker": subsetTuple[0], "category": subsetTuple[0]})
	shared_code.log(toReturn)
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

def findUser2(udict,uid):
	return udict.get(uid, False)

#Code to take in the user dictionary & a user ID and return if that user is verified
#	Note: users with missing data are considered unverified
def verifySpeaker(udict,uid):
	msgUser = findUser2(udict,uid)
	if(msgUser != False):
		return msgUser["verified"]
	else:
		return False

def countMarkers2(tokens,markers):
	return [val for val in tokens if val in markers.keys()]

def transformCSVnonP(markers, users, rows):
	utterances = []
	udict = makeUserDict(users)
	mdict = makeMarkerDict(markers)
	vcounts = {}
	tests = {"TrueTrue": 0, "TrueFalse": 0, "FalseTrue": 0, "FalseFalse": 0}
	for i, row in enumerate(rows):
		if(i % 10000 is 0):
			shared_code.log("On " + str(i) + " of " + str(len(rows))) 
		
		toAppend = processTweetCSVRow(row)

		if(users is not False):
			toAppend["verifiedSpeaker"] = verifySpeaker(udict,row[1])
			toAppend["verifiedReplier"] = verifySpeaker(udict,row[4])
			tests[str(toAppend["verifiedSpeaker"]) + str(toAppend["verifiedReplier"])] += 1
		toAppend["msgMarkers"] = countMarkers2(toAppend["msgTokens"],mdict)
		toAppend["replyMarkers"] = countMarkers2(toAppend["replyTokens"],mdict)

		utterances.append(toAppend)
	shared_code.log(tests)
	return utterances

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
			if result["powerDenom"] < k or result["baseDenom"] < k:
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


shared_code.initialize()
start = time.time()

users = readUserInfo()
markers = shared_code.readMarkers(markersFile)

rows = readCSV(inputFile, users)

realRows = []
for row in rows:
	realRows.append(processTweetCSVRow(row))
markers = getCommonMarkers(realRows, 50)


utterances = transformCSVnonP(markers, users,rows)

groupedUtterances = shared_code.group(utterances)
shared_code.log("Grouped utterances")
sparsities = shared_code.calculateSparsity(groupedUtterances)
shared_code.log("Calculated Sparsities")
setUppedResults = shared_code.metaDataExtractor(groupedUtterances, markers)
shared_code.log("Setted up Results")

results = shared_code.calculateAlignment(setUppedResults, markers, sparsities, 0, 0, 1, "TRUE_POWER")
logInfo(results, markers)
shared_code.writeFile(results, outputFile, True)
done = time.time()
shared_code.finish(start, done)
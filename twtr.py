import csv
import shared_code
from ast import literal_eval
import cProfile
import logger
import string

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

def remove_values_from_list(the_list, val):
   return [value for value in the_list if value != val]

# Reads in tweets
def readCSV(inputFile, users, numOfMarkers):
	functionWords = "of, at, in, without, between, he, they, anybody, it, one, the, a, that, my, more, much, either, neither, and, that, when, while, although, or, be, is, am, are, were, was, have, has, had, got, do, did, doing, no, not, nor, as"
	functionWords = functionWords.split(" ")
	reciprocities = {}
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	next(reader, None)
	utterances = []
	toReturn = []
	quotes = 0
	colons = 0
	rts = 0
	selfAligns = 0
	wholes = 0
	freqs = {}
	msgLove = 0
	replyLove = 0
	for i, row in enumerate(reader):
		if(i % 10000 is 0):
			logger.log("On line " + str(i) + " of 230000")
		row = processTweetCSVRow(row)
		reciprocities[row["convId"]] = False

		realMessage = remove_values_from_list(row["msgTokens"], "[mention]")
		realMessage = remove_values_from_list(realMessage, "[url]")
		if(len(realMessage) == 0):
			continue

		realReply = remove_values_from_list(row["replyTokens"], "[mention]")
		realReply = remove_values_from_list(realReply, "[url]")
		if(len(realReply) == 0):
			continue

		if("”" in row["reply"] and row["msg"] in row["reply"]):
			continue
		if("[mention] :" in row["reply"] and row["msg"] in row["reply"]):
			continue
		if(" rt " in row["reply"] and row["msg"] in row["reply"]):
			continue
		if(row["msgUserId"] == row["replyUserId"]):
			continue
		for word in row["msgTokens"]:
			freqs[word] = freqs.get(word, 0) + 1
		for word in row["replyTokens"]:
			freqs[word] = freqs.get(word, 0) + 1
		utterances.append(row)
	for reciprocity in reciprocities:
		reverse = (reciprocity[1], reciprocity[0])
		if reverse in reciprocities:
			reciprocities[reciprocity] = True
	for utterance in utterances:
		if reciprocities[utterance["convId"]]:
			utterance["reciprocity"] = True
		else:
			utterance["reciprocity"] = False
		#logger.log(utterance["msgMarkers"])
		if("up" in utterance["msgTokens"]):
			msgLove += 1
			replyLove += 1
		toReturn.append(utterance)
	logger.log(msgLove)
	logger.log(replyLove)
	markers = []
	freqs = [(k, freqs[k]) for k in sorted(freqs, key=freqs.get, reverse=True)]
	subset = freqs[0:numOfMarkers]
	
	for subsetTuple in subset:
		logger.log(subsetTuple)
		if(subsetTuple[0] == "[mention]" or subsetTuple[0] == "[url]"):
			continue
		else:
			markers.append({"marker": subsetTuple[0], "category": subsetTuple[0]})
		#if(subsetTuple[0] in functionWords):
		#	markers.append({"marker": subsetTuple[0], "category": "FunctionWords"})
		#elif(subsetTuple[0] in string.punctuation):
		#	markers.append({"marker": subsetTuple[0], "category": "Punctuation"})
		#elif("'" not in subsetTuple[0]):
		#	markers.append({"marker": subsetTuple[0], "category": "ContentWords"})
	logger.log(markers)
	return {"rows": toReturn, "markers": markers}



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

#Code to take in the user dictionary & a user ID and return if that user is verified
#	Note: users with missing data are considered unverified
def numFollowers(udict,uid):
	msgUser = findUser(udict,uid)
	if(msgUser != False):
		return float(msgUser["numfollowers"])
	else:
		return 0

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
			row["speakerFollowers"] = numFollowers(udict, row["msgUserId"])
			row["replierFollowers"] = numFollowers(udict, row["replyUserId"])

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
				if(result["percentDiff"] > 0.9182 and result["percentDiff"] is not 1 and result["verifiedSpeaker"]):
					averages["..truetrue"+markerFreqStr].append(result["alignment"])
				#elif(result["verifiedSpeaker"] and not result["verifiedReplier"]):
				#	averages[".truefalse"+markerFreqStr].append(result["alignment"])
				#elif((not result["verifiedSpeaker"]) and result["verifiedReplier"]):
				#	averages[".falsetrue"+markerFreqStr].append(result["alignment"])
				elif (result["percentDiff"] < 0.2296 and not result["verifiedReplier"]):
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
#exit()
logInfo(results, markers)
logger.finish(start)
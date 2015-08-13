import csv
import alignment
from ast import literal_eval
import cProfile
import logger1
import string
from random import shuffle

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"


inputFile = "data/pairedtweets2.txt"
markersFile = "wordlists/markers_worldenglish.csv"
outputFile = "debug/results"

userFile = "data/pairedtweets.txt.userinfo"

numMarkers = 50
smoothing = 1
shouldWriteHeader = True



# Reads in info about users
# Need this function for power proxy
def readUserInfo():
	reader=csv.reader(open(userFile),dialect="excel-tab")
	next(reader, None)
	users = []
	verified = 0
	unverified = 0
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
		if(toAppend["verified"]):
			verified += 1
		else:
			unverified += 1
		users.append(toAppend)
	logger1.log(verified)
	logger1.log(unverified)
	logger1.log("-------------")
	return users

#Processing the main information in a single row of the tweet TSV file & putting it into a dictionary

def processTweetCSVRow(row):
	toAppend = {}
	toAppend["docId"] = "TWITTER"
	toAppend["corpus"] = "TWITTER"
	toAppend["msgId"] = row[0]
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
	allMessages = {}
	functionWords = "of, at, in, without, between, he, they, anybody, it, one, the, a, that, my, more, much, either, neither, and, that, when, while, although, or, be, is, am, are, were, was, have, has, had, got, do, did, doing, no, not, nor, as"
	functionWords = functionWords.split(" ")
	reciprocities = {}
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	next(reader, None)
	utterances = []
	toReturn = []
	freqs = {}
	totalCount = 0.0
	for i, row in enumerate(reader):
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
		totalCount += 2
		if(row["msgId"] in allMessages):
			allMessages[row["msgId"]]["reply"] += row["reply"]
			allMessages[row["msgId"]]["replyTokens"] += row["replyTokens"]
			allMessages[row["msgId"]]["count"] += 1
		else:
			allMessages[row["msgId"]] = row
			allMessages[row["msgId"]]["count"] = 1

	for key in allMessages:
		row = allMessages[key]
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
		toReturn.append(utterance)
	markers = []
	freqs = [(k, freqs[k], freqs[k]/totalCount) for k in sorted(freqs, key=freqs.get, reverse=True)]
	subset = freqs[0:numOfMarkers]
	
	for subsetTuple in subset:
		if(subsetTuple[0] == "[mention]" or subsetTuple[0] == "[url]"):
			continue
		else:
			markers.append({"marker": subsetTuple[0], "category": subsetTuple[0], "freq": subsetTuple[2]})
	logger1.log(markers)
	exit()
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
	logger1.log(len(freqs))
	
	return toReturn

def makeDict(toConvert, key):
    toReturn = {}
    for element in toConvert:
		    toReturn[element[key]] = element
    return toReturn

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

# Code to take in the user dictionary & a user ID and return if that user is verified
# Note: users with missing data are considered unverified
def numFollowers(udict,uid):
	msgUser = findUser(udict,uid)
	if(msgUser != False):
		return float(msgUser["numfollowers"])
	else:
		return 0

def screenName(udict, uid):
	msgUser = findUser(udict,uid)
	if(msgUser != False):
		return msgUser["screenname"]
	else:
		return 0

def countMarkers(tokens,markers):
	return [val for val in tokens if val in markers.keys()]

def transformCSVnonP(markers, users, rows):
	utterances = []
	udict = makeDict(users, "uid")
	mdict = makeDict(markers, "marker")
	tests = {"TrueTrue": 0, "TrueFalse": 0, "FalseTrue": 0, "FalseFalse": 0}
	for i, row in enumerate(rows):
		if(users is not False):
			row["verifiedSpeaker"] = verifySpeaker(udict,row["msgUserId"])
			row["verifiedReplier"] = verifySpeaker(udict,row["replyUserId"])
			row["speakerFollowers"] = numFollowers(udict, row["msgUserId"])
			row["replierFollowers"] = numFollowers(udict, row["replyUserId"])
			row["msgScreenname"] = screenName(udict, row["msgUserId"])
			row["replyScreenname"] = screenName(udict, row["replyUserId"])

			tests[str(row["verifiedSpeaker"]) + str(row["verifiedReplier"])] += 1
		row["msgMarkers"] = countMarkers(row["msgTokens"],mdict)
		row["replyMarkers"] = countMarkers(row["replyTokens"],mdict)

		utterances.append(row)
	return utterances

def read(inputFile):
	reader=csv.reader(open(inputFile),dialect="excel-tab")
	toReturn = {}
	for i, row in enumerate(reader):
		toReturn[row[0]] = True
	return toReturn

def shuffleUtterances(utterances, shouldShuffleMsgMarkerFreqs, shouldShuffleReplyMarkerFreqs, shouldShuffleMsgUserIds, shouldShuffleReplyUserIds, shouldShuffleVerifiedSpeaker, shouldShuffleVerifiedReplier, shouldShuffleMsgMarkers, shouldShuffleReplyMarkers):
	replyUserIds = []
	msgUserIds = []
	allReplyMarkers = []
	allMsgMarkers = []
	verifiedReplies = []
	verifiedSpeakers = []
	msgMarkerFreqs = []
	replyMarkerFreqs = []
	for i, utterance in enumerate(utterances):
		if(i % 10000 is 0):
			logger1.log("Adding to utterances " + str(i) + " of " + str(len(utterances)))
		toAppend = {}
		toAppend["reply"] = utterance["reply"]
		replyUserIds.append(utterance["msgUserId"])
		msgUserIds.append(utterance["replyUserId"])
		msgMarkerFreqs.append(len(utterance["msgMarkers"]))
		replyMarkerFreqs.append(len(utterance["replyMarkers"]))
		for marker in utterance["replyMarkers"]:
			allReplyMarkers.append(marker)
		for marker in utterance["msgMarkers"]:
			allMsgMarkers.append(marker)
		verifiedReplies.append(utterance["verifiedReplier"])
		verifiedSpeakers.append(utterance["verifiedSpeaker"])
	shuffle(replyUserIds)
	shuffle(msgUserIds)

	shuffle(allMsgMarkers)
	shuffle(allReplyMarkers)

	shuffle(msgMarkerFreqs)
	shuffle(replyMarkerFreqs)

	shuffle(verifiedReplies)
	shuffle(verifiedSpeakers)
	replyCount = 0
	msgCount = 0
	vspeakCount = 0
	nvspeakCount = 0
	vspeakutterances = 0
	nvspeakutterances = 0
	vspeakutteranceLength = 0
	nvspeakutteranceLength = 0

	vreplyCount = 0
	nvreplyCount = 0
	vreplyutterances = 0
	nvreplyutterances = 0
	vreplyutteranceLength = 0
	nvreplyutteranceLength = 0
	for i, utterance in enumerate(utterances):
		if(i % 10000 is 0):
			logger1.log("Readding " + str(i) + " of " + str(len(utterances)))

	

		if(shouldShuffleReplyUserIds):
			utterances[i]["replyUserId"] = replyUserIds[i]
		if(shouldShuffleMsgUserIds):
			utterances[i]["msgUserId"] = msgUserIds[i]
		
		if(shouldShuffleVerifiedReplier):
			utterances[i]["verifiedReplier"] = verifiedReplies[i]
		if(shouldShuffleVerifiedSpeaker):
			utterances[i]["verifiedSpeaker"] = verifiedSpeakers[i]
		
		if(shouldShuffleReplyMarkers):
			utterances[i]["replyMarkers"] = []
			for j in range(0, replyMarkerFreqs[i]):
				utterances[i]["replyMarkers"].append(allReplyMarkers[replyCount])
				replyCount += 1

		if(shouldShuffleMsgMarkers):
			utterances[i]["msgMarkers"] = []
			for j in range(0, msgMarkerFreqs[i]):
				utterances[i]["msgMarkers"].append(allMsgMarkers[msgCount])
				msgCount += 1

		if(utterances[i]["verifiedSpeaker"] == True):
			vspeakCount += msgMarkerFreqs[i]
			vspeakutterances += 1
			vspeakutteranceLength += len(utterances[i]["msgTokens"])
		else:
			nvspeakCount += replyMarkerFreqs[i]
			nvspeakutterances += 1
			nvspeakutteranceLength += len(utterances[i]["msgTokens"])


		if(utterances[i]["verifiedReplier"] == True):
			vreplyCount += replyMarkerFreqs[i]
			vreplyutterances += 1
			vreplyutteranceLength += len(utterances[i]["replyTokens"])
		else:
			nvreplyCount += replyMarkerFreqs[i]
			nvreplyutterances += 1
			nvreplyutteranceLength += len(utterances[i]["replyTokens"])


	print("Speakers")
	print("Verified Utterances count: " + str(vspeakutterances))
	print("Unverified Utterances count: " + str(nvspeakutterances))
	print("Verified utterance length: " + str(vspeakutteranceLength))
	print("Unverified utterance length: " + str(nvspeakutteranceLength))
	print("Verified tokens per utterance %: " + str(float(vspeakutteranceLength)/vspeakutterances))
	print("Unverified tokens per utterance %: " + str(float(nvspeakutteranceLength)/nvspeakutterances))
	print("Verified marker count: " + str(vspeakCount))
	print("Unverified marker count: " + str(nvspeakCount))
	print("Verified markers per utterance %: " + str(float(vspeakCount)/vspeakutterances))
	print("Unverified markers per utterance %: " + str(float(nvspeakCount)/nvspeakutterances))
	print("-------------")
	print("Repliers")
	print("Verified Utterances count: " + str(vreplyutterances))
	print("Unverified Utterances count: " + str(nvreplyutterances))
	print("Verified utterance length: " + str(vreplyutteranceLength))
	print("Unverified utterance length: " + str(nvreplyutteranceLength))
	print("Verified tokens per utterance %: " + str(float(vreplyutteranceLength)/vreplyutterances))
	print("Unverified tokens per utterance %: " + str(float(nvreplyutteranceLength)/nvreplyutterances))
	print("Verified marker count: " + str(vreplyCount))
	print("Unverified marker count: " + str(nvreplyCount))
	print("Verified markers per utterance %: " + str(float(vreplyCount)/vreplyutterances))
	print("Unverified markers per utterance %: " + str(float(nvreplyCount)/nvreplyutterances))
	return utterances



start = logger1.initialize()

shouldShuffleMsgMarkerFreqs = True
shouldShuffleReplyMarkerFreqs = True
shouldShuffleMsgUserIds = True
shouldShuffleReplyUserIds = True
shouldShuffleVerifiedSpeaker = True
shouldShuffleVerifiedReplier = True
shouldShuffleMsgMarkers = True
shouldShuffleReplyMarkers = True


positives = read("data/positive.txt")
negatives = read("data/negative.txt")

users = readUserInfo()
result = readCSV(inputFile, users, numMarkers)
rows = result["rows"]
markers = result["markers"]

utterances = transformCSVnonP(markers, users,rows)

if(outputFile == "debug/shuffled/"):
	if(shouldShuffleMsgMarkerFreqs):
		outputFile += "T"
	else:
		outputFile += "F"
	if(shouldShuffleReplyMarkerFreqs):
		outputFile += "T"
	else:
		outputFile += "F"

	if(shouldShuffleMsgUserIds):
		outputFile += "T"
	else:
		outputFile += "F"
	if(shouldShuffleReplyUserIds):
		outputFile += "T"
	else:
		outputFile += "F"
	if(shouldShuffleVerifiedSpeaker):
		outputFile += "T"
	else:
		outputFile += "F"
	if(shouldShuffleVerifiedReplier):
		outputFile += "T"
	else:
		outputFile += "F"
	if(shouldShuffleMsgMarkers):
		outputFile += "T"
	else:
		outputFile += "F"
	if(shouldShuffleReplyMarkers):
		outputFile += "T"
	else:
		outputFile += "F"
	logger1.log(utterances[0])
	utterances = shuffleUtterances(utterances, shouldShuffleMsgMarkerFreqs, shouldShuffleReplyMarkerFreqs, shouldShuffleMsgUserIds, shouldShuffleReplyUserIds, shouldShuffleVerifiedSpeaker, shouldShuffleVerifiedReplier, shouldShuffleMsgMarkers, shouldShuffleReplyMarkers)
	logger1.log(utterances[0])

outputFile += "_" + str(numMarkers) + ".csv"

results = alignment.calculateAlignments(utterances, markers, smoothing, outputFile, shouldWriteHeader, {"positives": positives, "negatives": negatives})

logger1.finish(start)
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
outputFile = "debug/results.csv"

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
	freqs = {}
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
	freqs = [(k, freqs[k]) for k in sorted(freqs, key=freqs.get, reverse=True)]
	subset = freqs[0:numOfMarkers]
	
	for subsetTuple in subset:
		if(subsetTuple[0] == "[mention]" or subsetTuple[0] == "[url]"):
			continue
		else:
			markers.append({"marker": subsetTuple[0], "category": subsetTuple[0]})
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
	udict = makeDict(users, "uid")
	mdict = makeDict(markers, "marker")
	tests = {"TrueTrue": 0, "TrueFalse": 0, "FalseTrue": 0, "FalseFalse": 0}
	for i, row in enumerate(rows):
		if(users is not False):
			row["verifiedSpeaker"] = verifySpeaker(udict,row["msgUserId"])
			row["verifiedReplier"] = verifySpeaker(udict,row["replyUserId"])
			row["speakerFollowers"] = numFollowers(udict, row["msgUserId"])
			row["replierFollowers"] = numFollowers(udict, row["replyUserId"])

			tests[str(row["verifiedSpeaker"]) + str(row["verifiedReplier"])] += 1
		row["msgMarkers"] = countMarkers(row["msgTokens"],mdict)
		row["replyMarkers"] = countMarkers(row["replyTokens"],mdict)

		utterances.append(row)
	return utterances

def read(inputFile):
	reader=csv.reader(open(inputFile),dialect="excel-tab")
	toReturn = []
	for i, row in enumerate(reader):
		toReturn.append(row[0])
	return toReturn

def shuffleReplies(utterances):
	allReplies = []
	for i, utterance in enumerate(utterances):
		toAppend = {}
		toAppend["reply"] = utterance["reply"]
		toAppend["replyUserId"] = utterance["replyUserId"]
		toAppend["replyTokens"] = utterance["replyTokens"]
		toAppend["replyMarkers"] = utterance["replyMarkers"]
		allReplies.append(toAppend)
	shuffle(allReplies)
	for i, utterance in enumerate(allReplies):
		if(i % 10000 is 0):
			logger1.log("Readding " + str(i) + " of " + str(len(allReplies)))
		utterances[i]["reply"] = utterance["reply"]
		utterances[i]["replyTokens"] = utterance["replyTokens"]
		utterances[i]["replyUserId"] = utterance["replyUserId"]
		utterances[i]["replyMarkers"] = utterance["replyMarkers"]
	return utterances

def shuffleRepliesAndReplyUserIds(utterances):
	allReplies = []
	replyUserIds = []
	for i, utterance in enumerate(utterances):
		toAppend = {}
		toAppend["reply"] = utterance["reply"]
		toAppend["replyUserId"] = utterance["replyUserId"]
		toAppend["replyTokens"] = utterance["replyTokens"]
		toAppend["replyMarkers"] = utterance["replyMarkers"]
		allReplies.append(toAppend)
		replyUserIds.append(utterance["replyUserId"])
	shuffle(allReplies)
	shuffle(replyUserIds)
	for i, utterance in enumerate(allReplies):
		if(i % 10000 is 0):
			logger1.log("Readding " + str(i) + " of " + str(len(allReplies)))
		utterances[i]["reply"] = utterance["reply"]
		utterances[i]["replyTokens"] = utterance["replyTokens"]
		utterances[i]["replyUserId"] = replyUserIds[i]
		utterances[i]["replyMarkers"] = utterance["replyMarkers"]
	return utterances

def shuffleReplyMarkers(utterances):
	allMarkers = []
	allReplies = []
	for i, utterance in enumerate(utterances):
		toAppend = {}
		toAppend["reply"] = utterance["reply"]
		toAppend["replyUserId"] = utterance["replyUserId"]
		toAppend["replyTokens"] = utterance["replyTokens"]
		toAppend["replyMarkers"] = utterance["replyMarkers"]
		utterances[i]["replyMarkersLen"] = len(utterance["replyMarkers"])
		for marker in toAppend["replyMarkers"]:
			allMarkers.append(marker)
	shuffle(allMarkers)
	count = 0
	for i, utterance in enumerate(utterances):
		if(i %10000 == 0):
			logger1.log("Readding " + str(i) + " of " + str(len(utterances)))
		utterances[i]["replyMarkers"] = []
		for j in range(0, utterance["replyMarkersLen"]):
			utterances[i]["replyMarkers"].append(allMarkers[count])
			count += 1

	return utterances


def shuffleReplyMarkersAndReplyUserId(utterances, shouldShuffleReplyUserIds, shouldShuffleVerifiedSpeaker, shouldShuffleVerifiedReplier, shouldShuffleMsgMarkers, shouldShuffleReplyMarkers):
	newUtterances = []
	allReplyMarkers = []
	allMsgMarkers = []
	verifiedReplies = []
	verifiedSpeakers = []
	for i, utterance in enumerate(utterances):
		if(i % 10000 is 0):
			logger1.log("Adding to newUtterances " + str(i) + " of " + str(len(utterances)))
		toAppend = {}
		toAppend["reply"] = utterance["reply"]
		toAppend["replyUserId"] = utterance["replyUserId"]
		toAppend["replyTokens"] = utterance["replyTokens"]
		toAppend["replyMarkers"] = utterance["replyMarkers"]
		toAppend["msgMarkers"] = utterance["msgMarkers"]
		toAppend["replyMarkersLen"] = len(utterance["replyMarkers"])
		toAppend["msgMarkersLen"] = len(utterance["msgMarkers"])
		for marker in toAppend["replyMarkers"]:
			allReplyMarkers.append(marker)
		for marker in toAppend["msgMarkers"]:
			allMsgMarkers.append(marker)
		newUtterances.append(toAppend)
		verifiedReplies.append(utterance["verifiedReplier"])
		verifiedSpeakers.append(utterance["verifiedSpeaker"])
	shuffle(newUtterances)
	shuffle(allReplyMarkers)
	shuffle(allMsgMarkers)
	shuffle(verifiedReplies)
	shuffle(verifiedSpeakers)
	replyCount = 0
	msgCount = 0
	for i, utterance in enumerate(newUtterances):
		if(i % 10000 is 0):
			logger1.log("Readding " + str(i) + " of " + str(len(newUtterances)))
		utterances[i]["reply"] = ""
		utterances[i]["replyTokens"] = []

		if(shouldShuffleReplyUserIds):
			utterances[i]["replyUserId"] = utterance["replyUserId"]
		
		if(shouldShuffleVerifiedReplier):
			utterances[i]["verifiedReplier"] = verifiedReplies[i]
		if(shouldShuffleVerifiedSpeaker):
			utterances[i]["verifiedSpeaker"] = verifiedSpeakers[i]

		if(shouldShuffleReplyMarkers):
			utterances[i]["replyMarkers"] = []
			for j in range(0, utterance["replyMarkersLen"]):
				utterances[i]["replyMarkers"].append(allReplyMarkers[replyCount])
				replyCount += 1

		if(shouldShuffleMsgMarkers):
			utterances[i]["msgMarkers"] = []
			for j in range(0, utterance["msgMarkersLen"]):
				utterances[i]["msgMarkers"].append(allMsgMarkers[msgCount])
				msgCount += 1
	return utterances



start = logger1.initialize()

shouldShuffleReplyUserIds = False
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

if(outputFile == "debug/shuffled/replies.csv"):
	logger1.log(utterances[0])
	utterances = shuffleReplies(utterances)
	logger1.log(utterances[0])
elif(outputFile == "debug/shuffled/shuffleRepliesAndReplyUserIds.csv"):
	logger1.log(utterances[0])
	utterances = shuffleRepliesAndReplyUserIds(utterances)
	logger1.log(utterances[0])
elif(outputFile == "debug/shuffled/shuffleReplyMarkers.csv"):
	logger1.log(utterances[0])
	utterances = shuffleReplyMarkers(utterances)
	logger1.log(utterances[0])
elif(outputFile == "debug/shuffled/shuffleReplyMarkersAndReplyUserId.csv"):
	
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
	utterances = shuffleReplyMarkersAndReplyUserId(utterances, shouldShuffleReplyUserIds, shouldShuffleVerifiedSpeaker, shouldShuffleVerifiedReplier, shouldShuffleMsgMarkers, shouldShuffleReplyMarkers)
	logger1.log(utterances[0])




results = alignment.calculateAlignments(utterances, markers, smoothing, outputFile, shouldWriteHeader, {})

logger1.finish(start)
#Code to peform alignment calculations on Twitter data
# Created by Jake Prasad & Gabe Doyle
# Runs on Python 3, not 2!!

import csv
import alignment
from ast import literal_eval
import cProfile
import logger1
import string
import sys
from random import shuffle

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"

inputFile = "data/pairedtweets2.txt"
markersFile = "wordlists/LIWC_categories.tsv"
outputFile = "debug/shuffled/results.csv"

userFile = "data/pairedtweets.txt.userinfo"

smoothing = 1
shouldWriteHeader = True
markersFromData = True			#Should we obtain the markers from the data rather than a pre-specified list?
numMarkers = 50				#	If so, how many of the most common tokens to use as markers?

#If we want to try different shuffling options
shouldShuffleMsgUserIds = False
shouldShuffleReplyUserIds = False
shouldShuffleVerifiedSpeaker = False
shouldShuffleVerifiedReplier = False
shouldShuffleMsgMarkers = False
shouldShuffleReplyMarkers = False


# Processing code options - optional tags to be added as -x=VALUE
if (len(sys.argv) > 1):
	for arg in sys.argv[1:]:
		tag = arg[0:2]
		featval = arg[3:]
		if tag == '-i':					#-i: input file
			inputFile = featval
		elif tag == '-m':					#-m: marker list file (TODO: not yet implemented)
			markersFile = featval
			markersFromData = False
		elif tag == '-o':					#-o: output file
			outputFile = featval
		elif tag == '-u':					#-u: user information file
			userFile = featval
		elif tag == '-M':					#-M: derive markers from the data
			markersFromData = True
			if featval != '':				#	optional value: number of markers to use from the data
				numMarkers = int(featval)
		elif tag == '-S':					#-S: shuffle msgs, userids, and replies (to add: specify what options to shuffle)
			if featval =='':
				shouldShuffleMsgUserIds = True
				shouldShuffleReplyUserIds = True
				shouldShuffleVerifiedSpeaker = True
				shouldShuffleVerifiedReplier = True
				shouldShuffleMsgMarkers = True
				shouldShuffleReplyMarkers = True

print("Reading from", inputFile)
print("Printing to", outputFile)

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
		toAppend["verified"] = literal_eval(row[2])		#Verified is listed as "True"/"False"; literal_eval converts to boolean
		toAppend["numtweets"] = int(row[3])
		toAppend["numfriends"] = int(row[4])
		toAppend["numfollowers"] = int(row[5])
		toAppend["numlistsin"] = int(row[6])
		toAppend["numfavoritesgiven"] = int(row[7])
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
#	inputFile is a file of tab-separated values representing message-reply pairs
#	users is the dictionary(?) of user info from readUserInfo()
#	numOfMarkers is the number of markers to extract if extracting markers from data
def readCSV(inputFile, users, numOfMarkers):
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	csvheader = next(reader, None)
	utterances = []
	toReturn = []
	freqs = {}
	userPairs = {}				#Dictionary with key being msg sender, value being list of repliers
	
	for i, row in enumerate(reader):
		row = processTweetCSVRow(row)
		
		#removing mentions and urls from tweets, removing line if resulting tweet is empty
		realMessage = remove_values_from_list(row["msgTokens"], "[mention]")
		realMessage = remove_values_from_list(realMessage, "[url]")
		if(len(realMessage) == 0):
			continue

		realReply = remove_values_from_list(row["replyTokens"], "[mention]")
		realReply = remove_values_from_list(realReply, "[url]")
		if(len(realReply) == 0):
			continue
		
		#removing retweets that weren't screen out by the Twitter API
		if("”" in row["reply"] and row["msg"] in row["reply"]):
			continue
		if("[mention] :" in row["reply"] and row["msg"] in row["reply"]):
			continue
		if(" rt " in row["reply"] and row["msg"] in row["reply"]):
			continue
		if(row["msgUserId"] == row["replyUserId"]):
			continue
		
		#calculating word frequencies in the dataset
		if markersFromData:
			for word in row["msgTokens"]:
				freqs[word] = freqs.get(word, 0) + 1
			for word in row["replyTokens"]:
				freqs[word] = freqs.get(word, 0) + 1
		utterances.append(row)
		
		#Adding pair to userPairs for reciprocity
		userPair = row["convId"]
		if userPair not in userPairs:
			userPairs[userPair] = False
	
	for userPair in userPairs:
		reverse = (userPair[1], userPair[0])
		if reverse in userPairs:
			userPairs[userPair] = True
	
	for utterance in utterances:
		if userPairs[utterance["convId"]]:
			utterance["reciprocity"] = True
		else:
			utterance["reciprocity"] = False
		toReturn.append(utterance)
	
	if markersFromData:
		markers = []
		freqs = [(k, freqs[k]) for k in sorted(freqs, key=freqs.get, reverse=True)]
		subset = freqs[0:(numOfMarkers+2)]		#up to two types ([mention] & [url]) will be removed
		count = 0
		for subsetTuple in subset:
			if(subsetTuple[0] == "[mention]" or subsetTuple[0] == "[url]"):
				continue
			else:
				markers.append({"marker": subsetTuple[0], "category": subsetTuple[0]})
				count += 1
				if count==numOfMarkers:	#if we've gotten up to our marker limit and there are still some left, break out
					break
	else:
		csv.DictReader(open(markersFile,errors="ignore"),dialect="excel-tab")
		#TODO: add marker processing here
	
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

def shuffleUtterances(utterances, shouldShuffleMsgUserIds, shouldShuffleReplyUserIds, shouldShuffleVerifiedSpeaker, shouldShuffleVerifiedReplier, shouldShuffleMsgMarkers, shouldShuffleReplyMarkers):
	replyUserIds = []
	msgUserIds = []
	allReplyMarkers = []
	allMsgMarkers = []
	verifiedReplies = []
	verifiedSpeakers = []
	for i, utterance in enumerate(utterances):
		if(i % 10000 is 0):
			logger1.log("Adding to utterances " + str(i) + " of " + str(len(utterances)))
		toAppend = {}
		toAppend["reply"] = utterance["reply"]
		replyUserIds.append(utterance["msgUserId"])
		msgUserIds.append(utterance["replyUserId"])
		utterances[i]["replyMarkersLen"] = len(utterance["replyMarkers"])
		utterances[i]["msgMarkersLen"] = len(utterance["msgMarkers"])
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

	shuffle(verifiedReplies)
	shuffle(verifiedSpeakers)
	replyCount = 0
	msgCount = 0
	for i, utterance in enumerate(utterances):
		if(i % 10000 is 0):
			logger1.log("Readding " + str(i) + " of " + str(len(utterances)))
		utterances[i]["reply"] = ""
		utterances[i]["replyTokens"] = []

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

#Basic sentiment analysis 
positives = read("data/positive.txt")
negatives = read("data/negative.txt")

users = readUserInfo()
result = readCSV(inputFile, users, numMarkers)
rows = result["rows"]
markers = result["markers"]

utterances = transformCSVnonP(markers, users,rows)

if(outputFile == "debug/shuffled/shuffled"):
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
	utterances = shuffleUtterances(utterances, shouldShuffleMsgUserIds, shouldShuffleReplyUserIds, shouldShuffleVerifiedSpeaker, shouldShuffleVerifiedReplier, shouldShuffleMsgMarkers, shouldShuffleReplyMarkers)
	logger1.log(utterances[0])
	outputFile += ".csv"

results = alignment.calculateAlignments(utterances, markers, smoothing, outputFile, shouldWriteHeader, {})

logger1.finish(start)
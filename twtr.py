import csv
import operator
import itertools
import re
import traceback
import shared_code
import datetime

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"
testOutputFile = "debug/test_results.csv"

inputFile = "data/pairedtweets.txt"
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

# Reads in tweets
def readCSV(markers, inputFile, users, positives, negatives):
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	utterances = []
	header=True
	utterancesById = {}
	continues = 0
	averageSentiment = 0
	for i, row in enumerate(reader):
		if(i % 1000 is 0):
			shared_code.log("On line " + str(i) + " of 230000")
		#if(i > 150000):
		#	shared_code.log("Continuing")
		#	break
		if header:
			header=False
			continue
		toAppend = {}
		toAppend["docId"] = "TWITTER"
		toAppend["corpus"] = "TWITTER"
		
		if(len(row) < 6):
			continue
		toAppend["convId"] = (row[1], row[4])
		toAppend["msgUserId"] = row[1]
		toAppend["msg"] = row[2].lower()
		toAppend["replyUserId"] = row[4]
		toAppend["reply"] = row[5].lower()
		toAppend["msgMarkers"] = []
		toAppend["replyMarkers"] = []
		toAppend["msgTokens"] = row[2].split(" ")
		toAppend["replyTokens"] = row[5].split(" ")
		msgSentiment = 0
		for token in toAppend["msgTokens"]:
			if(token in positives):
				msgSentiment += 1
			elif token in negatives:
				msgSentiment -= 1
		toAppend["msgSentiment"] = msgSentiment

		replySentiment = 0
		for token in toAppend["replyTokens"]:
			if(token in positives):
				replySentiment += 1
			elif token in negatives:
				replySentiment -= 1
		toAppend["replySentiment"] = replySentiment

		averageSentiment += replySentiment
		allTokens = []
		allTokens.append(toAppend["msgTokens"])
		allTokens.append(toAppend["replyTokens"])
		allTokens = [item for sublist in allTokens for item in sublist]
		duplicates = list(set(toAppend["msgTokens"]) & set(toAppend["replyTokens"]))
		if(len(list(set(allTokens))) < 5):
			continue
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
		utterances.append(toAppend)
		userUtterances = utterancesById.get(toAppend["replyUserId"], [])
		userUtterances.append(toAppend["reply"])
		utterancesById[toAppend["replyUserId"]] = userUtterances
	shared_code.log("averageSentiment: " + str(float(averageSentiment)/float(len(utterances))))
	return {"utterances": utterances, "utterancesById": utterancesById}

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
		toAppend["verified"] = row[2]
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
		for category in categories:
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


shared_code.initialize()
positives = read("data/positive.txt")
negatives = read("data/negative.txt")

testResult = test(testFile, testMarkers, testOutputFile)
if(not testResult):
	shared_code.log("DIDN'T PASS TEST")
	exit()

users = readUserInfo()
markers = shared_code.readMarkers(markersFile)

result = readCSV(markers, inputFile, users, positives, negatives)
utterances = result["utterances"]
utterancesById = result["utterancesById"]
markers = getCommonMarkers(utterances)
shared_code.log(markers)
groupedUtterances = shared_code.group(utterances)
shared_code.log("Grouped utterances")
sparsities = shared_code.calculateSparsity(groupedUtterances)
shared_code.log("Calculated Sparsities")
setUppedResults = shared_code.metaDataExtractor(groupedUtterances, markers)
shared_code.log("Setted up Results")
results = shared_code.calculateAlignment(setUppedResults, markers, sparsities, utterances, markerFrequency, utterancesById, 0, 0)
logInfo(results, markers)

header = [list(results[0].keys())]
shared_code.writeFile(header, outputFile, "w")
toWrite = []
for row in results:
	toWrite.append(list(row.values()))
shared_code.writeFile(toWrite, outputFile, "a")

shared_code.initialize()
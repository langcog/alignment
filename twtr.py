import csv
import operator
import itertools
import re
import traceback
import shared_code
import datetime

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"
testOutputFile = "debug/results.csv"

inputFile = "data/pairedtweets1000.txt"
markersFile = "wordlists/markers_worldenglish.csv"
outputFile = "debug/results.csv"

userFile = "data/pairedtweets1000.txt.userinfo"

markerFrequency = 0

def test(testFile, testMarkersFile, testOutputFile):
	markers = shared_code.readMarkers(testMarkersFile)
	result = readCSV(markers, testFile, False, [])
	utterances = result["utterances"]
	utterancesById = result["utterancesById"]
	groupedUtterances = shared_code.group(utterances)
	sparsities = shared_code.calculateSparsity(groupedUtterances)
	setUppedResults = shared_code.metaDataExtractor(groupedUtterances, markers)
	results = shared_code.calculateAlignment(setUppedResults, markers, sparsities, utterances, markerFrequency, utterancesById)
	shared_code.writeFile(results, testOutputFile, "wb")
	results.pop(0)
	results = sorted(results, key=lambda k: -k[6])
	leastPower = results[len(results)-1]
	for result in results:
		shared_code.log(result)

	if (abs(leastPower[6] - -0.158) < 0.01):
		#print("PASSED TEST")
		return True
	else:
		#print("FAILED TEST")
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
	print(subset)
	for subsetTuple in subset:
		toReturn.append({"marker": subsetTuple[0], "category": subsetTuple[0]})
	return toReturn

def findUser(users, uId):
	for user in users:
		if(user["uid"] == uId):
			return user
	return False

# Reads in tweets
def readCSV(markers, inputFile, users, ignoredUsers):
	reader=csv.reader(open(inputFile),dialect="excel-tab")
	utterances = []
	header=True
	utterancesById = {}
	continues = 0
	for i, row in enumerate(reader):
		if(i % 1000 is 0):
			shared_code.log("On line " + str(i) + " of 230000")
		#if(i > 6000):
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
		if(len(ignoredUsers) > 0 and findUser(ignoredUsers, toAppend["msgUserId"]) is not False):
			continues += 1
			continue
		toAppend["msg"] = row[2].lower()
		toAppend["replyUserId"] = row[4]
		toAppend["reply"] = row[5].lower()
		toAppend["msgMarkers"] = []
		toAppend["replyMarkers"] = []
		toAppend["msgTokens"] = row[2].split(" ")
		toAppend["replyTokens"] = row[5].split(" ")
		allTokens = []
		allTokens.append(toAppend["msgTokens"])
		allTokens.append(toAppend["replyTokens"])
		allTokens = [item for sublist in allTokens for item in sublist]
		duplicates = list(set(toAppend["msgTokens"]) & set(toAppend["replyTokens"]))
		if(len(list(set(allTokens))) < 5):
			#continues += 1
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
	shared_code.log("Continues: " + str(continues))
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

def chunks(l, n):
    n = max(1, n)
    return [l[i:i + n] for i in range(0, len(l), n)]

shared_code.initialize()
testResult = test(testFile, testMarkers, testOutputFile)
if(not testResult):
	shared_code.log("DIDN'T PASS TEST")
	#exit()
ignoredUsers = []
magicNum = 10
users = readUserInfo()
for user in users:
	if(int(user["numlistsin"]) < magicNum or int(user["numfollowers"]) < magicNum or int(user["numfriends"]) < magicNum):
		shared_code.log(user)
		ignoredUsers.append(user)
shared_code.log("Read in user info")
markers = shared_code.readMarkers(markersFile)
shared_code.log("Read Markers")
result = readCSV(markers, inputFile, users, [])
#exit()
utterances = result["utterances"]
utterancesById = result["utterancesById"]
#chunked = chunks(utterances, 20000)
#for utterances in chunked:
shared_code.log("Result length: " + str(len(result)))
shared_code.log("Read utterances")
markers = getCommonMarkers(utterances)
shared_code.log("Got common Markers")
groupedUtterances = shared_code.group(utterances)
shared_code.log("Grouped utterances")
sparsities = shared_code.calculateSparsity(groupedUtterances)
shared_code.log("Calculated Sparsities")
setUppedResults = shared_code.metaDataExtractor(groupedUtterances, markers)
shared_code.log("Setted up Results")
results = shared_code.calculateAlignment(setUppedResults, markers, sparsities, utterances, markerFrequency, utterancesById)
shared_code.writeFile(results, outputFile, "wb")
#shared_code.testBoundaries(results, groupedUtterances)
shared_code.log(len(ignoredUsers))
shared_code.initialize()
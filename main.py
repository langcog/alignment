import csv
import operator
import itertools
import re
import traceback
import shared_code

#inputFile = "toy.users"
inputFile = "pairedtweets1000.txt"
markersFile = "test.csv"
outputFile = "results.csv"
userFile = "pairedtweets1000.txt.userinfo"


def findUser(users, uId):
	for user in users:
		if(user["uid"] == uId):
			return user
	return False

# Reads in tweets
def readCSV(markers, inputFile, users):
	reader=csv.reader(open(inputFile),dialect="excel-tab")
	utterances = []
	header=True
	for row in reader:
		if header:
			header=False
			continue
		toAppend = {}
		toAppend["conv#"] = (row[1], row[4])
		toAppend["msgUserId"] = row[1]
		toAppend["msg"] = row[2]
		toAppend["replyUserId"] = row[4]
		toAppend["reply"] = row[5]
		toAppend["msgMarkers"] = []
		toAppend["replyMarkers"] = []
		toAppend["msgTokens"] = row[2].split(" ")
		toAppend["replyTokens"] = row[5].split(" ")
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
		messages = row[2].split(" ")
		replies = row[5].split(" ")
		for marker in markers:
			if marker in messages:
				toAppend["msgMarkers"].append(marker)
			if marker in replies:
				toAppend["replyMarkers"].append(marker)
		toAppend["msgTokens"] = messages
		toAppend["replyTokens"] = replies
		utterances.append(toAppend)
	return utterances






# Finds a conversation given it's conversation #
def findConvo(convo, groupedUtterances):
	for groupedUtterance in groupedUtterances:
		if groupedUtterance[0]["conv#"] == convo:
			return groupedUtterance
	return False

# Prints the conversations with the max and least powers
def testBoundaries(results, groupedUtterances):
	results = sorted(results, key=lambda k: -k[2])
	maxPower = results[0]
	maxConvo = findConvo(maxPower[0], groupedUtterances)
	leastPower = results[len(results)-1]
	leastConvo = findConvo(leastPower[0], groupedUtterances)
	shared_code.log("Max Alignment: " + str(maxPower))
	shared_code.log("Min Alignment: " + str(leastPower))

# Finds out the number of conversations in which both A and B say the same marker
def testNumResults(results, groupedUtterances, markers):
	allCount = 0
	for result in results:
		for marker in markers:
			if(marker in result["intersect"]):
				allCount = allCount + 1
	shared_code.log("Conversations in which both A and B say a marker: " + str(allCount))


def testSetUp(groupedUtterances, markers, results, debug):
	if(len(results) < 10):
		return
	for index in range(1, 10):
		current = results[index]
		intersect = current["intersect"]
		convo = findConvo(current["conv"], groupedUtterances)
		a = current["a"]
		b = current["b"]
		userMarkers = current["userMarkers"]

		# Tests that the markers in intersect are actually said by A and B
		for key, value in intersect.iteritems():
			if(value > 0):
				replyMarkers = []
				msgMarkers = []
				for utterance in convo:
					replyMarkers.append(utterance["replyMarkers"])
					msgMarkers.append(utterance["msgMarkers"])
				replyMarkers = list(itertools.chain(*replyMarkers))
				msgMarkers = list(itertools.chain(*msgMarkers))
				replyMarkers = list(set(replyMarkers))
				msgMarkers = list(set(msgMarkers))
				if(debug):
					shared_code.log("Intersection: " + key)
					shared_code.log("Reply Markers: " + str(sorted(replyMarkers)))
					shared_code.log("Message Markers: " + str(sorted(msgMarkers)))
				if(not (key in msgMarkers and key in replyMarkers)):
					shared_code.log("Something went wrong...")
		
		# Tests if the markers that a or b says are actually in the conversation
		for marker in markers:
			if((a+marker) in userMarkers):
				if(not (marker in str(convo))):
					shared_code.log("Something went wrong...")
			if((b + marker) in userMarkers):
				if(not (marker in str(convo))):
					shared_code.log("Something went wrong...")
	return


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
		toAppend["verified"] = row[2]
		users.append(toAppend)
	return users

def testBayes(results, groupedUtterances):
	for index in range(1, 10):
		current = results[index]
		shared_code.log(current)
		conv = findConvo(current[0], groupedUtterances)
		shared_code.log(conv)
		intersect = current["intersect"]

shared_code.initialize()
users = readUserInfo()
markers = shared_code.readMarkers(markersFile)
utterances = readCSV(markers, inputFile, users)
groupedUtterances = shared_code.group(utterances)
setUppedResults = shared_code.metaDataExtractor(groupedUtterances, markers)
results = shared_code.calculateAlignment(setUppedResults, markers)
#testSetUp(groupedUtterances, markers, setUppedResults, False)
#testBayes(results, groupedUtterances)
shared_code.writeFile(results, outputFile, "wb")
testBoundaries(results, groupedUtterances)
#testNumResults(setUppedResults, groupedUtterances, markers)


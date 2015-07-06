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

shared_code.initialize()
users = readUserInfo()
markers = shared_code.readMarkers(markersFile)
utterances = readCSV(markers, inputFile, users)
groupedUtterances = shared_code.group(utterances)
setUppedResults = shared_code.metaDataExtractor(groupedUtterances, markers)
results = shared_code.calculateAlignment(setUppedResults, markers)
shared_code.writeFile(results, outputFile, "wb")
shared_code.testBoundaries(results, groupedUtterances)


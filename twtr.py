#Code to peform alignment calculations on Twitter data
# Created by Jake Prasad & Gabe Doyle
# Runs on Python 3, not 2!

import csv
import alignment
from ast import literal_eval
import logger1
import string
import sys
import re
from random import shuffle

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"

inputFile = "data/pairedtweets2.txt"
markersFile = "wordlists/LIWC_categories.tsv"
outputFile = "debug/shuffled/results.csv"

userFile = "data/pairedtweets.txt.userinfo"

smoothing = 1				#What is our smoothing variable (assuming +1 smoothing on logodds)
shouldWriteHeader = True		#Should the output have header row?
markersFromData = True			#Should we obtain the markers from the data rather than a pre-specified list?
numMarkers = 50				#	If so, how many of the most common tokens to use as markers?

#words that we want to remove, and words that we want to check as potential signs of manual RT
badwords = re.compile(r"(\[mention\]|\[url\])")
quotewords = re.compile(r"(\brt\b|“|”|\[mention\] :)")

#If we want to try different shuffling options
shuffleIds = False
shuffleTweets = False
shuffleMarkers = False
combineMsgReply = False

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
			outputFile = 'debug/shuffled/shuffled'
			if featval == 'ids':
				shuffleIds = True
				shuffleTweets = False
				shuffleMarkers = False
				combineMsgReply = False
			elif featval == 'words':
				shuffleIds = True
				shuffleTweets = False
				shuffleMarkers = True
				combineMsgReply = False
			elif featval == 'all':
				shuffleIds = True
				shuffleTweets = False
				shuffleMarkers = True
				combineMsgReply = True
			elif featval == 'tweets':
				shuffleIds = False
				shuffleTweets = True
				shuffleMarkers = False
				combineMsgReply = False				

print("Reading from", inputFile)
print("Printing to", outputFile)

# Reads in info about users
# Need this function for power proxy
def readUserInfo():
	reader=csv.reader(open(userFile),dialect="excel-tab")
	csvheader = next(reader, None)
	users = []
	for row in reader:
		toAppend = {"uid": row[0],
				"screenname": row[1],
				"verified": literal_eval(row[2]),		#Verified is listed as "True"/"False"; literal_eval converts to boolean
				"numtweets": int(row[3]),
				"numfriends": int(row[4]),
				"numfollowers": int(row[5]),
				"numlistsin": int(row[6]),
				"numfavoritesgiven": int(row[7])}
		users.append(toAppend)
	return users

#Processing the main information in a single row of the tweet TSV file & putting it into a dictionary
# Note that this is currently dependent on the columns being ordered correctly
# returns None if tweet row is invalid (e.g., no text in a tweet)
def processTweetCSVRow(row):
	toAppend = {"docId": "TWITTER", "corpus": "TWITTER", 	#corpus/doc are added to match CHILDES structure
			"convId": (row[1], row[4]),
			"msgUserId": row[1],
			"msg": row[2].lower(),
			"replyUserId": row[4],
			"reply": row[5].lower(),
			"msgMarkers": [],
			"replyMarkers": []}
	
	#removing self-replies 
	if (toAppend["msgUserId"] == toAppend["replyUserId"]): return None
	
	#removing retweets that weren't screened out by the Twitter API
	if ((toAppend["msg"] in toAppend["reply"]) and quotewords.search(toAppend["reply"])):	return None

	#removing bad words (i.e., mentions & urls)
	if badwords is not None:
		toAppend["msg"] = badwords.sub("",toAppend["msg"])
		toAppend["reply"] = badwords.sub("",toAppend["reply"])
		
	#Split, tokenized, & mention-/url-less versions of the tweets
	toAppend["msgTokens"] = toAppend["msg"].split()		
	toAppend["replyTokens"] = toAppend["reply"].split()
	toAppend["msg"] = ' '.join(toAppend["msgTokens"])
	toAppend["reply"] = ' '.join(toAppend["replyTokens"])
	
	#remove if either tweet has no text
	if (len(toAppend["msgTokens"])==0 or len(toAppend["replyTokens"])==0): return None
	
	#One more catch for retweets w/o mentions, urls
	if (quotewords.search(toAppend["reply"]) and toAppend["msg"] in toAppend["reply"]): return None
	
	return toAppend

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
	userPairs = {}				#Dictionary with key being msg-replier tuple value being whether reciprocal
	
	for i, row in enumerate(reader):
		row = processTweetCSVRow(row)
		if row is None:			#if the tweet pair is not valid (e.g., no text in at least one tweet), skip
			continue
		utterances.append(row)

		#calculating word frequencies in the dataset (only if getting markers from dataset)
		if markersFromData:
			for word in row["msgTokens"]:
				freqs[word] = freqs.get(word, 0) + 1
			for word in row["replyTokens"]:
				freqs[word] = freqs.get(word, 0) + 1
		
		#Adding pair to userPairs for reciprocity
		userPair = row["convId"]
		if userPair not in userPairs:
			userPairs[userPair] = False
		
	#Add reciprocity value to each row, compile into returnable list of dictionaries
	for utterance in utterances:
		utterance["reciprocity"] = ((utterance["replyUserId"],utterance["msgUserId"]) in userPairs)
		toReturn.append(utterance)
	
	#Using data to determine most common markers
	if markersFromData:
		markers = []
		freqs = [k for k in sorted(freqs, key=freqs.get, reverse=True)]
		subset = freqs[0:numOfMarkers]		#up to two types ([mention] & [url]) will be removed
		for word in subset:
			markers.append({"marker": word, "category": word})
	else:
		csv.DictReader(open(markersFile,errors="ignore"),dialect="excel-tab")
		#TODO: add marker processing here
	
	return {"rows": toReturn, "markers": markers}

#Converting a list into a dictionary for faster recall
def makeDict(toConvert, key):
    toReturn = {}
    for element in toConvert:
		    toReturn[element[key]] = element
    return toReturn

#Code to return a user's info, or None if info missing
def findUser(udict,uid):
	return udict.get(uid, None)

#Code to take in the user dictionary & a user ID and return if that user is verified
#	Note: users with missing data are considered unverified
def verifySpeaker(udict,uid):
	msgUser = findUser(udict,uid)
	if msgUser is not None:
		return msgUser["verified"]
	else:
		return False

#Code to take in the user dictionary & a user ID and return #followers
#	Note: users with missing data are reported as NA
def numFollowers(udict,uid):
	msgUser = findUser(udict,uid)
	if msgUser is not None:
		return int(msgUser["numfollowers"])
	else:
		return "NA"

#Extract only the markers from the list of words in the tweet
def extractMarkers(tokens,markers):
	return [val for val in tokens if val in markers.keys()]

#Adding speaker verification, marker sets to each tweet pair
def transformCSV(markers, users, rows):
	utterances = []
	udict = makeDict(users, "uid")
	mdict = makeDict(markers, "marker")
	for i, row in enumerate(rows):
		if(users is not False):
			row["verifiedSpeaker"] = verifySpeaker(udict,row["msgUserId"])
			row["verifiedReplier"] = verifySpeaker(udict,row["replyUserId"])
			row["speakerFollowers"] = numFollowers(udict, row["msgUserId"])
			row["replierFollowers"] = numFollowers(udict, row["replyUserId"])

		row["msgMarkers"] = extractMarkers(row["msgTokens"],mdict)
		row["replyMarkers"] = extractMarkers(row["replyTokens"],mdict)
		utterances.append(row)
	return utterances

def read(inputFile):
	reader=csv.reader(open(inputFile),dialect="excel-tab")
	toReturn = []
	for i, row in enumerate(reader):
		toReturn.append(row[0])
	return toReturn

#code to shuffle the set of tweet pairs in a variety of dimensions
def shuffleUtterances(utterances, shuffleIds, shuffleTweets, shuffleTokens, combineMsgReply):
	replyUserIds = []
	msgUserIds = []
	replyTweets = []
	msgTweets = []
	allReplyTokens = []
	allMsgTokens = []
	replyLengths = []
	msgLengths = []
	for i, utterance in enumerate(utterances):
		if(i % 10000 is 0):
			logger1.log("Adding to utterances " + str(i) + " of " + str(len(utterances)))
		msgUserIds.append(utterance["msgUserId"])
		msgTweets.append(utterance["msgTokens"])
		allMsgTokens.extend(utterance["msgTokens"])
		msgLengths.append(len(utterance["msgTokens"]))
		if not combineMsgReply:						#if we're shuffling msgs and replies together, put everything in msgs
			replyUserIds.append(utterance["replyUserId"])
			replyTweets.append(utterance["replyTokens"])
			allReplyTokens.extend(utterance["replyTokens"])
			replyLengths.append(len(utterance["replyTokens"]))
		else:
			msgUserIds.append(utterance["replyUserId"])
			msgTweets.append(utterance["replyTokens"])			
			allMsgTokens.extend(utterance["replyTokens"])
			msgLengths.append(len(utterance["replyTokens"]))
	
	shuffle(msgUserIds); shuffle(msgTweets); shuffle(allMsgTokens)
	if not combineMsgReply: 
		shuffle(replyUserIds); shuffle(replyTweets); shuffle(allReplyTokens)
	else:
		shuffle(msgLengths)		#only shuffle msgLengths if we're combining msgs and replies
	
	replyMarkerCount = 0
	msgMarkerCount = 0
	
	msgLengthsNew = []
	replyLengthsNew = []

	for i, utterance in enumerate(utterances):		
		utterances[i]["msg"] = ""
		utterances[i]["reply"] = ""

		if(shuffleIds):
			if not combineMsgReply:
				utterances[i]["msgUserId"] = msgUserIds[i]
				utterances[i]["replyUserId"] = replyUserIds[i] 
			else:
				utterances[i]["msgUserId"] = msgUserIds[2*i]
				utterances[i]["replyUserId"] = msgUserIds[2*i+1] 
				
		
		if(shuffleTweets):
			if not combineMsgReply:
				utterances[i]["msgTokens"] = msgTweets[i]
				utterances[i]["replyTokens"] = replyTweets[i]
			else:
				utterances[i]["msgTokens"] = msgTweets[2*i]
				utterances[i]["replyTokens"] = msgTweets[2*i+1]
		
		if(shuffleTokens):
			if not combineMsgReply:
				utterances[i]["msgTokens"] = allMsgTokens[msgMarkerCount:(msgMarkerCount+msgLengths[i])]
				msgMarkerCount += msgLengths[i]
				utterances[i]["replyTokens"] = allReplyTokens[replyMarkerCount:(replyMarkerCount+replyLengths[i])]
				replyMarkerCount += replyLengths[i]
			else:
				utterances[i]["msgTokens"] = allMsgTokens[msgMarkerCount:(msgMarkerCount+msgLengths[2*i])]
				msgMarkerCount += msgLengths[2*i]
				msgLengthsNew.append(msgLengths[2*i])
				utterances[i]["replyTokens"] = allMsgTokens[msgMarkerCount:(msgMarkerCount+msgLengths[2*i+1])]
				msgMarkerCount += msgLengths[2*i+1]
				replyLengthsNew.append(msgLengths[2*i+1])
		utterances[i]["convId"] = (utterances[i]["msgUserId"],utterances[i]["replyUserId"])
		
	return utterances



start = logger1.initialize()

logger1.log("Reading user info...")
users = readUserInfo()
logger1.log("Reading messages...")
result = readCSV(inputFile, users, numMarkers)
rows = result["rows"]
markers = result["markers"]

if(outputFile == "debug/shuffled/shuffled"):
	if(shuffleIds):
		outputFile += "T"
	else:
		outputFile += "F"
	if(shuffleTweets):
		outputFile += "T"
	else:
		outputFile += "F"
	if(shuffleMarkers):
		outputFile += "T"
	else:
		outputFile += "F"
	if(combineMsgReply):
		outputFile += "T"
	else:
		outputFile += "F"
	logger1.log(rows[0])
	rows = shuffleUtterances(rows, shuffleIds, shuffleTweets, shuffleMarkers, combineMsgReply)
	logger1.log(rows[0])
	outputFile += ".csv"

utterances = transformCSV(markers, users, rows)

results = alignment.calculateAlignments(utterances, markers, smoothing, outputFile, shouldWriteHeader)
logger1.finish(start)
from nltk.classify import NaiveBayesClassifier
import csv
import logger1
import random
import alignment
from ast import literal_eval
from collections import Counter
import math
import re

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"
testOutputFile = "debug/test_results.csv"

inputFile = "data/pairedtweets2.txt"
markersFile = "wordlists/markers_worldenglish.csv"
outputFile = "debug/discourse_results.csv"

userFile = "data/pairedtweets.txt.userinfo"

numMarkers = 300
smoothing = 1
shouldWriteHeader = True

def word_feats(words):
	return dict([(word, True) for word in words.split()])

def ngrams(input, n):
	output = []
	for i in range(len(input)-n+1):
		output.append(tuple(input[i:i+n]))
	return output

#Processing the main information in a single tweet of the tweet TSV file & putting it into a dictionary
def processTweetCSVRow(tweet):
	toAppend = {}
	toAppend["msgId"] = tweet[0]
	toAppend["msgUserId"] = tweet[1]
	toAppend["msg"] = tweet[2].lower()
	toAppend["replyUserId"] = tweet[4]
	toAppend["reply"] = tweet[5].lower()
	toAppend["msgTokens"] = toAppend["msg"].split(" ")
	toAppend["replyTokens"] = toAppend["reply"].split(" ")
	return toAppend

def remove_values_from_list(the_list, val):
   return [value for value in the_list if value != val]


# Reads in tweets
def readCSV(inputFile, users):
	reciprocities = {}
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	next(reader, None)
	utterances = []
	toReturn = []
	freqs = {}
	counts = {}
	for i, tweet in enumerate(reader):
		if(i % 10000 is 0):
			logger1.log("On line " + str(i) + " of 230000")
		tweet = processTweetCSVRow(tweet)
		counts[tweet["msgId"]] = counts.get(tweet["msgId"], 0) + 1
		realMessage = remove_values_from_list(tweet["msgTokens"], "[mention]")
		realMessage = remove_values_from_list(realMessage, "[url]")
		if(len(realMessage) == 0):
			continue

		realReply = remove_values_from_list(tweet["replyTokens"], "[mention]")
		realReply = remove_values_from_list(realReply, "[url]")
		if(len(realReply) == 0):
			continue

		if("”" in tweet["reply"] and tweet["msg"] in tweet["reply"]):
			continue
		if("[mention] :" in tweet["reply"] and tweet["msg"] in tweet["reply"]):
			continue
		if(" rt " in tweet["reply"] and tweet["msg"] in tweet["reply"]):
			continue
		if(tweet["msgUserId"] == tweet["replyUserId"]):
			continue

		utterances.append(tweet)
		
	tweets = []
	for utterance in utterances:
		if(counts[utterance["msgId"]] == 1):
			tweets.append(utterance)
			


	return tweets



def trainClassifier(toTrain):
	status = toTrain["status"]
	questionToFollowers = toTrain["questionToFollowers"]
	referenceBroadcast = toTrain["referenceBroadcast"]
	question = toTrain["question"]
	reaction = toTrain["reaction"]
	comment = toTrain["comment"]
	answer = toTrain["answer"]
	response = toTrain["response"]
	status_feats = [(word_feats(f), 'status') for f in status ]
	questionToFollowers_feats = [(word_feats(f), 'questionToFollowers') for f in questionToFollowers ]
	referenceBroadcast_feats = [(word_feats(f), 'referenceBroadcast') for f in referenceBroadcast ]
	question_feats = [(word_feats(f), 'question') for f in question ]
	reaction_feats = [(word_feats(f), 'reaction') for f in reaction ]
	comment_feats = [(word_feats(f), 'comment') for f in comment ]
	answer_feats = [(word_feats(f), 'answer') for f in answer ]
	response_feats = [(word_feats(f), 'response') for f in response ]

	trainfeats = status_feats + questionToFollowers_feats + referenceBroadcast_feats + question_feats + reaction_feats + comment_feats + answer_feats + response_feats
	classifier = NaiveBayesClassifier.train(trainfeats)
	return classifier

def createClassifier(tweets):
	toTrain = {
		"status": ["I . to ! my , is for up in ... and going was today so at go get back day got this am but Im now tomortweet night work tonight off morning home had gon need !! be just getting", "i just changed my twitter page bkgornd and now I can’t stop looking at it, lol !!"],
		"questionToFollowers": ["? you is do I to [url] what [mention] me , know if anyone why who can “ this or of that how does - : on your are need any rt u should people want get did have would tell", "anyone using google voice? just got my invite, should i ?? don’t know what it is? [url] for the video and break down"],
		"referenceBroadcast": ["[mention] ! [url] rt : [mention]: - “ my the , is ( you new – ? !! ) this for at in follow of on ¡ lol u are twitter your thanks via !!! by :) here 2 please check", "rt [mention]: [mention] word that mac lip gloss give u lock jaw ! lol"],
		"question": ["? you what ! are is how u do the did your that , lol where why or ?? hey about was have who it in so haha on doing going know good up get like were for there :) can", "dwl !! what song is that ??"],
		"reaction": ["! you I :) !! , thanks lol it haha that love so good too your thank is are u !!! was for :d me [mention] ¡ hope ? my 3 omg ... oh great hey awesome - happy now aww", "sweet ! im so stoked now !"],
		"comment": ["you I . to , ! do ? it be if me your know have we can get will :) but u that see lol would are so want go let up well need - come ca make or think them", "why are you in tx and why am I just now finding out about it ?! i’m in dfw, till I get a job . i’ll have to come to htown soon !"],
		"answer": [". I , you it “ that ? is but do was he the of a they if not would know be did or does think ) like ( as have what in are - no them said who say ‘", "my fave was “ keeping on top of other week ”"],
		"response": [". I , it was that lol but is yeah ! haha he my know yes you :) like too did well she so its ... though do had no - one as im thanks they think would not good oh", "nah im out in maryland , leaving for tour in a few days ."]
	}

	classifier = trainClassifier(toTrain)

	tests = ["sadly no. some pasta bake, but coffee and pasta bake is not a contender for tea and toast... .", "Rumbly tummy soon to be tamed by Dominos for lunch! Nom nom nom!"]
	for test in tests:
		print(test)
		test = word_feats(test)
		probs = classifier.prob_classify(test)
		classification = classifier.classify(test)
		print(classification)
		print(probs.prob(classification))


	tweetsLength = len(tweets)
	for i, tweet in enumerate(tweets):
		if(i % 10000 is 0):
			logger1.log("On line " + str(i) + " of " + str(tweetsLength))
		feats = word_feats(tweet["msg"])
		probs = classifier.prob_classify(feats)
		classification = classifier.classify(feats)
		prob = probs.prob(classification)
		if(prob > 0.9):
			count = 0
			samples = probs.samples()
			for sample in samples:
				if(probs.prob(sample) > 0.9):
					count += 1
			if(count is 1):
				toTrain[classification].append(tweet["msg"])

	for key in toTrain:
		print(key)
		print(len(toTrain[key]))
		print(random.choice(toTrain[key]))
		print(random.choice(toTrain[key]))
		print(random.choice(toTrain[key]))
		print("-----------------")
	classifier = trainClassifier(toTrain)
	return classifier

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

def makeDict(toConvert, key):
	toReturn = {}
	for element in toConvert:
			toReturn[element[key]] = element
	return toReturn

def transformCSVnonP(users, rows):
	utterances = []
	udict = makeDict(users, "uid")
	tests = {"TrueTrue": 0, "TrueFalse": 0, "FalseTrue": 0, "FalseFalse": 0}
	for i, row in enumerate(rows):
		if(i % 10000 is 0):
			logger1.log("On " + str(i) + " of " + str(len(rows)))
		if(users is not False):
			row["verifiedSpeaker"] = verifySpeaker(udict,row["msgUserId"])
			row["verifiedReplier"] = verifySpeaker(udict,row["replyUserId"])
			row["speakerFollowers"] = numFollowers(udict, row["msgUserId"])
			row["replierFollowers"] = numFollowers(udict, row["replyUserId"])

			tests[str(row["verifiedSpeaker"]) + str(row["verifiedReplier"])] += 1

		utterances.append(row)
	logger1.log(tests)
	return utterances

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

def main():
	utterances = []
	users = readUserInfo()
	tweets = readCSV(inputFile, users)
	classifier = createClassifier(tweets)
	tweetsLength = len(tweets)
	for i, tweet in enumerate(tweets):
		if(i % 10000 is 0):
			logger1.log("On line " + str(i) + " of " + str(tweetsLength))
		feats = word_feats(tweet["msg"])
		probs = classifier.prob_classify(feats)
		classification = classifier.classify(feats)
		prob = probs.prob(classification)
		if(prob > 0.9):
			count = 0
			samples = probs.samples()
			for sample in samples:
				if(probs.prob(sample) > 0.9):
					count += 1
			if(count is 1):
				tweet["msgType"] = classification
		if "msgType" not in tweet:
			tweet["msgType"] = False
			continue

		
		tweets[i] = tweet
	utterances = transformCSVnonP(users, tweets)
	tweets = []
	for i, utterance in enumerate(utterances):
		tweet = utterance
		maxNgram = 1
		ngramLengths = [2,3,4,5]
		ngramPercent = 0
		for ngramLength in ngramLengths:
			msgTrigrams = set(ngrams(tweet["msgTokens"], ngramLength))
			replyTrigrams = set(ngrams(tweet["replyTokens"], ngramLength))
			quoted = set(msgTrigrams).intersection(set(replyTrigrams))
			if len(quoted) == 0:
				maxNgram = ngramLength - 1
				ngramPercent = maxNgram/len(tweet["msgTokens"])
				break
		#logger1.log(ngramPercent)
		if(maxNgram == 1):
			ngramPercent = 0
		tweet["alignment"] = ngramPercent


		tweet["msg"] = False
		tweet["msgTokens"] = False
		tweet["replyTokens"] = False
		tweet["reply"] = False
		tweets.append(tweet)
	alignment.writeFile(tweets, outputFile, shouldWriteHeader)
users = readUserInfo()
tweets = readCSV(inputFile, users)
messages = []
for tweet in tweets:
	for word in tweet["msgTokens"]:
		messages.append(word)
word_counts = Counter(messages)
top150 = word_counts.most_common(150)

loopStart = 0
loopEnd = 100
categories = []
for i in range(loopStart, loopEnd):
	logger1.log("On " + str(i) + " of " + str(loopEnd))
	for j in range(loopStart, loopEnd):
		if(j <= i):
			continue
		word1 = top150[i][0]
		word2 = top150[j][0]
		msgTokens = tweet["msgTokens"]
		ab = 0.0
		anb = 0.0
		nab = 0.0
		nanb = 0.0
		for tweet in tweets:
			#pattern = re.compile(word1 + ".*" + word2)
			if(word1 in msgTokens and word2 in msgTokens):
				ab += 1
			elif(word1 in msgTokens and word2 not in msgTokens):
				anb += 1
			elif(word1 not in msgTokens and word2 in msgTokens):
				nab += 1
			else:
				nanb += 1
		if(ab == 0 or anb == 0 or (ab+anb == 0) or (anb+nanb == 0)):
			continue
		
		collocationScore = math.log(ab/(ab + anb)) - math.log(anb/(anb+nanb))
		if(collocationScore > 1):
			print(word1)
			print(word2)
			print(collocationScore)
			print("----------------------")
			if(word1 == "."):
				word1 = "\."
			if(word2 == "."):
				word2 = "\."
			categories.append(word1 + ".*" + word2)
			categories.append(word2 + ".*" + word1)

print("-----------------------")
print("-----------------------")
print("-----------------------")
print("-----------------------")
print("-----------------------")
print("-----------------------")
print("-----------------------")
print("-----------------------")

for i, category1 in enumerate(categories):
	logger1.log("On " + str(i) + " of " + str(len(categories)))
	for j, category2 in enumerate(categories):
		word1 = category1
		word2 = category2

		ab = 0.0
		anb = 0.0
		nab = 0.0
		nanb = 0.0
		for tweet in tweets:
			pattern1 = re.compile(category1)
			pattern2 = re.compile(category2)
			match1 = pattern1.match(tweet["msg"])
			match2 = pattern2.match(tweet["msg"])
			if(match1 and match2):
				ab += 1
			elif(match1 and not match2):
				anb += 1
			elif(not match1 and match2):
				nab += 1
			else:
				nanb += 1
		if(ab == 0 or anb == 0 or (ab+anb == 0) or (anb+nanb == 0)):
			continue
		
		collocationScore = math.log(ab/(ab + anb)) - math.log(anb/(anb+nanb))
		#if(collocationScore > 1):
		print(word1)
		print(word2)
		print(collocationScore)
		print("----------------------")
		#categories.append(word1 + ".*" + word2)


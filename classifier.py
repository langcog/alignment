from nltk.classify import NaiveBayesClassifier
import csv
import logger1
import random

testMarkers = "debug/test_markers.csv"
testFile = "debug/toy.users"
testOutputFile = "debug/test_results.csv"

inputFile = "data/pairedtweets2.txt"
markersFile = "wordlists/markers_worldenglish.csv"
outputFile = "debug/results.csv"

userFile = "data/pairedtweets.txt.userinfo"

numMarkers = 300
smoothing = 1
shouldWriteHeader = True

def word_feats(words):
    return dict([(word, True) for word in words.split()])

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
def readCSV(inputFile):
	reciprocities = {}
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	next(reader, None)
	utterances = []
	toReturn = []
	freqs = {}
	for i, row in enumerate(reader):
		if(i % 10000 is 0):
			logger1.log("On line " + str(i) + " of 230000")
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

	return toReturn



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
		"status": ["I . to ! my , is for up in ... and going was today so at go get back day got this am but Im now tomorrow night work tonight off morning home had gon need !! be just getting", "I just changed my twitter page bkgornd and now I can’t stop looking at it, lol !!"],
		"questionToFollowers": ["? you is do I to [url] what [mention] me , know if anyone why who can “ this or of that how does - : on your are need any rt u should people want get did have would tell", "anyone using google voice? just got my invite, should i?? don’t know what it is? [url] for the video and break down"],
		"referenceBroadcast": ["[mention] ! [url] rt : [mention]: - “ my the , is ( you new – ? !! ) this for at in follow of on ¡ lol u are twitter your thanks via !!! by :) here 2 please check", "rt [mention]: [mention] word that mac lip gloss give u lock jaw ! lol"],
		"question": ["? you what ! are is how u do the did your that , lol where why or ?? hey about was have who it in so haha on doing going know good up get like were for there :) can", "DWL!! what song is that ??"],
		"reaction": ["! you I :) !! , thanks lol it haha that love so good too your thank is are u !!! was for :d me [mention] ¡ hope ? my 3 omg ... oh great hey awesome - happy now aww", "sweet! im so stoked now !"],
		"comment": ["you I . to , ! do ? it be if me your know have we can get will :) but u that see lol would are so want go let up well need - come ca make or think them", "why are you in tx and why am I just now finding out about it ?! i’m in dfw, till I get a job . i’ll have to come to Htown soon !"],
		"answer": [". I , you it “ that ? is but do was he the of a they if not would know be did or does think ) like ( as have what in are - no them said who say ‘", "my fave was “keeping on top of other week ”"],
		"response": [". I , it was that lol but is yeah ! haha he my know yes you :) like too did well she so its ... though do had no - one as im thanks they think would not good oh", "nah im out in maryland , leaving for tour in a few days ."]
	}

	classifier = trainClassifier(toTrain)

	tests = ["sadly no. some pasta bake, but coffee and pasta bake is not a contender for tea and toast... .", "Rumbly tummy soon to be tamed by Dominos for lunch! Nom nom nom!"]
	for test in tests:
		print(test)
		test = word_feats(test)
		probs = classifier.prob_classify(test)
		classification = classifier.classify(test)
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

	classifier = trainClassifier(toTrain)

	
	return classifier

#classifier = createClassifier(tweets)
#tests = ["sadly no. some pasta bake, but coffee and pasta bake is not a contender for tea and toast... .", "Rumbly tummy soon to be tamed by Dominos for lunch! Nom nom nom!"]
#for test in tests:
#	print(test)
#	test = word_feats(test)
#	probs = classifier.prob_classify(test)
#	classification = classifier.classify(test)
#	print(classification)
#	print(probs.prob(classification))
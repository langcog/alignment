from nltk.classify import NaiveBayesClassifier
import csv
import logger1
import random
import alignment
from ast import literal_eval

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
def readCSV(inputFile, users, markers):
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

def transformCSVnonP(markers, users, rows):
	utterances = []
	udict = makeDict(users, "uid")
	mdict = makeDict(markers, "marker")
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


markers = [{'marker': '.', 'category': '.'}, {'marker': 'the', 'category': 'the'}, {'marker': 'i', 'category': 'i'}, {'marker': ',', 'category': ','}, {'marker': 'to', 'category': 'to'}, {'marker': 'a', 'category': 'a'}, {'marker': 'you', 'category': 'you'}, {'marker': '!', 'category': '!'}, {'marker': 'of', 'category': 'of'}, {'marker': '?', 'category': '?'}, {'marker': 'is', 'category': 'is'}, {'marker': 'and', 'category': 'and'}, {'marker': 'in', 'category': 'in'}, {'marker': 'it', 'category': 'it'}, {'marker': 'for', 'category': 'for'}, {'marker': 'my', 'category': 'my'}, {'marker': 'that', 'category': 'that'}, {'marker': '\\"', 'category': '\\"'}, {'marker': 'on', 'category': 'on'}, {'marker': 'so', 'category': 'so'}, {'marker': 'me', 'category': 'me'}, {'marker': 'this', 'category': 'this'}, {'marker': ':', 'category': ':'}, {'marker': 'be', 'category': 'be'}, {'marker': 'are', 'category': 'are'}, {'marker': 'but', 'category': 'but'}, {'marker': 'have', 'category': 'have'}, {'marker': 'with', 'category': 'with'}, {'marker': 'just', 'category': 'just'}, {'marker': 'not', 'category': 'not'}, {'marker': 'what', 'category': 'what'}, {'marker': "i'm", 'category': "i'm"}, {'marker': 'was', 'category': 'was'}, {'marker': 'your', 'category': 'your'}, {'marker': 'like', 'category': 'like'}, {'marker': 'do', 'category': 'do'}, {'marker': 'at', 'category': 'at'}, {'marker': 'all', 'category': 'all'}, {'marker': 'we', 'category': 'we'}, {'marker': 'if', 'category': 'if'}, {'marker': "it's", 'category': "it's"}, {'marker': 'about', 'category': 'about'}, {'marker': '...', 'category': '...'}, {'marker': 'love', 'category': 'love'}, {'marker': '&', 'category': '&'}, {'marker': 'up', 'category': 'up'}, {'marker': 'no', 'category': 'no'}, {'marker': 'new', 'category': 'new'}, {'marker': 'can', 'category': 'can'}, {'marker': 'as', 'category': 'as'}, {'marker': 'out', 'category': 'out'}, {'marker': 'one', 'category': 'one'}, {'marker': 'good', 'category': 'good'}, {'marker': 'how', 'category': 'how'}, {'marker': 'get', 'category': 'get'}, {'marker': 'from', 'category': 'from'}, {'marker': 'or', 'category': 'or'}, {'marker': "don't", 'category': "don't"}, {'marker': 'will', 'category': 'will'}, {'marker': ')', 'category': ')'}, {'marker': 'they', 'category': 'they'}, {'marker': 'think', 'category': 'think'}, {'marker': 'when', 'category': 'when'}, {'marker': 'follow', 'category': 'follow'}, {'marker': 'people', 'category': 'people'}, {'marker': 'now', 'category': 'now'}, {'marker': 'an', 'category': 'an'}, {'marker': '(', 'category': '('}, {'marker': 'more', 'category': 'more'}, {'marker': 'u', 'category': 'u'}, {'marker': 'know', 'category': 'know'}, {'marker': 'would', 'category': 'would'}, {'marker': 'go', 'category': 'go'}, {'marker': 'who', 'category': 'who'}, {'marker': 'too', 'category': 'too'}, {'marker': 'see', 'category': 'see'}, {'marker': 'im', 'category': 'im'}, {'marker': '️', 'category': '️'}, {'marker': "'", 'category': "'"}, {'marker': 'he', 'category': 'he'}, {'marker': 'video', 'category': 'video'}, {'marker': '!!', 'category': '!!'}, {'marker': 'there', 'category': 'there'}, {'marker': 'by', 'category': 'by'}, {'marker': 'why', 'category': 'why'}, {'marker': '“', 'category': '“'}, {'marker': 'time', 'category': 'time'}, {'marker': 'today', 'category': 'today'}, {'marker': 'much', 'category': 'much'}, {'marker': 'some', 'category': 'some'}, {'marker': '-', 'category': '-'}, {'marker': 'did', 'category': 'did'}, {'marker': '”', 'category': '”'}, {'marker': 'should', 'category': 'should'}, {'marker': 'tweet', 'category': 'tweet'}, {'marker': 'day', 'category': 'day'}, {'marker': 'yes', 'category': 'yes'}, {'marker': 'really', 'category': 'really'}, {'marker': "you're", 'category': "you're"}, {'marker': 'right', 'category': 'right'}, {'marker': '❤', 'category': '❤'}, {'marker': 'got', 'category': 'got'}, {'marker': 'guys', 'category': 'guys'}, {'marker': 'been', 'category': 'been'}, {'marker': 'want', 'category': 'want'}, {'marker': 'still', 'category': 'still'}, {'marker': "that's", 'category': "that's"}, {'marker': 'has', 'category': 'has'}, {'marker': 'make', 'category': 'make'}, {'marker': 'us', 'category': 'us'}, {'marker': "can't", 'category': "can't"}, {'marker': 'here', 'category': 'here'}, {'marker': 'going', 'category': 'going'}, {'marker': ':)', 'category': ':)'}, {'marker': 'had', 'category': 'had'}, {'marker': 'well', 'category': 'well'}, {'marker': 'rt', 'category': 'rt'}, {'marker': 'happy', 'category': 'happy'}, {'marker': 'than', 'category': 'than'}, {'marker': 'need', 'category': 'need'}, {'marker': 'thanks', 'category': 'thanks'}, {'marker': 'thank', 'category': 'thank'}, {'marker': 'great', 'category': 'great'}, {'marker': 'them', 'category': 'them'}, {'marker': 'only', 'category': 'only'}, {'marker': 'even', 'category': 'even'}, {'marker': 'back', 'category': 'back'}, {'marker': '*', 'category': '*'}, {'marker': 'please', 'category': 'please'}, {'marker': 'its', 'category': 'its'}, {'marker': 'last', 'category': 'last'}, {'marker': 'were', 'category': 'were'}, {'marker': 'our', 'category': 'our'}, {'marker': 'oh', 'category': 'oh'}, {'marker': 'say', 'category': 'say'}, {'marker': 'am', 'category': 'am'}, {'marker': 'never', 'category': 'never'}, {'marker': 'his', 'category': 'his'}, {'marker': 'then', 'category': 'then'}, {'marker': 'over', 'category': 'over'}, {'marker': 'could', 'category': 'could'}, {'marker': 'same', 'category': 'same'}, {'marker': 'omg', 'category': 'omg'}, {'marker': 'wait', 'category': 'wait'}, {'marker': 'also', 'category': 'also'}, {'marker': 'year', 'category': 'year'}, {'marker': '!!!', 'category': '!!!'}, {'marker': 'way', 'category': 'way'}, {'marker': '..', 'category': '..'}, {'marker': 'best', 'category': 'best'}, {'marker': 'hope', 'category': 'hope'}, {'marker': 'come', 'category': 'come'}, {'marker': 'because', 'category': 'because'}, {'marker': 'look', 'category': 'look'}, {'marker': "i've", 'category': "i've"}, {'marker': 'first', 'category': 'first'}, {'marker': 'gonna', 'category': 'gonna'}, {'marker': '☺', 'category': '☺'}, {'marker': 'very', 'category': 'very'}, {'marker': 'any', 'category': 'any'}, {'marker': "i'll", 'category': "i'll"}, {'marker': 'does', 'category': 'does'}, {'marker': 'better', 'category': 'better'}, {'marker': 'thing', 'category': 'thing'}, {'marker': 'where', 'category': 'where'}, {'marker': 'sure', 'category': 'sure'}, {'marker': 'ever', 'category': 'ever'}, {'marker': 'him', 'category': 'him'}, {'marker': 'take', 'category': 'take'}, {'marker': 'haha', 'category': 'haha'}, {'marker': 'night', 'category': 'night'}, {'marker': 'their', 'category': 'their'}, {'marker': '2', 'category': '2'}, {'marker': 'next', 'category': 'next'}, {'marker': 'everyone', 'category': 'everyone'}, {'marker': 'many', 'category': 'many'}, {'marker': 'feel', 'category': 'feel'}, {'marker': 'other', 'category': 'other'}, {'marker': 'always', 'category': 'always'}, {'marker': 'something', 'category': 'something'}, {'marker': 'being', 'category': 'being'}, {'marker': 'lol', 'category': 'lol'}, {'marker': 'music', 'category': 'music'}, {'marker': 'things', 'category': 'things'}, {'marker': 'work', 'category': 'work'}, {'marker': 'mean', 'category': 'mean'}, {'marker': 'off', 'category': 'off'}, {'marker': 'years', 'category': 'years'}, {'marker': 'fun', 'category': 'fun'}, {'marker': 'following', 'category': 'following'}, {'marker': 'life', 'category': 'life'}, {'marker': 'which', 'category': 'which'}, {'marker': 'most', 'category': 'most'}, {'marker': 'watch', 'category': 'watch'}, {'marker': 'though', 'category': 'though'}, {'marker': 'down', 'category': 'down'}, {'marker': "didn't", 'category': "didn't"}, {'marker': 'yeah', 'category': 'yeah'}, {'marker': 'hey', 'category': 'hey'}, {'marker': 'she', 'category': 'she'}, {'marker': 'amazing', 'category': 'amazing'}, {'marker': 'her', 'category': 'her'}, {'marker': 'actually', 'category': 'actually'}, {'marker': 'excited', 'category': 'excited'}, {'marker': 'christmas', 'category': 'christmas'}, {'marker': 'lot', 'category': 'lot'}, {'marker': 'show', 'category': 'show'}, {'marker': 'twitter', 'category': 'twitter'}, {'marker': 'made', 'category': 'made'}, {'marker': 'ok', 'category': 'ok'}, {'marker': 'these', 'category': 'these'}, {'marker': 'done', 'category': 'done'}, {'marker': 'those', 'category': 'those'}, {'marker': 'said', 'category': 'said'}, {'marker': 'hi', 'category': 'hi'}, {'marker': 'ur', 'category': 'ur'}, {'marker': 'tomorrow', 'category': 'tomorrow'}, {'marker': 'big', 'category': 'big'}, {'marker': 'tell', 'category': 'tell'}, {'marker': '....', 'category': '....'}, {'marker': 'thought', 'category': 'thought'}, {'marker': 'dm', 'category': 'dm'}, {'marker': 'again', 'category': 'again'}, {'marker': 'morning', 'category': 'morning'}, {'marker': 'read', 'category': 'read'}, {'marker': 'someone', 'category': 'someone'}, {'marker': 'ricky', 'category': 'ricky'}, {'marker': 'before', 'category': 'before'}, {'marker': 'into', 'category': 'into'}, {'marker': 'give', 'category': 'give'}, {'marker': 'doing', 'category': 'doing'}, {'marker': 'tonight', 'category': 'tonight'}, {'marker': 'world', 'category': 'world'}, {'marker': 'maybe', 'category': 'maybe'}, {'marker': 'man', 'category': 'man'}, {'marker': 'hard', 'category': 'hard'}, {'marker': 'already', 'category': 'already'}, {'marker': '1', 'category': '1'}, {'marker': 'real', 'category': 'real'}, {'marker': 'friends', 'category': 'friends'}, {'marker': 'stop', 'category': 'stop'}, {'marker': 'after', 'category': 'after'}, {'marker': 'bad', 'category': 'bad'}, {'marker': 'little', 'category': 'little'}, {'marker': 'okay', 'category': 'okay'}, {'marker': 'pretty', 'category': 'pretty'}, {'marker': 'every', 'category': 'every'}, {'marker': 'nice', 'category': 'nice'}, {'marker': 'true', 'category': 'true'}, {'marker': 'cute', 'category': 'cute'}, {'marker': 'sorry', 'category': 'sorry'}, {'marker': "doesn't", 'category': "doesn't"}, {'marker': 'soon', 'category': 'soon'}, {'marker': 'long', 'category': 'long'}, {'marker': '😂', 'category': '😂'}, {'marker': 'looks', 'category': 'looks'}, {'marker': '😊', 'category': '😊'}, {'marker': 'cool', 'category': 'cool'}, {'marker': 'might', 'category': 'might'}, {'marker': 'week', 'category': 'week'}, {'marker': 'yet', 'category': 'yet'}, {'marker': '3', 'category': '3'}, {'marker': 'getting', 'category': 'getting'}, {'marker': 'use', 'category': 'use'}, {'marker': 'ready', 'category': 'ready'}, {'marker': 'song', 'category': 'song'}, {'marker': 'anyone', 'category': 'anyone'}, {'marker': 'two', 'category': 'two'}, {'marker': 'let', 'category': 'let'}, {'marker': 'spree', 'category': 'spree'}, {'marker': 'school', 'category': 'school'}, {'marker': "what's", 'category': "what's"}, {'marker': "he's", 'category': "he's"}, {'marker': 'wrong', 'category': 'wrong'}, {'marker': 'both', 'category': 'both'}, {'marker': 'point', 'category': 'point'}, {'marker': "there's", 'category': "there's"}, {'marker': 'coming', 'category': 'coming'}, {'marker': "isn't", 'category': "isn't"}, {'marker': 'miss', 'category': 'miss'}, {'marker': 'check', 'category': 'check'}, {'marker': 'post', 'category': 'post'}, {'marker': 'such', 'category': 'such'}, {'marker': '??', 'category': '??'}, {'marker': 'keep', 'category': 'keep'}, {'marker': 'makes', 'category': 'makes'}, {'marker': 'wanna', 'category': 'wanna'}, {'marker': 'congrats', 'category': 'congrats'}, {'marker': 'meet', 'category': 'meet'}, {'marker': 'put', 'category': 'put'}, {'marker': 'cam', 'category': 'cam'}, {'marker': 'birthday', 'category': 'birthday'}, {'marker': 'favorite', 'category': 'favorite'}, {'marker': 'everything', 'category': 'everything'}, {'marker': 'guess', 'category': 'guess'}, {'marker': "we're", 'category': "we're"}, {'marker': 'x', 'category': 'x'}, {'marker': 'having', 'category': 'having'}]

utterances = []
users = readUserInfo()
tweets = readCSV(inputFile, users, markers)
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
utterances = transformCSVnonP(markers, users, tweets)
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
#tests = ["sadly no. some pasta bake, but coffee and pasta bake is not a contender for tea and toast... .", "Rumbly tummy soon to be tamed by Dominos for lunch! Nom nom nom!"]
#for test in tests:
#	print(test)
#	test = word_feats(test)
#	probs = classifier.prob_classify(test)
#	classification = classifier.classify(test)
#	print(classification)
#	print(probs.prob(classification))
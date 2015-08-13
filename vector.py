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
	toAppend["msgMat"] = createMat(toAppend["msgTokens"])
	toAppend["replyMat"] = createMat(toAppend["replyTokens"])
	return toAppend

def remove_values_from_list(the_list, val):
   return [value for value in the_list if value != val]


# Reads in tweets
def readCSV(inputFile, users):
	reader=csv.reader(open(inputFile,errors="ignore"),dialect="excel-tab")
	next(reader, None)
	utterances = []
	counts = {}
	for i, tweet in enumerate(reader):
		if(i % 10000 is 0):
			logger1.log("On line " + str(i) + " of 230000")
		if(i == 1000):
			break
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

def createMat(tokens):
	mat = {}
	for j, word in enumerate(tokens):
		for k, col in enumerate(tokens):
			if(k == j):
				continue
			if word not in mat:
				mat[word] = {}

			if col not in mat[word]:
				mat[word][col] = {}
				mat[word][col]["score"] = 0.0
				mat[word][col]["num"] = 0.0

			
			mat[word][col]["score"] += 1/(abs(k-j))
			mat[word][col]["num"] += 1.0

	for j, word in enumerate(tokens):
		for k, col in enumerate(tokens):
			if(k == j):
				continue

			if("weighted" in mat[word][col]):
				continue
			mat[word][col]["weighted"] = mat[word][col]["score"]/mat[word][col]["num"]
	return mat

def computeDist(vec1, vec2, words):
	squared = 0
	for word in words:
		if word in vec1:
			colScore1 = vec1[word]["weighted"]
		else:
			colScore1 = 0
		if word in vec2:
			colScore2 = vec2[word]["weighted"]
		else:
			colScore2 = 0
		squared += math.pow(colScore2-colScore1, 2)
	return math.sqrt(squared)


def categorize(tweets):
	categories = ["anyone using google voice? just got my invite, should i ?? don’t know what it is? [url] for the video and break down"]
	for category in categories:
		categoryTokens = category.split(" ")
		categoryMat = createMat(categoryTokens)
		
		for tweet in tweets:
			distances = []
			testTokens = tweet["msgTokens"]
			testMat = createMat(testTokens)
			tokens = list(set(categoryTokens + testTokens))
			

			for token in tokens:
				categoryRow = {}
				testRow = {}
				if(token in categoryMat):
					categoryRow = categoryMat[token]
				if(token in testMat):
					testRow = testMat[token]
				distances.append(computeDist(categoryRow, testRow, tokens))
			distance = sum(distances)/len(distances)
			if(distance < 1.5):
				logger1.log(tweet["msg"])
users = readUserInfo()
tweets = readCSV(inputFile, users)
categorize(tweets)

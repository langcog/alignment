import csv
import operator
import itertools
import pprint 

def readCSV():
	csvFile = "test.csv"
	reader=csv.reader(open(csvFile))
	utterances = []
	for row in reader:
		toAppend = {}
		toAppend["conv#"] = row[0]
		toAppend["msg#"] = row[1]
		toAppend["msg"] = row[2]
		toAppend["msgUserId"] = row[3]
		toAppend["replyContent"] = row[4]
		toAppend["replyUserId"] = row[5]
		utterances.append(toAppend)
	return utterances

def sort(utterances):
	utterances.sort(key=operator.itemgetter('conv#'))
	return utterances

def groupByConvos(seq):
	seq.sort(key = itemgetter(0))
	groups = groupby(seq, itemgetter(0))
	print([[item[0] for item in data] for (key, data) in groups])


utterances = readCSV()
utterances = sort(utterances)

list1 = []
for key, items in itertools.groupby(utterances, operator.itemgetter('conv#')):
    list1.append(list(items))
print(list1)
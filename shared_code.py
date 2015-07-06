import csv

# Just outputs lines to help when debugging
def initialize():
	print("--------------")
	print("--------------")
	print("--------------")

# Writes stuff to the output file
def writeFile(toWrite, outputFile, writeType):
	with open(outputFile, writeType) as f:
		writer = csv.writer(f)
		writer.writerows(toWrite)
	f.close()

# Reads a list of markers from the markersFile
def readMarkers(markersFile):
	reader = csv.reader(open(markersFile))
	markers = []
	for row in reader:
		markers.append(row[0])
	return markers
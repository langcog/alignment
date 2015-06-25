import csv
reader=csv.reader(open(csvFile))
for row in reader:
	print(row)
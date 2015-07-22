import datetime
import time
import traceback

# Just outputs lines to help when debugging
def initialize():
	print("--------------")
	print(datetime.datetime.now().time())
	print("--------------")
	return time.time()

# Prints the name of the function that called log and prints the line number
# Useful for debugging
def log(toPrint):
	print(traceback.extract_stack()[1][2] + " line " + str(traceback.extract_stack()[1][1]) + " at " + str(datetime.datetime.now().time()))
	print(toPrint)
	print("---------")

def finish(start):
	end = time.time()
	print("--------------")
	print("Program finished executing in " + str(end-start) + " seconds")
	print("--------------")
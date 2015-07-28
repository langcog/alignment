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
	stack = traceback.extract_stack()
	stack = stack[0:-1]
	function = stack[-1][2]
	line = stack[-1][1]
	print(str(function) + " line " + str(line) + " at " + str(datetime.datetime.now().time()))
	print(toPrint)
	print("---------")

def finish(start):
	end = time.time()
	print("--------------")
	print("Finished executing after " + str(end-start) + " seconds")
	print("--------------")

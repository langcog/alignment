Bugs/ Special modifications
	Need to change all ","s to "comma" so that the csv file gets parsed properly

Code examples
	- Reading in a csv
		https://docs.python.org/2/library/csv.html
	- Grouping list of dictionaries
		https://www.daniweb.com/software-development/python/code/216750/group-a-list-of-dictionaries-python

TO DO:
	Currently, we discard conversations with where a person replies to himself/herself
	Aggregation of markers
	Aggregations of conversations
	Create a list of markers

6/26/2015
Derived formula for Echoes of Power:
	powerProb = ((# of times marker is said by A and B)*(# of words A says to B))/((# of words B says to A)*(# of times A says the marker))
	baseProb = (# of times B says the marker)/(# of words B says)
	prob = powerProb - baseProb

6/29/2015
Figured out that the formula we were working off of was incorrect.
Instead of calculating probabilities using tokens, we need to calculate probabilites using utterances
	powerProb = (# of utterances where A and B both say marker)/(Number of utterances where A says the marker)
	baseProb = (# of utterances where B says the marker)/(# of utterances)
	prob = powerProb - baseProb
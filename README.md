# alignment
Conversational alignment

Code examples
    - Reading in a csv
        https://docs.python.org/2/library/csv.html
    - Grouping list of dictionaries
        https://www.daniweb.com/software-development/python/code/216750/group-a-list-of-dictionaries-python

Formulas
    - TRUE_POWER (Outputs the expected results)
    - DNM (Formula used in [Echoes of Power](http://www.cs.cornell.edu/~cristian/Echoes_of_power_files/echoes_of_power.pdf)

## changelog

**6/26/2015**

Derived formula for Echoes of Power:
    powerProb = ((# of times marker is said by A and B)*(# of words A says to B))/((# of words B says to A)*(# of times A says the marker))
    baseProb = (# of times B says the marker)/(# of words B says)
    prob = powerProb - baseProb

**6/29/2015**

Figured out that the formula we were working off of was incorrect.

Instead of calculating probabilities using tokens, we need to calculate probabilites using utterances
    powerProb = (# of utterances where A and B both say marker)/(Number of utterances where A says the marker)
    baseProb = (# of utterances where B says the marker)/(# of utterances that B says to A)
    prob = powerProb - baseProb

**7/13/2015**

Figured out that the formula we were working off of was incorrect.

Our new formula is:

powerProb = log((# of utterances where A and B both say marker)/(Number of utterances where A says the marker))
baseProb = log((# of utterances where B says the marker and A doesn't)/(# of utterances that A doesn't say the marker))
prob = powerProb - baseProb

**7/20/2015**

SHARED_CODE:
    Fixed a bug which was reducing our BNOTA

**7/22/2015**

SHARED_CODE:
    Added DNM formula option and options to change smoothing values

**7/23/2015**

TWTR:
    Updated readCSV to discard more utterances which quote a speaker

    - Check for unicode quotes
    - Check if speakers acknowledge each other
    - Check if utterance contains "RT"
    - Check if utterance contains "[mention]: "
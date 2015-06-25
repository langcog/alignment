Message = utterance
Ste ps:
1) Read in csv file
2) Close csv file
2) Split text up by conversations
	a) For twitter, use a groupBy function on conversationId
	b) For CHILDIS, ???
3) For each conversation
	a) For each message in messages
		Concat messages from person A together
		Concat messages from person B together
	b) For each M in markers
		a) Find out probability that A says M
		b) Find out probability that B says M
	c) For each message in the messages
		1) Find out if the message has a parent
		2) Find out if the message has any markers
		3) Find out if the parent message has any markers
		4) For each M in markers
			a) Calculate the probability that B said marker M given that A said marker M
				1) We already know the probability that B says M
				2) We already know the probability that A says M
				3) return P(B says M)*P(A says M)/(P(B says M))
			b) Aggregate the results of 3.c.4.a together

P(B says M | A says M) = P(A says M and B says M)/P(B says M)

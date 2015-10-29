library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


num_markers <- 1
num_people <- 3
num_utterances_ab <- matrix(c(0, 5, 5, 5, 0, 5, 5, 5, 0), nrow = 3)
num_utterances_notab <- num_utterances_ab
# counts_ab <- array(c(0, 3, 1, 4, 0, 3, 4, 5, 0), 
#                    dim = c(num_people, num_people, num_markers))
# counts_notab <- array(c(0, 3, 1, 4, 0, 0, 4, 3, 0),
#                       dim = c(num_people, num_people, num_markers))

counts_ab <- array(c(0, 4, 4, 3, 0, 5, 1, 3, 0), 
                   dim = c(num_people, num_people, num_markers))
counts_notab <- array(c(0, 4, 4, 3, 0, 3, 1, 0, 0),
                      dim = c(num_people, num_people, num_markers))



alignment_data <- list(NumMarkers = num_markers,
                       NumPeople = num_people,
                       NumUtterancesAB = num_utterances_ab,
                       NumUtterancesNotAB = num_utterances_notab,
                       CountsAB = counts_ab,
                       CountsNotAB = counts_notab)


fit <- stan(file = 'alignment.stan', data = alignment_data, 
            iter = 1000, chains = 1 )
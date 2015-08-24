library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


num_markers <- 2
num_people <- 3
num_utterances <- c(4,4,4)
counts <- matrix(c(1,1,1,3,3,3), nrow=3, ncol = 2)

alignment_data <- list(NumMarkers = num_markers,
                       NumPeople = num_people,
                       NumUtterances = num_utterances,
                       Counts = counts)


fit <- stan(file = 'alignment.stan', data = alignment_data, 
            iter = 1000, chains = 1 )
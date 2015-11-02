#This is the version of the HAM framework used in Doyle, Yurovsky, & Frank 2016 (WWW)
# Baseline prior is uniform, alignment prior is normal
# hierarchy is: marker, marker+group, marker+dyad

data {
  int<lower=0> NumMarkers;                              #Number of marker categories in data
  int<lower=0> NumSubPops;                              #Number of groups in the data
  int<lower=0> NumObservations;                         #Number of marker-dyad observations
  int<lower=0> SpeakerSubPop[NumObservations];          #Group number for each observation
  int<lower=0> MarkerType[NumObservations];             #Marker number for each observation
  int<lower=0> NumUtterancesAB[NumObservations];        #Number of times the first message didn't contain the marker
  int<lower=0> NumUtterancesNotAB[NumObservations];     #Number of times the first message did contain the marker
  int<lower=0> CountsAB[NumObservations];               #Number of times the first message didn't contain the marker but the reply did
  int<lower=0> CountsNotAB[NumObservations];            #Number of times the first message did contain the marker and the reply did
  real<lower=0> StdDev;                                 #SD for each normal dist in the hierarchy (sole free parameter)
}

parameters {
  real eta_pop[NumMarkers];                             #linear predictor for each marker's baseline
  real eta_ab_pop[NumMarkers];                          #linear predictor for each marker's alignment
  vector[NumMarkers] eta_subpop[NumSubPops];            #lin. pred. for each marker+group baseline
  vector[NumMarkers] eta_ab_subpop[NumSubPops];         #lin. pred. for each marker+group alignment
  vector[NumObservations] eta_observation;              #lin. pred. for each marker+dyad baseline
  vector[NumObservations] eta_ab_observation;           #lin. pred. for each marker+dyad alignment
}

transformed parameters {
  vector<lower=0,upper=1>[NumObservations] mu_notab;    #inv-logit transform of lin. pred. into probability space
  vector<lower=0,upper=1>[NumObservations] mu_ab;

  for (Observation in 1:NumObservations) {
    mu_notab[Observation] <- inv_logit(eta_observation[Observation]);
    mu_ab[Observation] <- inv_logit(eta_ab_observation[Observation]+eta_observation[Observation]);
  }
}

model {
  #marker level distributions
  eta_ab_pop ~ normal(0,StdDev);
  eta_pop ~ uniform(-5,5);                    #Note that this distribution may be changed if baselines are expected to be very high/low

  #marker-group level distributions
  for(SubPop in 1:NumSubPops) {
    eta_subpop[SubPop] ~ normal(eta_pop,StdDev);
    eta_ab_subpop[SubPop] ~ normal(eta_ab_pop,StdDev);
  }

  #marker-dyad level distributions
  for(Observation in 1:NumObservations) {
    eta_observation[Observation] ~ normal(eta_subpop[SpeakerSubPop[Observation],MarkerType[Observation]],StdDev);
    eta_ab_observation[Observation] ~ normal(eta_ab_subpop[SpeakerSubPop[Observation],MarkerType[Observation]],StdDev);
  }

  #drawing reply usage counts given number of msg-reply pairs
  CountsAB ~ binomial(NumUtterancesAB,mu_ab);
  CountsNotAB ~ binomial(NumUtterancesNotAB,mu_notab);
}
    

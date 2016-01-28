#This is the version of the HAM framework used in Yurovsky, Doyle, & Frank 2016 (Cogsci Conference)
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
  real eta_ab_pop;
  real eta_pop_Marker[NumMarkers];                      #linear predictor for each marker's baseline
  real eta_ab_subpop[NumSubPops];
  vector[NumMarkers] eta_subpop_Marker[NumSubPops];            #lin. pred. for each marker+group baseline
  vector[NumMarkers] eta_ab_subpop_Marker[NumSubPops];         #lin. pred. for each marker+group alignment
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
  #top level alignment
  eta_ab_pop ~ normal(0, StdDev);
  eta_ab_subpop ~ normal(eta_ab_pop, StdDev);

  #marker level distributions
  #eta_ab_pop_Marker ~ normal(eta_ab_pop, StdDev);
  eta_pop_Marker ~ uniform(-5,5);     #Note that this distribution may be changed if baselines are expected to be very high/low

  #marker-group level distributions
  for(SubPop in 1:NumSubPops) {
    eta_subpop_Marker[SubPop] ~ normal(eta_pop_Marker, StdDev);
    eta_ab_subpop_Marker[SubPop] ~ normal(eta_ab_subpop[SubPop], StdDev);
  }

  #marker-dyad level distributions
  for(Observation in 1:NumObservations) {
    eta_observation[Observation] ~ normal(eta_subpop_Marker[SpeakerSubPop[Observation], MarkerType[Observation]], StdDev);
    eta_ab_observation[Observation] ~ normal(eta_ab_subpop_Marker[SpeakerSubPop[Observation], MarkerType[Observation]], StdDev);
  }

  #drawing reply usage counts given number of msg-reply pairs
  CountsAB ~ binomial(NumUtterancesAB, mu_ab);
  CountsNotAB ~ binomial(NumUtterancesNotAB, mu_notab);
}
    

data {
  int<lower=0> NumMarkers;
  int<lower=0> NumSubPops;
  int<lower=0> NumObservations;
  int<lower=0> SpeakerSubPop[NumObservations];
  int<lower=0> MarkerType[NumObservations];
  int<lower=0> NumUtterancesAB[NumObservations];
  int<lower=0> NumUtterancesNotAB[NumObservations];
  int<lower=0> CountsAB[NumObservations];
  int<lower=0> CountsNotAB[NumObservations];

}

parameters {
  real m_pop;
  real m_ab_pop;
  real eta_pop[NumMarkers];
  real eta_ab_pop[NumMarkers];
  vector[NumMarkers] eta_subpop[NumSubPops];
  vector[NumMarkers] eta_ab_subpop[NumSubPops];
  vector[NumObservations] eta_observation;
  vector[NumObservations] eta_ab_observation;
}

transformed parameters {
  vector<lower=0,upper=1>[NumObservations] mu_notab;
  vector<lower=0,upper=1>[NumObservations] mu_ab;

  for (Observation in 1:NumObservations) {
    mu_notab[Observation] <- inv_logit(eta_observation[Observation]);
    mu_ab[Observation] <- inv_logit(eta_ab_observation[Observation]+eta_observation[Observation]);
  }
#  mu_notab <- 1 ./ (1+exp(-eta_observation));
#  mu_ab <- 1 ./ (1+exp(- eta_observation - eta_ab_observation));

}

model {

  # population level
  m_ab_pop ~ normal(0,1);
  m_pop ~ normal(0,1);

  #marker level
  eta_ab_pop ~ normal(m_ab_pop,1);
  eta_pop ~ normal(m_pop,1);

  #subpop level
  for(SubPop in 1:NumSubPops) {
    eta_subpop[SubPop] ~ normal(eta_pop,1.0);
    eta_ab_subpop[SubPop] ~ normal(eta_ab_pop,1.0);
  }

  # replier level DRAW AS A NORMAL
  for(Observation in 1:NumObservations) {
  #  eta_observation[Observation] <- eta_subpop[SpeakerSubPop[Observation],MarkerType[Observation]];
  #  eta_ab_observation[Observation] <- eta_ab_subpop[SpeakerSubPop[Observation],MarkerType[Observation]];
    eta_observation[Observation] ~ normal(eta_subpop[SpeakerSubPop[Observation],MarkerType[Observation]],1.0);
    eta_ab_observation[Observation] ~ normal(eta_ab_subpop[SpeakerSubPop[Observation],MarkerType[Observation]],1.0);
  }

  #replier level
  CountsAB ~ binomial(NumUtterancesAB,mu_ab);
  CountsNotAB ~ binomial(NumUtterancesNotAB,mu_notab);
}
    

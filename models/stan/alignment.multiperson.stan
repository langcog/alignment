data {
  int<lower=0> NumMarkers;
  int<lower=0> NumSubPops;
  int<lower=0> NumRepliers;
  int<lower=0> NumUtterancesAB[NumSubPops,NumRepliers,NumMarkers];
  int<lower=0> NumUtterancesNotAB[NumSubPops,NumRepliers,NumMarkers];
  int<lower=0> CountsAB[NumSubPops,NumRepliers,NumMarkers];
  int<lower=0> CountsNotAB[NumSubPops,NumRepliers,NumMarkers];
}

parameters {
  real m_pop;
  real m_ab_pop;
  real eta_pop[NumMarkers];
  real eta_ab_pop[NumMarkers];
  real eta_subpop[NumSubPops,NumRepliers,NumMarkers];
  real eta_ab_subpops[NumSubPops,NumRepliers,NumMarkers];
}

transformed parameters {
  real<lower=0,upper=1> mu_notab[NumSubPops,NumRepliers,NumMarkers];
  real<lower=0,upper=1> mu_ab[NumSubPops,NumRepliers,NumMarkers];

  mu_notab <- inv_logit(eta_subpop);
  mu_ab <- inv_logit(eta_subpop + eta_ab_subpops);
}

model {
  m_ab_pop ~ normal(0,1);
  m_pop ~ normal(0,1);

  for (Marker in 1:NumMarkers) {
    eta_ab_pop[Marker] ~ normal(m_ab_pop,1);
    eta_pop[Marker] ~ normal(m_pop,1);

    for (SpeakerSubPop in 1:NumSubPops) {
      for (ReplierSubPop in 1:NumRepliers) {
        eta_subpop[SpeakerSubPop,ReplierSubPop,Marker] ~ normal(eta_pop[Marker],1);
        eta_ab_subpops[SpeakerSubPop,ReplierSubPop,Marker] ~ normal(eta_ab_pop[Marker],1);

        CountsAB[SpeakerSubPop,ReplierSubPop,Marker] ~ binomial(NumUtterancesAB[SpeakerSubPop,ReplierSubPop,Marker],
                                           mu_ab[SpeakerSubPop,ReplierSubPop,Marker]);

        CountsNotAB[SpeakerSubPop,ReplierSubPop,Marker] ~ binomial(NumUtterancesNotAB[SpeakerSubPop,ReplierSubPop,Marker],
                                              mu_notab[SpeakerSubPop,ReplierSubPop,Marker]);
      }
    }
  }
}
    

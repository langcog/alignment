data {
  int<lower=0> NumMarkers;
  int<lower=0> NumSubPops;
  int<lower=0> NumUtterancesAB[NumSubPops,NumSubPops,NumMarkers];
  int<lower=0> NumUtterancesNotAB[NumSubPops,NumSubPops,NumMarkers];
  int<lower=0> CountsAB[NumSubPops,NumSubPops,NumMarkers];
  int<lower=0> CountsNotAB[NumSubPops,NumSubPops,NumMarkers];
}

parameters {
  real eta_subpop_m[NumSubPops,NumSubPops,NumMarkers];
  real eta_ab_subpop_m[NumSubPops,NumSubPops,NumMarkers];
  real eta_pop;
  real eta_ab_pop;
  real eta_subpop[NumSubPops,NumSubPops];
  real eta_ab_subpop[NumSubPops,NumSubPops];
}

transformed parameters {
  real<lower=0,upper=1> mu_notab[NumSubPops,NumSubPops,NumMarkers];
  real<lower=0,upper=1> mu_ab[NumSubPops,NumSubPops,NumMarkers];

  for (Marker in 1:NumMarkers) {
   for (SpeakerSubPop in 1:NumSubPops) {
      for (ReplierSubPop in 1:NumSubPops) {
        mu_notab[SpeakerSubPop,ReplierSubPop,Marker] <- inv_logit(eta_subpop_m[SpeakerSubPop,ReplierSubPop,Marker]);
        mu_ab[SpeakerSubPop,ReplierSubPop,Marker] <- inv_logit(eta_subpop_m[SpeakerSubPop,ReplierSubPop,Marker] +
                                                             eta_ab_subpop_m[SpeakerSubPop,ReplierSubPop,Marker]);
      }
    }
  }
}

model {
  eta_ab_pop ~ normal(0,1);
  eta_pop ~ normal(0,1);

    for (SpeakerSubPop in 1:NumSubPops) {
      for (ReplierSubPop in 1:NumSubPops) {
        eta_ab_subpop[SpeakerSubPop,ReplierSubPop] ~ normal(eta_ab_pop,1);
        eta_subpop[SpeakerSubPop,ReplierSubPop] ~ normal(eta_pop,1);
        for (Marker in 1:NumMarkers) {
        eta_subpop_m[SpeakerSubPop,ReplierSubPop,Marker] ~ normal(eta_subpop[SpeakerSubPop,ReplierSubPop],1);
        eta_ab_subpop_m[SpeakerSubPop,ReplierSubPop,Marker] ~ normal(eta_ab_subpop[SpeakerSubPop,ReplierSubPop],1);

        CountsAB[SpeakerSubPop,ReplierSubPop,Marker] ~ binomial(NumUtterancesAB[SpeakerSubPop,ReplierSubPop,Marker],
                                           mu_ab[SpeakerSubPop,ReplierSubPop,Marker]);

        CountsNotAB[SpeakerSubPop,ReplierSubPop,Marker] ~ binomial(NumUtterancesNotAB[SpeakerSubPop,ReplierSubPop,Marker],
                                              mu_notab[SpeakerSubPop,ReplierSubPop,Marker]);
      }
    }
  }
}
    

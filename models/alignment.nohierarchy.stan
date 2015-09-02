data {
  int<lower=0> NumSubPops;
  int<lower=0> NumUtterancesAB[NumSubPops,NumSubPops];
  int<lower=0> NumUtterancesNotAB[NumSubPops,NumSubPops];
  int<lower=0> CountsAB[NumSubPops,NumSubPops];
  int<lower=0> CountsNotAB[NumSubPops,NumSubPops];
}

parameters {
  real eta_pop;
  real eta_ab_pop;
  real eta_subpop[NumSubPops];
  real eta_ab_subpops[NumSubPops,NumSubPops];
  real<lower=0> n_pop;
  real<lower=0> n_subpop[NumSubPops];
  real<lower=0,upper=1> thetaA[NumSubPops,NumSubPops];
  real<lower=0,upper=1> thetaNotA[NumSubPops,NumSubPops];
}

transformed parameters {
  real<lower=0,upper=1> mu_subpop[NumSubPops];
  real<lower=0> mu_ab[NumSubPops,NumSubPops];
  real<lower=0> alphaA[NumSubPops,NumSubPops];
  real<lower=0> alphaNotA[NumSubPops,NumSubPops];
  real<lower=0> betaA[NumSubPops,NumSubPops];
  real<lower=0> betaNotA[NumSubPops,NumSubPops];

  for (SubPop in 1:NumSubPops) {
     mu_subpop[SubPop] <- inv_logit(eta_subpop[SubPop]);
  }

   for (SpeakerSubPop in 1:NumSubPops) {
      for (ReplierSubPop in 1:NumSubPops) {
        mu_ab[SpeakerSubPop,ReplierSubPop] <- inv_logit(eta_subpop[ReplierSubPop] + eta_ab_subpops[SpeakerSubPop,ReplierSubPop]);
        alphaA[SpeakerSubPop,ReplierSubPop] <- mu_ab[SpeakerSubPop,ReplierSubPop] * n_subpop[ReplierSubPop];
        alphaNotA[SpeakerSubPop,ReplierSubPop] <- mu_subpop[ReplierSubPop] * n_subpop[ReplierSubPop];
        betaA[SpeakerSubPop,ReplierSubPop] <- (1 -  mu_ab[SpeakerSubPop,ReplierSubPop]) *  n_subpop[ReplierSubPop];
        betaNotA[SpeakerSubPop,ReplierSubPop] <- (1 - mu_subpop[ReplierSubPop]) *  n_subpop[ReplierSubPop];
   }
  }
}

model {
  eta_ab_pop ~ normal(0,1);
  eta_pop ~ normal(0,1);
  n_pop ~ gamma(5,2);

  for (SubPop in 1:NumSubPops) {
     eta_subpop[SubPop] ~ normal(eta_pop,1);
     n_subpop[SubPop] ~ gamma(5,5/n_pop);
    }

  for (SpeakerSubPop in 1:NumSubPops) {
    for (ReplierSubPop in 1:NumSubPops) {
      eta_ab_subpops[SpeakerSubPop,ReplierSubPop] ~ normal(eta_ab_pop,1);

      thetaA[SpeakerSubPop,ReplierSubPop] ~ beta(alphaA[SpeakerSubPop,ReplierSubPop], betaA[SpeakerSubPop,ReplierSubPop]);
      thetaNotA[SpeakerSubPop,ReplierSubPop] ~ beta(alphaNotA[SpeakerSubPop,ReplierSubPop], betaNotA[SpeakerSubPop,ReplierSubPop]);

      CountsAB[SpeakerSubPop,ReplierSubPop] ~ binomial(NumUtterancesAB[SpeakerSubPop,ReplierSubPop],
                                         thetaA[SpeakerSubPop,ReplierSubPop]);

      CountsNotAB[SpeakerSubPop,ReplierSubPop] ~ binomial(NumUtterancesNotAB[SpeakerSubPop,ReplierSubPop],
                                            thetaNotA[SpeakerSubPop,ReplierSubPop]);
    }
  }
}
    

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
  real eta_subpop[NumSubPops,NumSubPops];
  real eta_ab_subpops[NumSubPops,NumSubPops];
  real<lower=0> n_pop;
  real<lower=0> n_subpop[NumSubPops];
}

transformed parameters {
  real<lower=0,upper=1> mu_notab[NumSubPops,NumSubPops];
  real<lower=0,upper=1> mu_ab[NumSubPops,NumSubPops];


   for (SpeakerSubPop in 1:NumSubPops) {
      for (ReplierSubPop in 1:NumSubPops) {
        mu_notab[SpeakerSubPop,ReplierSubPop] <- inv_logit(eta_subpop[SpeakerSubPop,ReplierSubPop]);
        mu_ab[SpeakerSubPop,ReplierSubPop] <- inv_logit(eta_subpop[SpeakerSubPop,ReplierSubPop] + eta_ab_subpops[SpeakerSubPop,ReplierSubPop]);
       
   }
  }
}

model {
  eta_ab_pop ~ normal(0,1);
  eta_pop ~ normal(0,1);
  n_pop ~ gamma(500,2);

  for (SubPop in 1:NumSubPops) {
     n_subpop[SubPop] ~ gamma(500,500/n_pop);
    }

  for (SpeakerSubPop in 1:NumSubPops) {
    for (ReplierSubPop in 1:NumSubPops) {
      eta_subpop[SpeakerSubPop,ReplierSubPop] ~ normal(eta_pop,1);
      eta_ab_subpops[SpeakerSubPop,ReplierSubPop] ~ normal(eta_ab_pop,1);

      CountsAB[SpeakerSubPop,ReplierSubPop] ~ binomial(NumUtterancesAB[SpeakerSubPop,ReplierSubPop],
                                         mu_ab[SpeakerSubPop,ReplierSubPop]);

      CountsNotAB[SpeakerSubPop,ReplierSubPop] ~ binomial(NumUtterancesNotAB[SpeakerSubPop,ReplierSubPop],
                                            mu_notab[SpeakerSubPop,ReplierSubPop]);
    }
  }
}
    

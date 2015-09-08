data {
  int<lower=0> NumMarkers;
  int<lower=0> NumSubPops;
  int<lower=0> NumRepliers;
  int<lower=0> NumSpeakers;
  int<lower=0> NumPairs;
  int<lower=0> SpeakerSubPop[NumSpeakers];
  int<lower=0> ReplierSubPop[NumRepliers];
  int<lower=0> ReplierID[NumPairs];
  int<lower=0> SpeakerID[NumPairs];
  int<lower=0> NumUtterancesAB[NumPairs,NumMarkers];
  int<lower=0> NumUtterancesNotAB[NumPairs,NumMarkers];
  int<lower=0> CountsAB[NumPairs,NumMarkers];
  int<lower=0> CountsNotAB[NumPairs,NumMarkers];
}

parameters {
  real eta_pop;
  real eta_ab_pop;
  real eta_subpop[NumSubPops,NumSubPops];
  real eta_ab_subpop[NumSubPops,NumSubPops];
  real eta_subpop_m[NumSubPops,NumSubPops,NumMarkers];
  real eta_ab_subpop_m[NumSubPops,NumSubPops,NumMarkers];
  real<lower=0> N_pop;
  real<lower=0> N_subpop[NumSubPops];
  real<lower=0,upper=1> mu_si_rj_m[NumPairs,NumMarkers];
  real<lower=0,upper=1> mu_ab_si_rj_m[NumPairs,NumMarkers];
  
}

transformed parameters {
  real<lower=0,upper=1> mu_notab[NumSubPops,NumSubPops,NumMarkers];
  real<lower=0,upper=1> mu_ab[NumSubPops,NumSubPops,NumMarkers];

  real<lower=0> alpha_s_rj_m[NumSubPops,NumSubPops,NumMarkers];
  real<lower=0> beta_s_rj_m[NumSubPops,NumSubPops,NumMarkers];
  real<lower=0> alpha_ab_s_rj_m[NumSubPops,NumSubPops,NumMarkers];
  real<lower=0> beta_ab_s_rj_m[NumSubPops,NumSubPops,NumMarkers];
  

  for (Marker in 1:NumMarkers) {
   for (SSubPop in 1:NumSubPops) {
      for (RSubPop in 1:NumSubPops) {
        mu_notab[SSubPop,RSubPop,Marker] <- inv_logit(eta_subpop_m[SSubPop,RSubPop,Marker]);
        mu_ab[SSubPop,RSubPop,Marker] <- inv_logit(eta_subpop_m[SSubPop,RSubPop,Marker] +
                                                             eta_ab_subpop_m[SSubPop,RSubPop,Marker]);

        alpha_s_rj_m[SSubPop,RSubPop,Marker] <- mu_notab[SSubPop,RSubPop,Marker]*N_subpop[RSubPop];
        beta_s_rj_m[SSubPop,RSubPop, Marker] <- (1-mu_notab[SSubPop,RSubPop,Marker])*N_subpop[RSubPop];
        alpha_ab_s_rj_m[SSubPop,RSubPop,Marker] <- mu_ab[SSubPop,RSubPop,Marker]*N_subpop[RSubPop];
        beta_ab_s_rj_m[SSubPop,RSubPop,Marker] <- (1-mu_ab[SSubPop,RSubPop,Marker])*N_subpop[RSubPop];
        
      }
    }
  }
}

model {
  eta_ab_pop ~ normal(0,1);
  eta_pop ~ normal(0,1);
  N_pop ~ gamma(5,2);

  for (SubPop in 1:NumSubPops) {
    N_subpop[SubPop] ~ gamma(5,5/N_pop);
  }

  for (SSubPop in 1:NumSubPops) {
    for (RSubPop in 1:NumSubPops) {
      eta_ab_subpop[SSubPop,RSubPop] ~ normal(eta_ab_pop,1);
      eta_subpop[SSubPop,RSubPop] ~ normal(eta_pop,1);
 
      for (Marker in 1:NumMarkers) {
      eta_subpop_m[SSubPop,RSubPop,Marker] ~ normal(eta_subpop[SSubPop,RSubPop],1);
      eta_ab_subpop_m[SSubPop,RSubPop,Marker] ~ normal(eta_ab_subpop[SSubPop,RSubPop],1);
    }
  }

  for (Pair in 1:NumPairs) {
      for (Marker in 1:NumMarkers) {
        mu_si_rj_m[Pair,Marker] ~ beta(alpha_s_rj_m[SpeakerSubPop[SpeakerID[Pair]],ReplierSubPop[ReplierID[Pair]],Marker],
                                        beta_s_rj_m[SpeakerSubPop[SpeakerID[Pair]],ReplierSubPop[ReplierID[Pair]],Marker]);
        mu_ab_si_rj_m[Pair,Marker] ~ beta(alpha_ab_s_rj_m[SpeakerSubPop[SpeakerID[Pair]],ReplierSubPop[ReplierID[Pair]],Marker],
                                          beta_ab_s_rj_m[SpeakerSubPop[SpeakerID[Pair]],ReplierSubPop[ReplierID[Pair]],Marker]);

        CountsAB[Pair,Marker] ~ binomial(NumUtterancesAB[Pair,Marker],
                                         mu_ab_si_rj_m[Pair,Marker]);

        CountsNotAB[Pair,Marker] ~ binomial(NumUtterancesNotAB[Pair,Marker],
                                         mu_si_rj_m[Pair,Marker]);
      }
    }
  }

}
    

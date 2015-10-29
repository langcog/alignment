data {
  int<lower=0> NumSubPops;
  int<lower=0> NumPeople;
  int<lower=0> NumPairs;
  int<lower=0> NumUtterancesAB[NumPairs];
  int<lower=0> NumUtterancesNotAB[NumPairs];
  int<lower=0> CountsAB[NumPairs];
  int<lower=0> CountsNotAB[NumPairs];
  int<lower=0> Speaker[NumPairs];
  int<lower=0> Replier[NumPairs];
  int<lower=0> SpeakerType[NumPairs];
  int<lower=0> ReplierType[NumPairs];
}

parameters {
  real eta_pop;
  real eta_person[NumPeople];
  real eta_ab_pop;
  real eta_ab_subpops[NumSubPops,NumSubPops];
  real<lower=0> n_pop;
  real<lower=0> n_person[NumPeople];
  real<lower=0,upper=1> thetaA[NumPairs];
  real<lower=0,upper=1> thetaNotA[NumPairs];
}

transformed parameters {
  real<lower=0,upper=1> mu_person[NumPeople];
  real<lower=0> mu_ab[NumPairs];
  real<lower=0> alphaA[NumPairs];
  real<lower=0> alphaNotA[NumPairs];
  real<lower=0> betaA[NumPairs];
  real<lower=0> betaNotA[NumPairs];

  for (Person in 1:NumPeople) {
     mu_person[Person] <- inv_logit(eta_person[Person] + eta_pop);
  }

  for (Pair in 1:NumPairs) {
    mu_ab[Pair] <- inv_logit(eta_pop + eta_person[Replier[Pair]] + eta_ab_pop + eta_ab_subpops[SpeakerType[Pair],ReplierType[Pair]]);
    alphaA[Pair] <- mu_ab[Pair] * n_person[Replier[Pair]];
    alphaNotA[Pair] <- mu_person[Replier[Pair]] * n_person[Replier[Pair]];
    betaA[Pair] <- (1 - mu_ab[Pair]) * n_person[Replier[Pair]];
    betaNotA[Pair] <- (1 - mu_person[Replier[Pair]]) * n_person[Replier[Pair]];
 }
}

model {
  eta_ab_pop ~ normal(0,1);
  eta_pop ~ normal(0,1);
  n_pop ~ gamma(50,2);

  for (Person in 1:NumPeople) {
     eta_person[Person] ~ normal(0,1);
     n_person[Person] ~ gamma(50,50/n_pop);
    }

  for (SpeakerSubPop in 1:NumSubPops) {
    for (ReplierSubPop in 1:NumSubPops) {
      eta_ab_subpops[SpeakerSubPop,ReplierSubPop] ~ normal(0,1);
    }
  }

  for (Pair in 1:NumPairs) {
    thetaA[Pair] ~ beta(alphaA[Pair], betaA[Pair]);
    thetaNotA[Pair] ~ beta(alphaNotA[Pair], betaNotA[Pair]);

    CountsAB[Pair] ~ binomial(NumUtterancesAB[Pair],
                                         thetaA[Pair]);

    CountsNotAB[Pair] ~ binomial(NumUtterancesNotAB[Pair],
                                            thetaNotA[Pair]);
  }
}
    

data {
  int<lower=0> NumPeople;
  int<lower=0> NumPairs;
  int<lower=0> NumUtterancesAB[NumPairs];
  int<lower=0> NumUtterancesNotAB[NumPairs];
  int<lower=0> CountsAB[NumPairs];
  int<lower=0> CountsNotAB[NumPairs];
  int<lower=0> Speaker[NumPairs];
  int<lower=0> Replier[NumPairs];
}

parameters {
  real eta_ab_pop;
  real eta_pop;
  real eta_person[NumPeople];
  real<lower=0> n_pop;
  real<lower=0> n_person[NumPeople];
  real<lower=0,upper=1> thetaA[NumPairs];
  real<lower=0,upper=1> thetaNotA[NumPairs];
  real eta_ab[NumPairs];

}

transformed parameters {
  real<lower=0,upper=1> mu_person[NumPeople];
  real<lower=0> mu_ab[NumPairs];
  real<lower=0> alphaA[NumPairs];
  real<lower=0> alphaNotA[NumPairs];
  real<lower=0> betaA[NumPairs];
  real<lower=0> betaNotA[NumPairs];

  for (Person in 1:NumPeople) {
     mu_person[Person] <- inv_logit(eta_person[Person]);
  }

  for (Pair in 1:NumPairs) {
    mu_ab[Pair] <- inv_logit(eta_person[Replier[Pair]] + eta_ab[Pair]);
    alphaA[Pair] <- mu_ab[Pair] * n_person[Replier[Pair]];
    alphaNotA[Pair] <- mu_person[Replier[Pair]] * n_person[Replier[Pair]];
    betaA[Pair] <- (1 - mu_ab[Pair]) * n_person[Replier[Pair]];
    betaNotA[Pair] <- (1 - mu_person[Replier[Pair]]) * n_person[Replier[Pair]];

 }
}

model {
  eta_ab_pop ~ normal(0,1);
  eta_pop ~ normal(0,1);
  n_pop ~ gamma(5,2);

  for (Person in 1:NumPeople) {
     eta_person[Person] ~ normal(eta_pop,1);
     n_person[Person] ~ gamma(5,5/n_pop);
    }

  for (Pair in 1:NumPairs) {
    eta_ab[Pair] ~ normal(eta_ab_pop,1);
  
    thetaA[Pair] ~ beta(alphaA[Pair], betaA[Pair]);
    thetaNotA[Pair] ~ beta(alphaNotA[Pair], betaNotA[Pair]);

    CountsAB[Pair] ~ binomial(NumUtterancesAB[Pair],
                                         thetaA[Pair]);

    CountsNotAB[Pair] ~ binomial(NumUtterancesNotAB[Pair],
                                            thetaNotA[Pair]);
  }
}
    

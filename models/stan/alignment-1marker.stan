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
  real<lower=0> alpha_pop;
  real<lower=0> beta_pop;
  real<lower=0> alpha_person[NumPeople];
  real<lower=0> beta_person[NumPeople];
  real<lower=0,upper=1> thetaA[NumPairs];
  real<lower=0,upper=1> thetaNotA[NumPairs];
  real<lower=0>pos_inf[NumPairs];
  real<lower=0>neg_inf[NumPairs];
}

model {
  for (Person in 1:NumPeople) {
      alpha_person[Person] ~ exponential(1);
      beta_person[Person] ~ exponential(1);
    }

  for (Pair in 1:NumPairs) {
    pos_inf[Pair] ~ exponential(1);
    neg_inf[Pair] ~ exponential(1);


    thetaA[Pair] ~ beta(alpha_pop + alpha_person[Replier[Pair]] +
                                   pos_inf[Pair],
                                   beta_pop + beta_person[Replier[Pair]] + 
                                   neg_inf[Pair]);
    thetaNotA[Pair] ~ beta(alpha_pop + alpha_person[Replier[Pair]],
                                      beta_pop + beta_person[Replier[Pair]]);

    CountsAB[Pair] ~ binomial(NumUtterancesAB[Pair],
                                         thetaA[Pair]);

    CountsNotAB[Pair] ~ binomial(NumUtterancesNotAB[Pair],
                                            thetaNotA[Pair]);
  }
}
    

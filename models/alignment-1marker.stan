data {
  int<lower=0> NumPeople;
  int<lower=0> NumUtterancesAB[NumPeople,NumPeople];
  int<lower=0> NumUtterancesNotAB[NumPeople,NumPeople];
  int<lower=0> CountsAB[NumPeople,NumPeople];
  int<lower=0> CountsNotAB[NumPeople,NumPeople];
}

parameters {
  real<lower=0> alpha_pop;
  real<lower=0> beta_pop;
  real<lower=0> alpha_person[NumPeople];
  real<lower=0> beta_person[NumPeople];
  real<lower=0,upper=1> thetaA[NumPeople,NumPeople];
  real<lower=0,upper=1> thetaNotA[NumPeople,NumPeople];
  real<lower=0>pos_inf[NumPeople,NumPeople];
  real<lower=0>neg_inf[NumPeople,NumPeople];
}

model {
  for (Person in 1:NumPeople) {
      alpha_person[Person] ~ exponential(1);
      beta_person[Person] ~ exponential(1);

    }


  for (PersonA in 1:NumPeople) {
    for (PersonB in PersonA:NumPeople) {
      pos_inf[PersonA,PersonB] ~ exponential(1);
      neg_inf[PersonA,PersonB] ~ exponential(1);
      pos_inf[PersonB,PersonA] ~ exponential(1);
      neg_inf[PersonB,PersonA] ~ exponential(1);

      thetaA[PersonA,PersonB] ~ beta(alpha_pop + alpha_person[PersonB] +
                                     pos_inf[PersonA,PersonB],
                                     beta_pop + beta_person[PersonB] + 
                                     neg_inf[PersonA,PersonB]);
      thetaNotA[PersonA,PersonB] ~ beta(alpha_pop + alpha_person[PersonB],
                                               beta_pop + beta_person[PersonB]);

      CountsAB[PersonA,PersonB] ~ binomial(NumUtterancesAB[PersonA,PersonB],
                                           thetaA[PersonA,PersonB]);

      CountsNotAB[PersonA,PersonB] ~ binomial(NumUtterancesNotAB[PersonA,PersonB],
                                              thetaNotA[PersonA,PersonB]);

      #B -> A
      thetaA[PersonB,PersonA] ~ beta(alpha_pop + alpha_person[PersonA] +
                                     pos_inf[PersonB,PersonA],
                                     beta_pop + beta_person[PersonA] + 
                                     neg_inf[PersonB,PersonA]);
      thetaNotA[PersonB,PersonA] ~ beta(alpha_pop + alpha_person[PersonA],
                                        beta_pop + beta_person[PersonA]);
      
      CountsAB[PersonB,PersonA] ~ binomial(NumUtterancesAB[PersonB,PersonA],
                                           thetaA[PersonB,PersonA,Marker]);
      
      CountsNotAB[PersonB,PersonA] ~ binomial(NumUtterancesNotAB[PersonB,PersonA],
                                              thetaNotA[PersonB,PersonA]);
    }
  }
}
    

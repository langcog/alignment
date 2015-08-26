data {
  int<lower=0> NumPeople;
  int<lower=0> NumMarkers;
  int<lower=0> NumUtterancesAB[NumPeople,NumPeople];
  int<lower=0> NumUtterancesNotAB[NumPeople,NumPeople];
  int<lower=0> CountsAB[NumPeople,NumPeople,NumMarkers];
  int<lower=0> CountsNotAB[NumPeople,NumPeople,NumMarkers];
}

parameters {
  real<lower=0> alpha_pop[NumMarkers];
  real<lower=0> beta_pop[NumMarkers];
  real<lower=0> alpha_person[NumPeople,NumMarkers];
  real<lower=0> beta_person[NumPeople,NumMarkers];
  real<lower=0,upper=1> thetaA[NumPeople,NumPeople,NumMarkers];
  real<lower=0,upper=1> thetaNotA[NumPeople,NumPeople,NumMarkers];
  real<lower=0>pos_inf[NumPeople,NumPeople];
  real<lower=0>neg_inf[NumPeople,NumPeople];
}

model {
  for (Marker in 1:NumMarkers) {
        alpha_pop[Marker] ~ exponential(1);
        beta_pop[Marker] ~ exponential(1);

    for (Person in 1:NumPeople) {
      alpha_person[Person,Marker] ~ exponential(1);
      beta_person[Person,Marker] ~ exponential(1);

    }

  }

  for (PersonA in 1:NumPeople) {
    for (PersonB in PersonA:NumPeople) {
      pos_inf[PersonA,PersonB] ~ exponential(1);
      neg_inf[PersonA,PersonB] ~ exponential(1);
      pos_inf[PersonB,PersonA] ~ exponential(1);
      neg_inf[PersonB,PersonA] ~ exponential(1);

      for (Marker in 1:NumMarkers) {


        #A -> B
        thetaA[PersonA,PersonB,Marker] ~ beta(alpha_pop[Marker]+ alpha_person[PersonB,Marker] +
                                                                 pos_inf[PersonA,PersonB],
                                         beta_pop[Marker] + beta_person[PersonB,Marker] + 
                                                                 neg_inf[PersonA,PersonB]);
        thetaNotA[PersonA,PersonB,Marker] ~ beta(alpha_pop[Marker]+ alpha_person[PersonB,Marker],
                                                 beta_pop[Marker] + beta_person[PersonB,Marker]);

        CountsAB[PersonA,PersonB,Marker] ~ binomial(NumUtterancesAB[PersonA,PersonB],
                                                    thetaA[PersonA,PersonB,Marker]);

        CountsNotAB[PersonA,PersonB,Marker] ~ binomial(NumUtterancesNotAB[PersonA,PersonB],
                                                    thetaNotA[PersonA,PersonB,Marker]);

        #B -> A
        thetaA[PersonB,PersonA,Marker] ~ beta(alpha_pop[Marker]+ alpha_person[PersonA,Marker] +
                                                                 pos_inf[PersonB,PersonA],
                                         beta_pop[Marker] + beta_person[PersonA,Marker] + 
                                                                 neg_inf[PersonB,PersonA]);
        thetaNotA[PersonB,PersonA,Marker] ~ beta(alpha_pop[Marker]+ alpha_person[PersonA,Marker],
                                                 beta_pop[Marker] + beta_person[PersonA,Marker]);

        CountsAB[PersonB,PersonA,Marker] ~ binomial(NumUtterancesAB[PersonB,PersonA],
                                                    thetaA[PersonB,PersonA,Marker]);

        CountsNotAB[PersonB,PersonA,Marker] ~ binomial(NumUtterancesNotAB[PersonB,PersonA],
                                                    thetaNotA[PersonB,PersonA,Marker]);
      }
    }
  }
}
    

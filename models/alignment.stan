data {
  int<lower=0> NumPeople;
  int<lower=0> NumMarkers;
  int<lower=0> NumUtterances[NumPeople];
  int<lower=0> Counts[NumPeople,NumMarkers];
}

parameters {
  real<lower=0> alpha_pop[NumMarkers];
  real<lower=0> beta_pop[NumMarkers];
  real<lower=0,upper=1> theta[NumPeople,NumMarkers];
}

model {
  for (Marker in 1:NumMarkers) {
      alpha_pop[Marker] ~ exponential(1);
      beta_pop[Marker] ~ exponential(1);
    for (Person in 1:NumPeople) {
      theta[Person,Marker] ~ beta(alpha_pop[Marker],beta_pop[Marker]);
      Counts[Person,Marker] ~ binomial(NumUtterances[Person],theta[Person,Marker]);
    }
  }
}
    

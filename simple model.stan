data{
  int<lower = 0> N;
  real<lower = 0, upper = 1> regwins[N];
  real<lower = 0, upper = 1> playoffwins[N];
  real<lower = 0, upper = 1> latewins[N];
  int<lower = 0> seed[8];
}

parameters{
  real beta_reg[8];
  real beta_late[8];
  real sigma_reg;
  real sigma_late;
  real alpha_late[8];
  real alpha_reg[8];
  // real sigma;
}

model{
  // target += normal_lpdf(playoffwins | beta_reg*regwins[N], sigma_reg);
  target += normal_lpdf(playoffwins | alpha_late[seed[N]] + beta_late[seed[N]]*latewins[N], sigma_reg);
  target += normal_lpdf(playoffwins | alpha_reg[seed[N]] + beta_reg[seed[N]]*regwins[N], sigma_late);
  // beta_late ~ normal(0, sigma_late);
  // beta_reg ~ normal(0, sigma_reg);
}

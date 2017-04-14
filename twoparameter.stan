data{
  int<lower = 0> N;
  real<lower = 0, upper = 1> regwins[N];
  // real<lower = 0, upper = 1> playoffwins[N];
  real playoffwins[N];
  real<lower = 0, upper = 1> latewins[N];
  int<lower = 0> seed1[N];
}

parameters{
  real beta_reg[8];
  real beta_late[8];
  real <lower = 0> sigma;
  real alpha[8];
  real mu_late;
  real mu_reg;
  // real sigma;
}

model{
  // target += normal_lpdf(playoffwins | beta_reg*regwins[N], sigma_reg);
  // target += normal_lpdf(playoffwins | alpha_late[seed[N]] + beta_late[seed[N]]*latewins[N], sigma_late);
  // target += normal_lpdf(playoffwins | alpha_reg[seed[N]] + beta_reg[seed[N]]*regwins[N], sigma_reg);
  
  for(i in 1:N){
    playoffwins[i] ~ normal(alpha[seed1[i]] + beta_late[seed1[i]]*latewins[i] + beta_reg[seed1[i]]*regwins[i], sigma);
  }
  
  beta_late ~ normal(mu_late, 8);
  beta_reg ~ normal(mu_reg, 8);
  alpha ~ normal(0, 3);
  mu_late ~ normal(0, 10);
  mu_reg ~ normal(0, 10);  
}

generated quantities{
  real y_pred[N];
  for(i in 1:N){
    y_pred[i] = normal_rng(alpha[seed1[i]] + beta_late[seed1[i]]*latewins[i] + beta_reg[seed1[i]]*regwins[i], sigma);
    }
}

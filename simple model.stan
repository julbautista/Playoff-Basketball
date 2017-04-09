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
  real <lower = 0> sigma_reg;
  real <lower = 0> sigma_late;
  real alpha_late[8];
  real alpha_reg[8];
  real mu_late;
  real mu_reg;
  // real sigma;
}

model{
  // target += normal_lpdf(playoffwins | beta_reg*regwins[N], sigma_reg);
  // target += normal_lpdf(playoffwins | alpha_late[seed[N]] + beta_late[seed[N]]*latewins[N], sigma_late);
  // target += normal_lpdf(playoffwins | alpha_reg[seed[N]] + beta_reg[seed[N]]*regwins[N], sigma_reg);
  
  for(i in 1:N){
    playoffwins[i] ~ normal(alpha_late[seed1[i]] + beta_late[seed1[i]]*latewins[i], sigma_late);
    playoffwins[i] ~ normal(alpha_reg[seed1[i]] + beta_reg[seed1[i]]*regwins[N], sigma_reg);
  }
  
  beta_late ~ normal(mu_late, sigma_late);
  beta_reg ~ normal(mu_reg, sigma_reg);
  alpha_late ~ normal(0, 3);
  alpha_reg ~ normal(0, 3);
  mu_late ~ normal(0, 10);
  mu_reg ~ normal(0, 10);  
}

generated quantities{
  real y_pred_reg[N];
  real y_pred_late[N];
  for(i in 1:N){
    y_pred_reg[i] = normal_rng(alpha_late[seed1[i]] + beta_late[seed1[i]]*latewins[i], sigma_late);
    y_pred_late[i] = normal_rng(alpha_reg[seed1[i]] + beta_reg[seed1[i]]*regwins[N], sigma_reg);
  }
}

data{
  int<lower = 0> N;
  real<lower = 0, upper = 1> regwins[N];
  // real<lower = 0, upper = 1> playoffwins[N];
  int playoffwins[N];
  real<lower = 0, upper = 1> latewins[N];
  int<lower = 0> seed1[N];
}

parameters {
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

transformed parameters {
  real A_reg[N];
  real A_late[N];
  real B_reg[N];
  real B_late[N];
  for(i in 1:N){
    A_late[i] = alpha_late[seed1[i]] + beta_late[seed1[i]]*latewins[i];
    A_reg[i] = alpha_reg[seed1[i]] + beta_reg[seed1[i]]*regwins[i];
    B_late[i] = alpha_late[seed1[i]] + beta_late[seed1[i]]*latewins[i];
    B_reg[i] = alpha_reg[seed1[i]] + beta_reg[seed1[i]]*regwins[i];
  }  
}


model {
  for(i in 1:N){
    playoffwins[i] ~ beta_binomial(16, A_reg[i], B_reg[i]);
    playoffwins[i] ~ beta_binomial(16, A_late[i], B_late[i]);      
  }
  beta_late ~ normal(mu_late, sigma_late);
  beta_reg ~ normal(mu_reg, sigma_reg);
  alpha_late ~ normal(10, 1.5);
  alpha_reg ~ normal(10, 1.5);
  mu_late ~ normal(40, 10);
  mu_reg ~ normal(40, 10);
}

generated quantities{
  real y_pred_reg[N];
  real y_pred_late[N];
  for(i in 1:N){
    y_pred_reg[i] = beta_binomial_rng(16, A_reg[i], B_reg[i]);
    y_pred_late[i] = beta_binomial_rng(16, A_late[i], B_late[i]);
  }
}

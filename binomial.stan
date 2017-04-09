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
  real theta_reg[N];
  real theta_late[N];
  for(i in 1:N){
    theta_late[i] = inv_logit(alpha_late[seed1[i]] + beta_late[seed1[i]]*latewins[i]);
    theta_reg[i] = inv_logit(alpha_reg[seed1[i]] + beta_reg[seed1[i]]*regwins[N]);
  }  
}


model {
  for(i in 1:N){
    playoffwins[i] ~ binomial(16, theta_reg[i]);
    playoffwins[i] ~ binomial(16, theta_late[i]);      
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
    y_pred_reg[i] = binomial_rng(16, theta_reg[i]);
    y_pred_late[i] = binomial_rng(16, theta_late[i]);
  }
}

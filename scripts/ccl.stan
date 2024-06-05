data {
  int<lower=1> len_data;
  int<lower=1> len_pred;

  array[len_data + len_pred] int<lower=0, upper=1> origin1id;
  array[len_data + len_pred] real logprem;
  array[len_data] real logloss;
  array[len_data + len_pred] int<lower=1> origin;
  array[len_data + len_pred] int<lower=1> dev;
}
transformed data {
  int n_origin = max(origin);
  int n_dev = max(dev);
  int len_total = len_data + len_pred;
}
parameters {
  array[n_origin - 1] real r_alpha;
  array[n_dev - 1] real r_beta;
  real log_elr;
  array[n_dev] real<lower=0, upper=100000> a_ig;
  real<lower=0, upper=1> r_rho;
  array[len_pred] real logloss_pred;
}
transformed parameters {
  array[n_origin] real alpha;
  array[n_dev] real beta;
  array[n_dev] real sig2;
  array[n_dev] real sig;
  array[len_data] real mu;
  array[len_pred] real mu_pred;
  real<lower=-1, upper=1> rho;

  rho = -2*r_rho + 1;

  alpha[1] = 0;
  for (i in 2:n_origin) {
   alpha[i] = r_alpha[i-1];
  }

  for (i in 1:(n_dev - 1)) {
   beta[i] = r_beta[i];
  }

  beta[n_dev] = 0;

  sig2[n_dev] = gamma_cdf(1/a_ig[n_dev] | 1, 1);
  for (i in 1:(n_dev - 1)) {
   sig2[n_dev - i] = sig2[n_dev + 1 - i] + gamma_cdf(1/a_ig[i] | 1, 1);
  }

  for (i in 1:n_dev) {
   sig[i] = sqrt(sig2[i]);
  }

  mu[1] = logprem[1] + log_elr + beta[dev[1]];

  for (i in 2:len_data) {
   mu[i] = logprem[i] + log_elr + alpha[origin[i]] + beta[dev[i]] + 
    rho*(logloss[i-1] - mu[i-1]) * origin1id[i];
  }

  mu_pred[1] = logprem[(len_data) + 1] + alpha[origin[len_data + 1]] + 
   log_elr + beta[dev[len_data + 1]] + 
   rho*(logloss[len_data] - mu[len_data]) * origin1id[len_data + 1];

  for (i in 2:len_pred) {
    mu_pred[i] = logprem[len_data + i] + alpha[origin[len_data + i]] + 
     log_elr + beta[dev[len_data + i]] +
     rho*(logloss_pred[i-1] - mu_pred[i-1]) * origin1id[len_data + i];
  }
}
model {
  log_elr ~ normal(0, 1);
  r_alpha ~ normal(0, sqrt(10/1.0));
  r_beta ~ normal(0, sqrt(10/1.0));
  a_ig ~ inv_gamma(1, 1);
  r_rho ~ beta(2, 2);

  for (i in  1:(len_data)) {
    logloss[i] ~ normal(mu[i], sig[dev[i]]);
  }

  for (i in 1:(len_pred)) {
    logloss_pred[i] ~ normal(mu_pred[i], sig[dev[len_data + i]]);
  }
}
generated quantities {
  vector[len_data] log_lik;
  vector[len_total] ppc_loss;

  for (i in 1:len_data) {
   log_lik[i] = normal_lpdf(logloss[i] | mu[i], sig[dev[i]]);
  }

  for (i in 1:len_data) {
   ppc_loss[i] = exp(normal_rng(mu[i], sig[dev[i]]));
  }

  for (i in 1:len_pred) {
   ppc_loss[len_data + i] = exp(normal_rng(mu_pred[i], sig[dev[len_data + i]]));
  }
}

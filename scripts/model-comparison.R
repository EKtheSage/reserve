if (!exists('create_loss_data', mode = 'function')) source('./scripts/00-init.R')

cas_data <- fread('https://www.casact.org/sites/default/files/2021-04/comauto_pos.csv') # nolint: quotes_linter.

setnames(
  cas_data,
  c('EarnedPremNet_C', 'AccidentYear', 'DevelopmentLag', 
    'IncurLoss_C', 'CumPaidLoss_C', 'BulkLoss_C'),
  c('premium', 'accident_year', 'dev',
    'incurred_loss', 'paid_loss', 'bulk_loss')
)


# ---- CCL ----
loss_data <- create_loss_data(cas_data, company_code = 353, loss_type = 'incurred')

loss_data[, 'split' := ifelse(is.na(loss_train), 'test', 'train')]

mod_ccl <- cmdstan_model('./scripts/ccl.stan')

stan_data <- create_stan_data(loss_data)

fit_ccl <- mod_ccl$sample(
  data = stan_data,
  seed = 1234,
  iter_sampling = 4000,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  adapt_delta = 0.99,
  max_treedepth = 10
)

# ---- CSR ----
loss_data <- create_loss_data(cas_data, company_code = 353, loss_type = 'paid')

loss_data[, 'split' := ifelse(is.na(loss_train), 'test', 'train')]

mod_csr <- cmdstan_model('./scripts/csr.stan')

stan_data <- create_stan_data(loss_data)

fit_csr <- mod_csr$sample(
  data = stan_data,
  seed = 1234,
  iter_sampling = 4000,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  adapt_delta = 0.99,
  max_treedepth = 10
)

# ---- Comparison ----
waic(fit_ccl$draws('log_lik'))
waic(fit_csr$draws('log_lik'))

r_eff_ccl <- relative_eff(exp(fit_ccl$draws('log_lik')))
r_eff_csr <- relative_eff(exp(fit_csr$draws('log_lik')))

loo_ccl <- loo(fit_ccl$draws('log_lik'), r_eff = r_eff_ccl)
loo_csr <- loo(fit_csr$draws('log_lik'), r_eff = r_eff_csr)

waics <- c(
  waic(fit_ccl$draws('log_lik'))$estimates['elpd_waic', 1],
  waic(fit_csr$draws('log_lik'))$estimates['elpd_waic', 1]
)

## prefer Pseudo BMA+ with BB or Stacking
lpd_point <- cbind(
  loo_ccl$pointwise[, 'elpd_loo'],
  loo_csr$pointwise[, 'elpd_loo']
)

waic_wts <- exp(waics) / sum(exp(waics))
pbma_wts <- pseudobma_weights(lpd_point, BB = FALSE) # not using bayesian bootstrap
pbma_bb_wts <- pseudobma_weights(lpd_point) # this is more conservative
stacking_wts <- stacking_weights(lpd_point)

round(cbind(waic_wts, pbma_wts, pbma_bb_wts, stacking_wts), 2)

## wrapper model weights
loo_model_weights(list(CCL = loo_ccl, CSR = loo_csr), method = 'stacking')
loo_model_weights(list(CCL = loo_ccl, CSR = loo_csr), method = 'pseudobma')
loo_model_weights(list(CCL = loo_ccl, CSR = loo_csr), method = 'pseudobma', BB = FALSE)

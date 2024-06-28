if (!exists('create_loss_data', mode = 'function')) source('./scripts/00-init.R')

cas_data <- fread('https://www.casact.org/sites/default/files/2021-04/comauto_pos.csv') # nolint: quotes_linter.

loss_data <- create_loss_data(cas_data, company_code = 353, loss_type = 'incurred')

mod <- cmdstan_model('./scripts/ccl.stan')

stan_data <- create_stan_data(loss_data)

fit <- mod$sample(
  data = stan_data,
  seed = 1234,
  iter_sampling = 4000,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  adapt_delta = 0.99,
  max_treedepth = 10
)

fit$summary(
  variables = c('alpha', 'beta', 'rho', 'log_elr', 'sig'),
  c('mean', 'median', 'rhat', 'ess_bulk'),
  extra_quantiles = ~quantile2(., probs = c(0.025, 0.975))
)

ppc_loss <- fit$summary(
  variables = c('ppc_loss'),
  c('mean', 'median', 'rhat', 'ess_bulk'),
  extra_quantiles = ~quantile2(., probs = c(0.025, 0.975))
)

loss_data <- cbind(
  loss_data,
  pred_mean = ppc_loss$mean,
  pred_median = ppc_loss$median,
  pred_q2.5 = ppc_loss$q2.5,
  pred_q97.5 = ppc_loss$q97.5
)

ggplot(loss_data, aes(dev, loss)) +
  geom_point(aes(color = split)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) + 
  facet_wrap(~ accident_year)

ggplot(loss_data, aes(dev, loss)) +
  geom_point(aes(color = split)) +
  geom_line(aes(dev, pred_mean, color = 'Prediction Mean')) +
  geom_ribbon(
    aes(ymin = pred_q2.5, ymax = pred_q97.5, fill = '95% Credible Interval'),
    alpha = 0.25) +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  scale_color_manual(
    values = c(
      'train' = '#317CEF',
      'test' = '#FBCC6B',
      'Prediction Mean' = '#1A0B60'),
    labels = c(
      'train' = 'Train',
      'test' = 'Test',
      'Prediction Mean' = 'Prediction Mean'
    )) +
  scale_fill_manual(values = c('95% Credible Interval' = '#5ACAFA')) +
  labs(
    'title' = 'AY Predicted Loss and 95% Credible Interval',
    x = 'Development Year', y = NULL,
    color = NULL, fill = NULL) +
  facet_wrap(~ accident_year, ncol = 5)

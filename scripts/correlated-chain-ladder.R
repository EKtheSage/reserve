library(data.table)
library(ggplot2)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(showtext)
set_cmdstan_path('/opt/conda/envs/reserve/bin/cmdstan/')

font_add_google('Nunito', 'nunito')
font_add_google('Inter', 'inter')
showtext_auto()

theme_set(
  theme_minimal() +
    theme(text = element_text(family = 'nunito'))
)

cas_data <- fread('https://www.casact.org/sites/default/files/2021-04/comauto_pos.csv') # nolint: quotes_linter.

setnames(
  cas_data,
  c('EarnedPremNet_C', 'AccidentYear', 'DevelopmentLag', 
    'IncurLoss_C', 'CumPaidLoss_C', 'BulkLoss_C'),
  c('premium', 'accident_year', 'dev',
    'incurred_loss', 'paid_loss', 'bulk_loss')
)

create_loss_data <- function(cas_data, company_code, loss_type= 'incurred'){

  comp_data <- cas_data[
    GRCODE==company_code,  # nolint: object_usage_linter.
    c('premium', 'accident_year', 'dev', 'incurred_loss', 'paid_loss', 'bulk_loss')
  ]

  comp_data <- comp_data[, `:=`(
    origin = accident_year - min(accident_year) + 1,
    # origin period starting from 1, id to identify first origin period
    origin1id = ifelse(accident_year == min(accident_year), 0,  1))]

  # set losses < 0 to 1, as we will take the log later
  if (loss_type %in% "incurred"){
    comp_data[, loss := pmax(incurred_loss - bulk_loss, 1)]
  } else {
    comp_data[, loss := pmax(paid_loss, 1)]
  }
  # add calendar period and sort data by dev and then origin
  comp_data[, cal := origin + dev - 1]
  setorder(comp_data, dev, origin)

  comp_data[, `:=`(
    loss_train = ifelse(cal <= max(origin), loss, NA),
    loss_test = ifelse(cal > max(origin), loss, NA))]

  train_test <- rbindlist(
    list(
      comp_data[cal <= max(origin)],
      comp_data[cal > max(origin)]
    )
  )
  return(train_test)
}

loss_data <- create_loss_data(cas_data, company_code = 353, loss_type = 'incurred')

loss_data[, 'split' := ifelse(is.na(loss_train), 'test', 'train')]

ggplot(loss_data, aes(dev, loss)) +
  geom_point(aes(color = split)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) + 
  facet_wrap(~ accident_year)

mod <- cmdstan_model('./scripts/ccl.stan')

create_stan_data <- function(loss_data) {
  list(
    len_data = loss_data[!is.na(loss_train), .N],
    len_pred = loss_data[is.na(loss_train), .N],
    logprem = log(loss_data[, premium]),
    logloss = log(loss_data[!is.na(loss_train), loss_train]),
    origin = loss_data[, origin],
    dev = loss_data[, dev],
    origin1id = loss_data[, origin1id]

  )
}


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
  facet_wrap(~ accident_year)

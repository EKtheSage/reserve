library(here)
library(data.table)
library(brms)
library(posterior)
library(ggplot2)
library(showtext)
options(mc.cores = parallel::detectCores())

font_add_google('Nunito', 'nunito')
font_add_google('Inter', 'inter')
showtext_auto()

theme_set(
  theme_minimal() +
    theme(text = element_text(family = 'nunito'))
)

dat <- fread('./data/us-eu-prop-liab-motor.csv')

dat <- dat[!is.na(cum_paid_loss_ratio)]

dat <- melt(
  dat,
  measure.vars = list(
    incr_amount = c('cum_amount_os', 'amount_paid'),
    cum_amount = c('cum_amount_os', 'cum_amount_paid'),
    incr_loss_ratio = c('os_loss_ratio', 'incr_paid_loss_ratio'),
    cum_loss_ratio = c('os_loss_ratio', 'cum_paid_loss_ratio')
  ),
  variable.name = 'delta',
  variable.factor = FALSE)

dat[, deltaf := ifelse(delta == 1, 'os', 'paid')]
dat[, delta := ifelse(delta == 1, 0, 1)]
dat[, incr_loss_ratio_adj := ifelse(
  incr_loss_ratio <= 0, 1e-7, incr_loss_ratio)]
dat[, cum_loss_ratio_adj := ifelse(
  cum_loss_ratio <= 0, 1e-7, cum_loss_ratio
)]
dat[, dev := dev_q / 12]
# ---- Correlated Chain Ladder ----
# ---- Changing Settlement Rate ----
# ---- Hierarchical Growth Curve ----

options(cmdstanr_write_stan_file_dir = here('models/hierarchical-growth-curve'))
options(rcpp.cache.dir = here('rcpp-cache/hierarchical-growth-curve'))
fit_growth_curve <- brm(
  bf(
    cum_loss_ratio_adj ~ log(ult * (1 - exp(-(dev * phi)^omega))),
    ult ~ 1 + (1 | uwq) + (1 | res_q_level_1) + (1 | res_q_level_4),
    omega ~ 1 + (1 | uwq) + (1 | res_q_level_1) + (1 | res_q_level_4),
    phi ~ 1 + (1 | uwq) + (1 | res_q_level_1) + (1 | res_q_level_4),
    nl = TRUE
  ),
  data = dat[res_q_level_4 != 'Motor' & deltaf == 'paid'],
  family = lognormal(link = 'identity', link_sigma = 'log'),
  prior = c(
    prior(lognormal(log(0.5), log(1.2)), nlpar = 'ult', lb = 0),
    prior(normal(1.25, 0.25), nlpar = 'omega', lb = 0),
    prior(normal(0.25, 0.25), nlpar = 'phi', lb = 0),
    prior(student_t(5, 0, 0.25), class = 'sigma'),
    prior(student_t(5, 0, 0.25), class = 'sd', nlpar = 'ult')
  ),
  backend = 'cmdstan',
  file = 'hierarchical-growth-curve-prod',
  file_refit = 'on_change',
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)


pred_cum_paid_loss_ratio <- fitted(fit_growth_curve)

result <- dat[res_q_level_4 != 'Motor' & deltaf == 'paid']
result[, estimate := pred_cum_paid_loss_ratio[, 'Estimate']]
result[, q2.5 := pred_cum_paid_loss_ratio[, 'Q2.5']]
result[, q97.5 := pred_cum_paid_loss_ratio[, 'Q97.5']]

cols <- c('estimate', 'q2.5', 'q97.5')
setnames(result, cols, paste0('cum_', cols))

df_plot <- dcast(
  result, uwq + uwq2 + res_q_level_1 + res_q_level_4 + reporting_month +
  dev_q + cum_incurred_loss_ratio ~
  deltaf,
  value.var = c('cum_estimate', 'cum_q2.5', 'cum_q97.5')
)

setnames(
  df_plot
  , c('cum_estimate_paid', 'cum_q2.5_paid', 'cum_q97.5_paid')
  , c('cum_estimate_loss_ratio', 'cum_q2.5_loss_ratio', 'cum_q97.5_loss_ratio')
)

# ---- Compartmental Cumulative Paid Gaussian ----
# skipping motor for now

options(cmdstanr_write_stan_file_dir = here('models/compartmental-cumpaid-os-gaussian-prod'))
options(rcpp.cache.dir = here('rcpp-cache/compartmental-cumpaid-os-gaussian-prod'))

function_cum_paid <- '
real paid(real t, real ker, real kp, real RLR, real RRF){
 return(
  RLR*RRF/(ker - kp) * (ker *(1 - exp(-kp*t)) - 
  kp*(1 - exp(-ker*t)))
 );
}
real outstanding(real t, real ker, real kp, real RLR){
 return(
  (RLR*ker/(ker - kp) * (exp(-kp*t) - exp(-ker*t)))
 );
}
real claimsprocess(real t, real ker, real kp, 
                   real RLR, real RRF, real delta){
    real out; 
    out = outstanding(t, ker, kp, RLR) * (1 - delta) + 
          paid(t, ker, kp, RLR, RRF) * delta;
    
    return(out);
}
'

formula_cum_paid <- bf(
  cum_amount ~ premium * claimsprocess(dev_q, ker, kp, RLR, RRF, delta),
  nlf(ker ~ 3 * exp(oker * 0.1)),
  nlf(kp ~ 1 * exp(okp * 0.1)),
  nlf(RLR ~ 0.5 * exp(oRLR * 0.2)),
  nlf(RRF ~ 0.8 * exp(oRRF * 0.1)),
  oRLR ~ 1 + (1 | ID | uwq) + (1 | res_q_level_1) + (1 | res_q_level_4),
  oRRF ~ 1 + (1 | ID | uwq) + (1 | res_q_level_1) + (1 | res_q_level_4),
  oker ~ 1, okp ~ 1, sigma ~ 0 + deltaf,
  nl = TRUE
)

priors_cum_paid <- c(
  prior(normal(0, 1), nlpar = 'oRLR'),
  prior(normal(0, 1), nlpar = 'oRRF'),
  prior(normal(0, 1), nlpar = 'oker'),
  prior(normal(0, 1), nlpar = 'okp'),
  prior(student_t(1, 0, 1000), class = 'b', coef = 'deltafpaid', dpar = 'sigma'),
  prior(student_t(1, 0, 1000), class = 'b', coef = 'deltafos', dpar = 'sigma'),
  prior(student_t(10, 0, 0.2), class = 'sd', nlpar = 'oRLR'),
  prior(student_t(10, 0, 0.1), class = 'sd', nlpar = 'oRRF'),
  prior(lkj(1), class = 'cor'))

fit_cum_paid <- brm(
  formula_cum_paid,
  data = dat[res_q_level_4 != 'Motor'], family = gaussian(),
  prior = priors_cum_paid,
  stanvars = stanvar(scode = function_cum_paid, block = 'functions'),
  backend = 'cmdstan',
  file = 'hierarchical-compartmental-cumpaid-os-gaussian-prod',
  file_refit = 'on_change',
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

expose_functions(fit_cum_paid, vectorize = TRUE)

pred_cum_paid <- fitted(fit_cum_paid)

## ---- Post Predict Data Processing ----
result <- dat[res_q_level_4 != 'Motor']
result[, estimate := pred_cum_paid[, 'Estimate']]
result[, q2.5 := pred_cum_paid[, 'Q2.5']]
result[, q97.5 := pred_cum_paid[, 'Q97.5']]

cols <- c('estimate', 'q2.5', 'q97.5')

result[, (cols) := lapply(.SD, \(x) (x / premium)), .SDcols = cols]

setnames(result, cols, paste0('cum_', cols))

df_plot <- dcast(
  result, uwq + uwq2 + res_q_level_1 + res_q_level_4 + reporting_month + 
  dev_q + cum_incurred_loss_ratio~
  deltaf,
  value.var = c('cum_estimate', 'cum_q2.5', 'cum_q97.5')
)

df_plot[, cum_estimate_loss_ratio := cum_estimate_os + cum_estimate_paid]
df_plot[, cum_q2.5_loss_ratio := cum_q2.5_os + cum_q2.5_paid]
df_plot[, cum_q97.5_loss_ratio := cum_q97.5_os + cum_q97.5_paid]

# ---- Compartmental Cumulative Paid Lognormal ----
# ---- Compartmental Incremental Paid Lognormal ----
options(cmdstanr_write_stan_file_dir = here('models/compartmental-incrpaid-os-lognorm-prod'))
options(rcpp.cache.dir = here('rcpp-cache/compartmental-incrpaid-os-lognorm-prod'))

function_incr_paid <- '
real paid(real t, real ker, real kp, real RLR, real RRF){
 return(
  RLR*RRF/(ker - kp) * (ker *(1 - exp(-kp*t)) - 
  kp*(1 - exp(-ker*t)))
 );
}
real outstanding(real t, real ker, real kp, real RLR){
 return(
  (RLR*ker/(ker - kp) * (exp(-kp*t) - exp(-ker*t)))
 );
}
real incrclaimsprocess(real t, real devfreq, real ker, real kp, 
                   real RLR, real RRF, real delta){
    real out; 
    out = outstanding(t, ker, kp, RLR) * (1 - delta) + 
          paid(t, ker, kp, RLR, RRF) * delta;
    
    if( (delta > 0) && (t > devfreq) ){ // paid greater dev period 1
    // incremental paid
     out = out - paid(t - devfreq, ker, kp, RLR, RRF)*delta;
    }
    return(out);
}
'

formula_incr_paid <- bf(
  incr_loss_ratio_adj ~ eta,
  nlf(eta ~ log(incrclaimsprocess(dev_q, 1.0, ker, kp, RLR, RRF, delta))),
  nlf(ker ~ 1 * exp(oker * 0.1)), # modify for monthly - uwq at month 3 ~ 95% reported
  nlf(kp ~ 1 * exp(okp * 0.1)),
  nlf(RLR ~ 0.5 * exp(oRLR * 0.2)), # median reported LR at 50%
  nlf(RRF ~ 0.8 * exp(oRRF * 0.1)),
  oRLR ~ 1 + (1 | ID | uwq) + (1 | dev_q),
  oRRF ~ 1 + (1 | ID | uwq) + (1 | dev_q),
  oker ~ 1 + (1 | uwq) + (1 | dev_q),
  okp ~ 1 + (1 | uwq) + (1 | dev_q),
  sigma ~ 0 + deltaf,
  nl = TRUE
)

priors_incr_paid <- c(
  prior(normal(0, 1), nlpar = 'oRLR'),
  prior(normal(0, 1), nlpar = 'oRRF'),
  prior(normal(0, 1), nlpar = 'oker'),
  prior(normal(0, 1), nlpar = 'okp'),
  prior(normal(log(0.2), 0.2), class = 'b', coef = 'deltafpaid', dpar = 'sigma'),
  prior(normal(log(0.2), 0.2), class = 'b', coef = 'deltafos', dpar = 'sigma'),
  prior(student_t(10, 0, 0.3), class = 'sd', nlpar = 'oker'), # reduce spread
  prior(student_t(10, 0, 0.3), class = 'sd', nlpar = 'okp'), # reduce spread
  prior(student_t(10, 0, 0.7), class = 'sd', nlpar = 'oRLR'),
  prior(student_t(10, 0, 0.5), class = 'sd', nlpar = 'oRRF'),
  prior(lkj(1), class = 'cor')
)

fit_incr_paid <- brm(
  formula_incr_paid,
  data = dat[res_q_level_4 == 'Property' & res_q_level_1 == 'Europe'],
  family = brmsfamily('lognormal', link_sigma = 'log'),
  prior = priors_incr_paid,
  stanvars = stanvar(scode = function_incr_paid, block = 'functions'),
  backend = 'cmdstan',
  file = 'hierarchical-compartmental-incrpaid-os-lognorm-prod',
  file_refit = 'on_change',
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  iter = 2000, chains = 4
)

expose_functions(fit_incr_paid, vectorize = TRUE)
# rfunc_incr_paid <- cmdstan_expose_functions(function_incr_paid)

# expected value of ppd - residual error is ignored
pred_incr_paid <- fitted(fit_incr_paid)
# posterior_epred()

# posterior draws of the ppd, residual error is incorporated
# pred_incr_paid <- predict(fit_incr_paid)

## ---- Post Predict Data Processing ----
result <- dat[res_q_level_4 == 'Property' & res_q_level_1 == 'Europe']
result[, estimate := pred_incr_paid[, 'Estimate']]
result[, q2.5 := pred_incr_paid[, 'Q2.5']]
result[, q97.5 := pred_incr_paid[, 'Q97.5']]

result_pred <- result[
  deltaf == 'paid'
  , .(uwq, uwq2, res_q_level_1, res_q_level_4, reporting_month, premium,
      estimate, q2.5, q97.5)
]

cols <- c('estimate', 'q2.5', 'q97.5')
result_pred[
  , (cols) := lapply(.SD, \(x) (x * premium))
  , .SDcols = c('estimate', 'q2.5', 'q97.5')
]

setorder(result_pred, uwq, uwq2, res_q_level_1, res_q_level_4, reporting_month)

result_pred[
  , (cols) := lapply(.SD, \(x) (cumsum(x)))
  , by = .(uwq, uwq2, res_q_level_1, res_q_level_4)
  , .SDcols = cols
]

result_pred[, (cols) := lapply(.SD, \(x) (x / premium)), .SDcols = cols]


result <- result_pred[
  result[, -'premium']
  , on = c('uwq', 'uwq2', 'res_q_level_1', 'res_q_level_4', 'reporting_month')]

setnames(result, paste0('i.', cols), paste0('incr_', cols))
result[deltaf == 'os', (cols) := .(incr_estimate, incr_q2.5, incr_q97.5)]
setnames(result, cols, paste0('cum_', cols))

df_plot <- dcast(
  result, uwq + uwq2 + res_q_level_1 + res_q_level_4 + reporting_month + 
  dev_q + cum_incurred_loss_ratio~
  deltaf,
  value.var = c('cum_estimate', 'cum_q2.5', 'cum_q97.5')
)

df_plot[, cum_estimate_loss_ratio := cum_estimate_os + cum_estimate_paid]
df_plot[, cum_q2.5_loss_ratio := cum_q2.5_os + cum_q2.5_paid]
df_plot[, cum_q97.5_loss_ratio := cum_q97.5_os + cum_q97.5_paid]
# pred_draws_incr_paid <- as_draws_array(pred_incr_paid)
# summarise_draws(
#   pred_draws_incr_paid,
#   mean, median, sd,
#   ~quantile2(.x, probs = c(0.025, 0.975)))

## ---- Result ----

### ---- US Property ----
ggplot(df_plot[res_q_level_1 == 'US' & res_q_level_4 == 'Property'], aes(dev_q)) +
  geom_ribbon(
    aes(
      ymin = cum_q2.5_loss_ratio, ymax = cum_q97.5_loss_ratio,
      fill = '95% CI'), alpha = 0.4) +
  geom_point(aes(y = cum_incurred_loss_ratio, color = 'Actual')) +
  geom_line(aes(y = cum_estimate_loss_ratio, color = 'Predicted')) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    values = c('Predicted' = '#317CEF', 'Actual' = '#E1337C'),
    name = 'Incurred Loss Ratio') +
  scale_fill_manual(values = c('95% CI' = '#F8CC6B'), name = NULL) +
  guides(color = guide_legend(order = 1)) +
  labs('Estimated Ultimate LR and 95% CI', y = 'Loss Ratio', x = 'Development Month') +
  facet_wrap(~ uwq)

### ---- US Liability ----
ggplot(df_plot[res_q_level_1 == 'US' & res_q_level_4 == 'Liability'], aes(dev_q)) +
  geom_ribbon(
    aes(
      ymin = cum_q2.5_loss_ratio, ymax = cum_q97.5_loss_ratio,
      fill = '95% CI'), alpha = 0.4) +
  geom_point(aes(y = cum_incurred_loss_ratio, color = 'Actual')) +
  geom_line(aes(y = cum_estimate_loss_ratio, color = 'Predicted')) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    values = c('Predicted' = '#317CEF', 'Actual' = '#E1337C'),
    name = 'Incurred Loss Ratio') +
  scale_fill_manual(values = c('95% CI' = '#F8CC6B'), name = NULL) +
  guides(color = guide_legend(order = 1)) +
  labs('Estimated Ultimate LR and 95% CI', y = 'Loss Ratio', x = 'Development Month') +
  facet_wrap(~ uwq)

### ---- EU Property ----
ggplot(df_plot[res_q_level_1 == 'Europe' & res_q_level_4 == 'Property'], aes(dev_q)) +
  geom_ribbon(
    aes(
      ymin = cum_q2.5_loss_ratio, ymax = cum_q97.5_loss_ratio,
      fill = '95% CI'), alpha = 0.8) +
  geom_point(aes(y = cum_incurred_loss_ratio, color = 'Actual')) +
  geom_line(aes(y = cum_estimate_loss_ratio, color = 'Predicted')) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    values = c('Predicted' = '#317CEF', 'Actual' = '#E1337C'),
    name = 'Incurred Loss Ratio') +
  scale_fill_manual(values = c('95% CI' = '#F8CC6B'), name = NULL) +
  guides(color = guide_legend(order = 1)) +
  labs('Estimated Ultimate LR and 95% CI', y = 'Loss Ratio', x = 'Development Month') +
  facet_wrap(~ uwq)

### ---- EU Liability ----
ggplot(df_plot[res_q_level_1 == 'Europe' & res_q_level_4 == 'Liability'], aes(dev_q)) +
  geom_ribbon(
    aes(
      ymin = cum_q2.5_loss_ratio, ymax = cum_q97.5_loss_ratio,
      fill = '95% CI'), alpha = 0.4) +
  geom_point(aes(y = cum_incurred_loss_ratio, color = 'Actual')) +
  geom_line(aes(y = cum_estimate_loss_ratio, color = 'Predicted')) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    values = c('Predicted' = '#317CEF', 'Actual' = '#E1337C'),
    name = 'Incurred Loss Ratio') +
  scale_fill_manual(values = c('95% CI' = '#F8CC6B'), name = NULL) +
  guides(color = guide_legend(order = 1)) +
  labs('Estimated Ultimate LR and 95% CI', y = 'Loss Ratio', x = 'Development Month') +
  facet_wrap(~ uwq)



# ---- Model Comparison ----
# Can't compare them since growth curve only predicts cumulative paid
fit_growth_curve <- add_criterion(fit_growth_curve, criterion = 'loo')
fit_cum_paid <- add_criterion(fit_cum_paid, criterion = 'loo')
fit_incr_paid <- add_criterion(fit_incr_paid, criterion = 'loo')

loo_compare(fit_growth_curve, fit_cum_paid, fit_incr_paid)

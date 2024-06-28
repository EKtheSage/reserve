
# RLR: Reported Loss Ratio
# RRF: Reserve Robust Factor
# RLM: Reported Loss Ratio Multiplier
# RRM: Reserve Robustness Change Multiplier
# EX: Exposure
# OS: Outstanding Claims
# PD: Paid Claims
# k_er: Rate of exposure expiration as claims are reported
# k_p: Speed of claims settlement

# the rate of Outstanding Claims is equal to
# k_er * RLR * EX - k_p * OS

# OS(t) is the difference between two exponential decay curves
# which shows how oustanding losses decay as payments are made

if (!exists('create_loss_data', mode = 'function')) source('./scripts/00-init.R')

cas_data <- fread('https://www.casact.org/sites/default/files/2021-04/comauto_pos.csv')

loss_data <- create_loss_data(cas_data, company_code = 353, loss_type = 'paid')

model_data <- melt(
  loss_data,
  id.vars = c('accident_year', 'dev', 'cal_year', 'premium', 'split'),
  measure.vars = c(
    'incr_paid_loss_ratio', 'os_loss_ratio'
  ),
  value.name = 'loss_ratio'
)

model_data[
  , c('delta', 'deltaf', 'loss_ratio_train', 'loss_ratio_test') :=
  .(
    ifelse(variable == 'incr_paid_loss_ratio', 1, 0),
    ifelse(variable == 'incr_paid_loss_ratio', 'paid', 'os'),
    ifelse(split == 'train', loss_ratio, NA),
    ifelse(split == 'test', loss_ratio, NA)
  )
]


myFunsIncrPaid <- '
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

frml <- bf(
  loss_ratio_train ~ eta,
  nlf(eta ~ log(incrclaimsprocess(dev, 1.0, ker, kp, RLR, RRF, delta))),
  nlf(ker ~ 3 * exp(oker * 0.1)),
  nlf(kp ~ 1 * exp(okp * 0.1)),
  nlf(RLR ~ 0.7 * exp(oRLR * 0.2)),
  nlf(RRF ~ 0.8 * exp(oRRF * 0.1)),
  oRLR ~ 1 + (1 | ID |accident_year) + (1 | dev),
  oRRF ~ 1 + (1 | ID |accident_year) + (1 | dev),
  oker ~ 1 + (1 | accident_year) + (1 | dev),
  okp ~ 1 + (1 | accident_year) + (1 | dev),
  sigma ~ 0 + deltaf,
  nl = TRUE
)

mypriors <- c(
  prior(normal(0, 1), nlpar = 'oRLR'),
  prior(normal(0, 1), nlpar = 'oRRF'),
  prior(normal(0, 1), nlpar = 'oker'),
  prior(normal(0, 1), nlpar = 'okp'),
  prior(normal(log(0.2), 0.2), class = 'b', coef = 'deltafpaid', dpar = 'sigma'),
  prior(normal(log(0.2), 0.2), class = 'b', coef = 'deltafos', dpar = 'sigma'),
  prior(student_t(10, 0, 0.3), class = 'sd', nlpar = 'oker'),
  prior(student_t(10, 0, 0.3), class = 'sd', nlpar = 'okp'),
  prior(student_t(10, 0, 0.7), class = 'sd', nlpar = 'oRLR'),
  prior(student_t(10, 0, 0.5), class = 'sd', nlpar = 'oRRF'),
  prior(lkj(1), class = 'cor')
)

fit <- brm(
  frml,
  data = model_data[split == 'train'],
  family = brmsfamily('lognormal', link_sigma = 'log'),
  prior = mypriors,
  stanvars = stanvar(scode = myFunsIncrPaid, block = 'functions'),
  backend = 'cmdstan',
  file = 'hierarchical-compartmental-incrpaid-os-lognorm',
  file_refit = 'on_change',
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 42, iter = 2000, chains = 4
)

fit

x <- summarize_draws(
  as_draws_array(fit, variable = '^b', regex = TRUE),
  mean, sd, ~quantile2(.x, probs = c(0.025, 0.975)))

expose_functions(fit, vectorize = TRUE)
fitted(fit)

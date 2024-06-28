
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
    'paid_loss', 'os_loss'
  ),
  value.name = 'loss_amount'
)

model_data[
  , c('delta', 'deltaf', 'loss_amount_train', 'loss_amount_test') :=
  .(
    ifelse(variable == 'paid_loss', 1, 0),
    ifelse(variable == 'paid_loss', 'paid', 'os'),
    ifelse(split == 'train', loss_amount, NA),
    ifelse(split == 'test', loss_amount, NA)
  )
]


myFunsCumPaid <- '
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

# see ?brmsformula, model group-levels terms of the same grouping factor as correlated
# ID is not a variable in the data but just a symbol to indicate correlations
# between multiple group-level terms.
frml <- bf(
  loss_amount_train ~ premium * claimsprocess(dev, ker, kp, RLR, RRF, delta),
  nlf(ker ~ 3 * exp(oker * 0.1)),
  nlf(kp ~ 1 * exp(okp * 0.1)),
  nlf(RLR ~ 0.7 * exp(oRLR * 0.2)),
  nlf(RRF ~ 0.8 * exp(oRRF * 0.1)),
  oRLR ~ 1 + (1 | ID | accident_year),
  oRRF ~ 1 + (1 | ID | accident_year),
  oker ~ 1, okp ~ 1, sigma ~ 0 + deltaf,
  nl = TRUE
)

mypriors <- c(
  prior(normal(0, 1), nlpar = 'oRLR'),
  prior(normal(0, 1), nlpar = 'oRRF'),
  prior(normal(0, 1), nlpar = 'oker'),
  prior(normal(0, 1), nlpar = 'okp'),
  prior(student_t(1, 0, 1000), class = 'b', coef = 'deltafpaid', dpar = 'sigma'),
  prior(student_t(1, 0, 1000), class = 'b', coef = 'deltafos', dpar = 'sigma'),
  prior(student_t(10, 0, 0.2), class = 'sd', nlpar = 'oRLR'),
  prior(student_t(10, 0, 0.1), class = 'sd', nlpar = 'oRRF'),
  prior(lkj(1), class = 'cor'))

fit <- brm(
  frml,
  data = model_data[split == 'train'], family = gaussian(),
  prior = mypriors,
  stanvars = stanvar(scode = myFunsCumPaid, block = 'functions'),
  backend = 'cmdstan',
  file = 'hierarchical-compartmental-cumpaid-os-gaussian',
  file_refit = 'on_change',
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

fit

x <- summarize_draws(
  as_draws_array(fit, variable = '^b', regex = TRUE),
  mean, sd, ~quantile2(.x, probs = c(0.025, 0.975)))

expose_functions(fit, vectorize = TRUE)
fitted(fit)
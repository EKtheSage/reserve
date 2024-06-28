
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

setnames(
  cas_data,
  c('EarnedPremNet_C', 'AccidentYear', 'DevelopmentLag', 
    'IncurLoss_C', 'CumPaidLoss_C', 'BulkLoss_C'),
  c('premium', 'accident_year', 'dev',
    'incurred_loss', 'paid_loss', 'bulk_loss')
)

loss_data <- create_loss_data(cas_data, company_code = 353, loss_type = 'paid')


library(ChainLadder)
data(GenIns)

loss_data <- data.table(
  accident_year = rep(1991:2000, 10),
  dev = sort(rep(1:10, 10)),
  premium = rep(10000000+400000*0:9, 10),
  cum_loss = as.vector(GenIns),
  incr_loss = as.vector(cum2incr(GenIns))
)[order(accident_year, dev),
  `:=`(cum_lr = cum_loss/premium,
       incr_lr = incr_loss/premium)]

myFuns <- '
vector compartmentmodel(real t, vector y, vector theta) {
  vector[3] dydt;
  real ke = theta[1];
  real dr = theta[2];
  real kp1 = theta[3];
  real kp2 = theta[4];
  
  dydt[1] = pow(ke, dr) * pow(t, dr - 1) * exp(-t * ke)/tgamma(dr)
          - (kp1 + kp2) * y[1]; // Exposure
  dydt[2] = kp2 * (y[1] - y[2]); // Outstanding
  dydt[3] = (kp1 *  y[1] + kp2 * y[2]); // Paid
 
  return dydt;
}
real lossemergence(real t, real devfreq, real ke, real dr, 
                   real kp1, real kp2){
    vector[3] y0;
    array[1] vector[3] y;
    vector[4] theta;
    theta[1] = ke;
    theta[2] = dr;
    theta[3] = kp1;
    theta[4] = kp2;
    real out; 
    // Set initial values
    y0[1] = 0; // Exposure
    y0[2] = 0; // Outstanding
    y0[3] = 0; // Paid
    
    y = ode_rk45(compartmentmodel, y0, 0, rep_array(t, 1), theta);
    out = y[1, 3];
    
    if(t > devfreq){ // paid greater dev period 1
    y = ode_rk45(compartmentmodel, y0, 0, rep_array(t - devfreq, 1), theta);
    // incremental paid
     out = out - y[1, 3];
    }
    
    return(out);
}
'

frml <- bf(
  incr_lr ~ eta,
  nlf(eta ~ log(ELR * lossemergence(dev, 1.0, ke, dr, kp1, kp2))),
  nlf(ke ~ exp(oke * 0.5)),
  nlf(dr ~ 1 + 0.1 * exp(odr * 0.5)),
  nlf(kp1 ~ 0.5 * exp(okp1 * 0.5)),
  nlf(kp2 ~ 0.1 * exp(okp2 * 0.5)),
  ELR ~ 1 + (1 | accident_year), 
  oke  ~ 1 + (1 | accident_year), odr ~ 1 + (1 | accident_year), 
  okp1 ~ 1 + (1 | accident_year), okp2 ~ 1 + (1 | accident_year),
  nl = TRUE
)

mypriors <- c(
  prior(inv_gamma(4, 2), nlpar = 'ELR', lb = 0),
  prior(normal(0, 1), nlpar = 'oke'),
  prior(normal(0, 1), nlpar = 'odr'),
  prior(normal(0, 1), nlpar = 'okp1'),
  prior(normal(0, 1), nlpar = 'okp2'),
  prior(student_t(10, 0, 1), class = 'sd', nlpar = 'ELR'),
  prior(student_t(10, 0, 1), class = 'sd', nlpar = 'oke'),
  prior(student_t(10, 0, 1), class = 'sd', nlpar = 'odr'),
  prior(student_t(10, 0, 1), class = 'sd', nlpar = 'okp1'),
  prior(student_t(10, 0, 1), class = 'sd', nlpar = 'okp2'),
  #class = sigma for lognormal family to account for residual sd
  prior(student_t(10, 0, 1), class = 'sigma') 
)

fit <- brm(
  frml,
  data = loss_data, family = lognormal(), 
  prior = mypriors,
  stanvars = stanvar(scode = myFuns, block = 'functions'),
  backend = 'cmdstan',
  file = 'hierarchical-compartmental-model-wide-prior',
  file_refit = 'on_change',
  control = list(adapt_delta = 0.99)
)

fit

x <- summarize_draws(
  as_draws_array(fit, variable = '^b', regex = TRUE),
  mean, sd, ~quantile2(.x, probs = c(0.025, 0.975)))

expose_functions(fit, vectorize = TRUE)
fitted(fit)
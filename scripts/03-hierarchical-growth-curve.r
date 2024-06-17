cas_data <- fread('https://www.casact.org/sites/default/files/2021-04/comauto_pos.csv')

setnames(
  cas_data,
  c('EarnedPremNet_C', 'AccidentYear', 'DevelopmentLag', 
    'IncurLoss_C', 'CumPaidLoss_C', 'BulkLoss_C'),
  c('premium', 'accident_year', 'dev',
    'incurred_loss', 'paid_loss', 'bulk_loss')
)

loss_data <- create_loss_data(cas_data, company_code = 353, loss_type = 'paid')

loss_data[, 'split' := ifelse(is.na(loss_train), 'test', 'train')]

fit <- brm(
  bf(
    paid_loss ~ ult * (1 - exp(-(dev/theta)^omega)),
    ult ~ 1 + (1 | accident_year),
    omega ~ 1, theta ~ 1,
    nl = TRUE
  ),
  data = loss_data, family = gaussian(),
  prior = c(
    prior(normal(5000, 1000), nlpar = 'ult'),
    prior(normal(1, 2), nlpar = 'omega'),
    prior(normal(45, 10), nlpar = 'theta')
  ),
  backend = 'cmdstan',
  control = list(adapt_delta = 0.99)
)

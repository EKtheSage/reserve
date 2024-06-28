if (!exists('create_loss_data', mode = 'function')) source('./scripts/00-init.R')

cas_data <- fread('https://www.casact.org/sites/default/files/2021-04/comauto_pos.csv')

loss_data <- create_loss_data(cas_data, company_code = 353, loss_type = 'paid')

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
  file = 'hierarchical-growth-curve',
  file_refit = 'on_change',
  control = list(adapt_delta = 0.99)
)

conditions <- data.table(
  accident_year = unique(loss_data$accident_year))

me_loss <- conditional_effects(
  fit, conditions = conditions,
  re_formula = NULL, method = 'predict'
)

plot(me_loss, ncol = 5, points = TRUE)

pp_check(fit)
pp_check(fit, type = 'error_hist', ndraws = 11)
pp_check(fit, type = "scatter_avg", ndraws = 100)
pp_check(fit, type = "stat_2d")
pp_check(fit, type = "ppc_loo_pit_qq")

ggplot(loss_data, aes(dev, loss, group = 1)) +
  geom_point(aes(color = split)) +
  geom_line(aes(color = split)) + 
  labs(
    title = 'Cumulative Paid Loss by AY',
    x = 'Development Year',
    y = NULL
  ) +
  facet_wrap(~ accident_year, scales = 'free_y')

# to-do: 
# 1. posterior predictive
# 2. split data into train and test - current model 
# runs on all data
# 3. get ELPD for loo comparison
# 4. use bayesblend from python?
# 5. How to make prediction based on weights of models?
# brms::posterior_average seems promising

# loss_data <- cbind(
#   loss_data,
#   pred_mean = ppc_loss$mean,
#   pred_median = ppc_loss$median,
#   pred_q2.5 = ppc_loss$q2.5,
#   pred_q97.5 = ppc_loss$q97.5
# )

# ggplot(loss_data, aes(dev, loss)) +
#   geom_point(aes(color = split)) +
#   geom_line() +
#   scale_x_continuous(breaks = seq(2, 10, by = 2)) + 
#   facet_wrap(~ accident_year)

# ggplot(loss_data, aes(dev, loss)) +
#   geom_point(aes(color = split)) +
#   geom_line(aes(dev, pred_mean, color = 'Prediction Mean')) +
#   geom_ribbon(
#     aes(ymin = pred_q2.5, ymax = pred_q97.5, fill = '95% Credible Interval'), 
#     alpha = 0.25) +
#   scale_x_continuous(breaks = seq(2, 10, by = 2)) + 
#   scale_color_manual(
#     values = c(
#       'train' = '#317CEF',
#       'test' = '#FBCC6B',
#       'Prediction Mean' = '#1A0B60'),
#     labels = c(
#       'train' = 'Train',
#       'test' = 'Test',
#       'Prediction Mean' = 'Prediction Mean'
#     )) +
#   scale_fill_manual(values = c('95% Credible Interval' = '#5ACAFA')) +
#   labs(
#     'title' = 'AY Predicted Loss and 95% Credible Interval',
#     x = 'Development Year', y = NULL,
#     color = NULL, fill = NULL) +
#   facet_wrap(~ accident_year, ncol = 5)

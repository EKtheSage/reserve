library(curl)
library(data.table)
library(ggplot2)
library(brms)
library(cmdstanr)
library(posterior)
library(loo)
library(bayesplot)
library(showtext)

set_cmdstan_path('/opt/conda/envs/reserve/bin/cmdstan/')
options(mc.cores = 4)
options(cmdstanr_write_stan_file_dir = paste0(getwd(), '/scripts'))

font_add_google('Nunito', 'nunito')
font_add_google('Inter', 'inter')
showtext_auto()

theme_set(
  theme_minimal() +
    theme(text = element_text(family = 'nunito'))
)

create_loss_data <- function(cas_data, company_code, loss_type = 'incurred') {

  comp_data <- cas_data[
    GRCODE == company_code,  # nolint: object_usage_linter.
    c('premium', 'accident_year', 'dev', 'incurred_loss', 'paid_loss', 'bulk_loss')
  ]

  comp_data <- comp_data[, `:=`(
    origin = accident_year - min(accident_year) + 1,
    # origin period starting from 1, id to identify first origin period
    origin1id = ifelse(accident_year == min(accident_year), 0,  1))]

  # set losses < 0 to 1, as we will take the log later
  if (loss_type %in% "incurred") {
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
  
  train_test[, 'split' := ifelse(is.na(loss_train), 'test', 'train')]
  train_test[
    , 'incr_loss' := pmax(1e-6, loss - shift(loss, fill = 0)) # needs to be gt 0
    , by = accident_year
  ]
  train_test[, 'incr_lr' := incr_loss / premium]
  return(train_test[])
}

create_stan_data <- function(loss_data, log_amt = TRUE) {
  if (log_amt) {
    prem <- log(loss_data[, premium])
    loss <- log(loss_data[!is.na(loss_train), loss_train])
  } else {
    prem <- loss_data[, premium]
    loss <- loss_data[!is.na(loss_train), loss_train]
  }

  list(
    len_data = loss_data[!is.na(loss_train), .N],
    len_pred = loss_data[is.na(loss_train), .N],
    prem = prem,
    loss = loss,
    origin = loss_data[, origin],
    dev = loss_data[, dev],
    origin1id = loss_data[, origin1id]

  )
}
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
options(mc.cores = parallel::detectCores())
options(cmdstanr_write_stan_file_dir = paste0(getwd(), '/scripts'))

font_add_google('Nunito', 'nunito')
font_add_google('Inter', 'inter')
showtext_auto()

theme_set(
  theme_minimal() +
    theme(text = element_text(family = 'nunito'))
)

create_loss_data <- function(cas_data, company_code, loss_type = 'incurred') {

  setnames(
    cas_data,
    c('EarnedPremNet_C', 'AccidentYear', 'DevelopmentLag',
      'IncurLoss_C', 'CumPaidLoss_C', 'BulkLoss_C'),
    c('premium', 'accident_year', 'dev',
      'incurred_loss', 'paid_loss', 'bulk_loss')
  )


  comp_data <- cas_data[
    GRCODE == company_code,  # nolint: object_usage_linter.
    c('premium', 'accident_year', 'dev', 'incurred_loss', 'paid_loss', 'bulk_loss')
  ]

  comp_data <- comp_data[, `:=`(
    origin = accident_year - min(accident_year) + 1,
    # origin period starting from 1, id to identify first origin period
    origin1id = ifelse(accident_year == min(accident_year), 0,  1))]

  comp_data[, cal_year := accident_year + dev - 1]

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
  train_test[, 'paid_loss_ratio' := paid_loss / premium]
  train_test[, 'os_loss' := incurred_loss - paid_loss]
  train_test[, 'os_loss_ratio' := (incurred_loss - paid_loss) / premium]
  train_test[, 'incr_paid_loss_ratio' := incr_lr]

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

# set_cmdstan_path('/workspaces/reserve/cmdstan-2.35.0')
# install_cmdstan(
#   dir = paste(getwd(), '.cmdstan', sep = '/'),
#   cpp_options = list(
#     'TBB_CXX_TYPE' = 'gcc',
#     'TBB_INTERFACE_NEW' = 'true',
#     'STAN_HAS_CXX17' = 'true',
#     'TBB_INC' = '/opt/conda/envs/reserve/include/',
#     'TBB_LIB' = '/opt/conda/envs/reserve/lib/'
#   ),
#   overwrite = TRUE
# )

# https://discourse.mc-stan.org/t/segfault-when-using-brms-cmdstanr-compile-model-methods-true/33771/3
# cmdstan_make_local(
#   cpp_options = list(
#     'STAN_THREADS' = TRUE)
# )

# rebuild_cmdstan()

# caching of compiled stan functions
# https://github.com/stan-dev/cmdstanr/issues/870
cmdstan_expose_functions <- function(...) {
  pseudo_model_code <- paste(c("functions {", ..., "}"), collapse="\n")
  functions_hash <- rlang::hash(pseudo_model_code)
  model_name <- paste0("model-functions-", functions_hash)
  ## note: cmdstanr somehow only compiles standalone functions
  ## whenever one is compiling the model (and not allowing to export
  ## the functions if one is not compiling it). This is why
  ## force_compile=TRUE is a save option
  ##pseudo_model <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(pseudo_model_code), compile_standalone=TRUE, force_compile=TRUE, stanc_options=list(name=paste0("model-functions-", functions_hash)))
  ##pseudo_model$functions
  ## but things seem to work ok if we abuse a bit the internals... tested with cmdstanr 0.6.1
  ## note that we have to set the model name manually to a
  ## determinstic string (depending only on the stan functions being
  ## compiled)
  stan_file <- cmdstanr::write_stan_file(pseudo_model_code)
  pseudo_model <- cmdstanr::cmdstan_model(stan_file, stanc_options=list(name=model_name))
  pseudo_model$functions$existing_exe <- FALSE
  pseudo_model$functions$external <- FALSE
  stancflags_standalone <- c("--standalone-functions", paste0("--name=", model_name))
  pseudo_model$functions$hpp_code <- cmdstanr:::get_standalone_hpp(stan_file, stancflags_standalone)
  pseudo_model$expose_functions(FALSE, FALSE) ## will return the functions in an environment
  pseudo_model$functions
}

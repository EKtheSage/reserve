# project library cache dir
Sys.setenv('RENV_PATHS_CACHE' = paste0(
  getwd(),
  '.cache/R/renv/'
  )
)
options(
  repos = c(
    CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest'
  ),
  renv.config.ppm.enabled = TRUE,
  renv.config.repos.override = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest'
)
renv::init()
options(
  repos = c(
    CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest',
  ),
  renv.config.ppm.enabled = TRUE,
  renv.config.repos.override = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest'
) 

renv::restore(repos = getOption('repos'))
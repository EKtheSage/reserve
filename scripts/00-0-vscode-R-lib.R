# VSCode-R library cache dir
Sys.setenv('RENV_PATHS_CACHE' = '/workspaces/reserve/.cache/R/vscode-R')
options(
  repos = c(
    CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest'
  ),
  renv.config.ppm.enabled = TRUE,
  renv.config.repos.override = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest'
)
renv::init(project = '/workspaces/reserve/lib/vscode-R', repos = getOption('repos'))

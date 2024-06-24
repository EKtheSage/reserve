#!/bin/bash

eval "$(micromamba shell hook --shell=bash)"

micromamba activate reserve

export RENV_PATHS_CACHE="$PWD/.cache/R/renv"

Rscript /tmp/restore.R

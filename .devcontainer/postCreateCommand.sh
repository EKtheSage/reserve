#!/bin/bash

eval "$(micromamba shell hook --shell=bash)"

micromamba activate reserve

export RENV_PATHS_CACHE="$PWD/.cache/R/renv/v5/R-4.3/x86_64-conda-linux-gnu"

Rscript /tmp/restore.R

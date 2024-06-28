#!/bin/bash

eval "$(micromamba shell hook --shell=bash)"

micromamba activate reserve

export RENV_PATHS_CACHE="$PWD/.cache/R/renv"

# Path to the make/local file
MAKE_LOCAL_FILE="$CONDA_PREFIX/bin/cmdstan/make/local"

# Check if the line already exists in the make/local file
# append this flag to local stan environment variables file
if ! grep -Fxq "STAN_HAS_CXX17=true" "$MAKE_LOCAL_FILE"; then
    echo "STAN_HAS_CXX17=true" >> "$MAKE_LOCAL_FILE"
fi

Rscript /tmp/restore.R

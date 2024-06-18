#!/bin/bash

eval "$(micromamba shell hook --shell=bash)"

micromamba activate reserve

R -e "renv::restore()"

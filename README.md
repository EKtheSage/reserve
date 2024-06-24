# reserve

## Setup

1. Clone the repo
2. Devcontainer Rebuild the container
3. If this is the first time launching the container in a new folder, then need to change directory to lib/vscode-R, open R in the directory, renv should ask you to restore the libraries (languageserver, httpgd) for autocomplete and plotting
4. Reload VSCode for the setup to take effect

## Bayesian Environment

In the scripts folder, we have several models:

1. Correlated ChainLadder (Incurred)
2. Changing Settlement Rate (Paid)
3. Hierarchical Growth Curve (Paid)
    * By LOB, UW Dates
4. Compartmental Models
    * Multiple Versions of Compartmental Models
5. Model Comparison using Bayesian LOO
    * Stacking Weight for optimal model
6. Backtesting
    * Quarterly review starting in 2020-Q1
    * 2020 - 2023 Q4 = 16 snapshot

* Also show error metrics
* Parameters
* Best Estimate Reserve Credible Interval





# RLR: Reported Loss Ratio
# RRF: Reserve Robust Factor
# RLM: Reported Loss Ratio Multiplier
# RRM: Reserve Robustness Change Multiplier
# EX: Exposure
# OS: Outstanding Claims
# PD: Paid Claims
# k_er: Rate of exposure expiration as claims are reported
# k_p: Speed of claims settlement

# the rate of Outstanding Claims is equal to
# k_er * RLR * EX - k_p * OS

# OS(t) is the difference between two exponential decay curves
# which shows how oustanding losses decay as payments are made
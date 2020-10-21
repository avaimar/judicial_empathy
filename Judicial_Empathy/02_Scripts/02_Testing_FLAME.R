# INFO -------------------------------------------
# Script: 01_Testing_FLAME.R
# Inputs:
#   * None
# Outputs:
#   * None
# Note: Code below is sourced from the webpage Getting Started
# with FLAME as written by the authors. This script is meant to 
# explore the package's functionality.
# https://almostmatchingexactly.github.io/flame.html

# 0. Working set up ------------------------------
# Libraries

# Install FLAME
# devtools::install_github('https://github.com/vittorioorlandi/FLAME')
library('FLAME')

# 1. Exploring functionality -----------------------------------
# Generate a toy dataset with 250 units and 5 covariates
data <- gen_data(n = 250, p = 5)

# Run FLAME 
FLAME_out <- FLAME(data = data, 
                   treated_column_name="treated", 
                   outcome_column_name="outcome")


# FLAME_out is a list including:
#   1. data: the data frame with an added Boolean column 'matched' indicating
#   whether the unit was matched, and a 'weight' column indicating how many 
#   times the unit was matched.
#   2. MGs: a list of every matched group formed
#   3. CATEs: a vector of the CATE for each of the matched groups
#   4. matched_on: a list of 31 indicating the covariate values being matched on
#   for each group.
#   5. Matching covariates: a list of 1 including the covariates being matched on
#   6. dropped: a vector of the covariate dropped at each iteration

# Find the matched groups of units 1 and 2: returns a list with elements for
# each particular unit, including the matched units, covariate values, outcome
# and treatment status.
# We see one control in each of the following matched groups. 
MG(c(1,2), FLAME_out)

# Here we see unit 10, which appeared in unit 1's group, has different controls in
# its group.
MG(10, FLAME_out)

# CATEs of particular units
CATE(c(1,2), FLAME_out)

# ATE and ATT
ATE(FLAME_out = FLAME_out)
ATT(FLAME_out = FLAME_out)

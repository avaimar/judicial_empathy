# INFO -------------------------------------------
# Script: 06_Analysis.R
# Inputs:
#   * Modified judge data (including matches)
#     File: Judicial_empathy/03_Output/03_Matching/
#   * Cleaned case data
#     File: Judicial_empathy/01_data/02_Cleaned_data/cases_cleaned.csv
# Outputs:
#   * Plots and latex tables
#     Location: Judicial_empathy/03_Output/04_Analysis

# 0. Working set up ------------------------------
# Libraries
library(data.table) # Table manipulation
library(DOS2) 
library(optmatch)
library(sensitivitymult)
library(ggplot2) # Plots

# Output directory
output <- 'Judicial_empathy/03_Output/04_Analysis/'

# Helper functions
source('Judicial_empathy/02_Scripts/00_utility.R')

# Parameters
#selected_match <- 'Judicial_empathy/03_Output/03_Matching/[INSERT MATCH HERE].csv'
selected_match <- '../trial_export'

# 1. Load data ------------------------------------
# Matched judge data
data_judges <- fread(selected_match)

# Cleaned case data
data_cases <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/cases_cleaned.csv')

# 2. Judge data -----------------------------------
# cleaned_judges <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/judges_cleaned.csv')

# * 2.1 FRT ---------------------------------------

# matches_transformed <- cast.senm(data_judges, data_judges$matches)
# FRT = senm(matches_transformed$y, matches_transformed$z, matches_transformed$mset, trim=Inf)
# FRT$pval
# hopefully above pval will be smaller than 0.05

# * 2.2 Sensitivity analysis ----------------------
## test out various values of gamma until we find one that gives us an insignificant result?
## something similar to this?

# gammas = seq(from = 1, to = 5, by = 0.05)
# max_gamma = 1
# for (i in  gammas) {
#     sen = senm(matches_transformed$y, matches_transformed$z, matches_transformed$mset, trim=Inf, gamma = i)
#     if (sen$pval > 0.05) {
#         max_gamma = i
#         break
#     }
# }
# max_gamma

# * 2.3 Sensibility analysis ----------------------

# age, religion, birth year, race
# grouped = data_judges %>% group_by(matches, z) 
#          %>% summarize(average_outcome = mean(progressive.vote)
# followed by something like diffs <- grouped %>% group_by(matches) %>% 
#          %>% summarize(differences = diff(average_outcome)
# display boxplots of differences in outcomes before and after removing the N/A
# observations. One set of boxplots for each variable of interest? 

# * 2.4 Heterogeneous effects ---------------------
# x: covariate of interest (e.g woman, republican, etc.)
# we should only consider those pairs that match exactly on the covariate
# wolcox.test(differences[differences$x == 0]$average_outcome,
#             differences[differences$x ==1]$average_outcome, mu=0, paired=False)

# 3. Case data ------------------------------------
# * 3.1 Obtain residuals --------------------------

# * 3.2 FRT ---------------------------------------

# * 3.3 Sensitivity analysis ----------------------

# * 3.4 Sensibility analysis ----------------------

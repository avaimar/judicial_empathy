# INFO -------------------------------------------
# Script: 05_Matching.R
# Inputs:
#   * Cleaned case data (as modified for the project)
#     File: Judicial_empathy/01_Data/01_Cleaned_data/judges_cleaned.csv !!!!!!!!!!!!!!!!!!
# Outputs:
#   * Matched pairs to be used for analysis at 03_Output/03_Matching/

# 0. Working set up ------------------------------
# Libraries
library(data.table) # Table manipulation
library(DOS2) 
library(optmatch)
library(RItools)
library(ggplot2) # Plots

# Output directory
output <- 'Judicial_empathy/03_Output/03_Matching/'

# 1. Load data ------------------------------------
data_judges <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/judges_cleaned.csv')

# Drop outcome variable
data_judges <- data_judges[, progressive.vote := NULL]

# 2. Calculate propensity score ------------------------------
ps_model <- glm(formula = treatment ~ circuit.1 + child + woman + age + race + religion + 
                  republican + no_cases + agemiss + religmiss + racemiss,
                family = binomial, data = data_judges)
data_judges <- data_judges[, pscore := ps_model$fitted.values]

# Match 1. Robust Mahalanobis distance for all covariates -------
m1.DM <- smahal(z = treatment,
             X = data_judges[, .(circuit.1, child, woman, age, race, religion,
                                 republican, no_cases, agemiss, religmiss, racemiss)])

m1.match <- pairmatch(x = m1.DM, data=data_judges)
m1.summary <- summarize.match(as.data.frame(data), m1.match, ps.name = 'pscore')

# Plot
plot(xBalance(
  treatment ~ circuit.1 + child + woman + age + race + religion + 
    republican + no_cases + agemiss + religmiss + racemiss + pscore + strata(m1.match) - 1, 
  data=data_judges))

# Match 2. RMahalanobis adding caliper -------------------------


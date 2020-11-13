# INFO -------------------------------------------
# Script: 05_Matching.R
# Inputs:
#   * Cleaned case data (as modified for the project)
#     File: Judicial_empathy/01_Data/01_Cleaned_data/judges_cleaned.csv
# Outputs:
#   * Matched pairs to be used for analysis at 03_Output/03_Matching/

# 0. Working set up ------------------------------
# Libraries
library(data.table) # Table manipulation
library(DOS2) 
library(optmatch)
library(RItools)
library(rcbalance)
library(ggplot2) # Plots

# Output directory
output <- 'Judicial_empathy/03_Output/03_Matching/'

# Helper functions
source('Judicial_empathy/02_Scripts/00_utility.R')

# 1. Load data ------------------------------------
data_judges <- 
  fread('Judicial_Empathy/01_Data/02_Cleaned_data/judges_cleaned.csv')

# Drop outcome variable
data_judges <- data_judges[, progressive.vote := NULL]

# Create vector of covariates (excluding dummies)
covars <- c('child', 'woman', 'age', 'republican', 'no_cases',
            'agemiss', 'religmiss', 'racemiss', 'circuit', 'race', 'religion')

# Create vector of covariates (including dummies)
covars_and_dummies <- c('child', 'woman', 'age', 'republican', 'no_cases',
                        'agemiss', 'religmiss', 'racemiss', 'circuit',
                        #grep('circuit', colnames(data_judges), value = TRUE),
                        grep('race.', colnames(data_judges), value = TRUE),
                        grep('religion.', colnames(data_judges), value = TRUE))

# 2. Get number of treatment and control units ----------
N_table <- data_judges[, .(Ni = .N), by = z]

# 3. Calculate propensity score ------------------------------
ps_model <- glm(formula = z ~ circuit + child + woman + age + race + religion + 
                  republican + no_cases + agemiss + religmiss + racemiss,
                family = binomial, data = data_judges)
data_judges <- data_judges[, pscore := ps_model$fitted.values]
sum(data_judges$pscore > 0.999)


ps_model <- glm(formula = z ~ circuit + woman + age + race + religion + 
                  republican + no_cases + agemiss + religmiss + racemiss,
                family = binomial, data = data_judges)
data_judges <- data_judges[, pscore := ps_model$fitted.values]
sum(data_judges$pscore > 0.999)

# 4. Matching --------------------------------------------------

# Match 1. Robust Mahalanobis distance (all covariates) 1:1
perform_matching(
  match_id = 'm1_RM_covars.png',
  dmatrix = 'MD',
  variables = covars,
  data = data_judges
)

# Match 2. RMahalanobis (all covariates) adding caliper 1:1
perform_matching(
  match_id = 'm2_RM_covars_c.png',
  dmatrix = 'MD',
  variables = covars,
  caliper = 0.1,
  data = data_judges
)

# Match 3. Robust Mahalanobis distance (using dummies) 1:1
perform_matching(
  match_id = 'm3_RM_cdummies.png',
  dmatrix = 'MD',
  variables = covars_and_dummies,
  caliper = 0,
  match_ratio = 1,
  data = data_judges
)

# Match 4. RMahalanobis (using dummies) adding caliper 1:1
perform_matching(
  match_id = 'm4_RM_cdummies_c.png',
  dmatrix = 'MD',
  variables = covars_and_dummies,
  caliper = 0.1,
  match_ratio = 1,
  data = data_judges
)

# Match 5. Robust Mahalanobis distance (all covariates) 1:2
perform_matching(
  match_id = 'm5_RM_covars_2.png',
  dmatrix = 'MD',
  variables = covars,
  caliper = 0,
  match_ratio = 2,
  data = data_judges
)

# Match 6. Almost exact on child
perform_matching(
  match_id = 'm6_RM_covars_1_am.png',
  dmatrix = 'MD',
  variables = covars,
  caliper = 0,
  match_ratio = 1,
  almost_exact_variables = c("child"),
  data = data_judges
)

# Match 7. Fine balance on circuit
perform_matching(
  match_id = 'm7_RM_covars_1_fb.png',
  dmatrix = 'MD',
  variables = covars,
  caliper = 0,
  match_ratio = 1,
  fine_balance_variables = c("circuit"),
  data = data_judges
)

# Match 8. Fine balance women and child
perform_matching(
  match_id = 'm8_RM_covars_1_fb.png',
  dmatrix = 'MD',
  variables = covars,
  caliper = 0,
  match_ratio = 1,
  fine_balance_variables = c("woman", "child"),
  data = data_judges
)

# Match 9. Exact balance women and child
perform_matching(
  match_id = 'm9_RM_covars_1_ne.png',
  dmatrix = 'MD',
  variables = covars,
  caliper = 0,
  match_ratio = 1,
  exact_variables = c('woman', 'child'),
  data = data_judges
)

# Match 10. Exact matching on woman
perform_matching(
  match_id = 'm10_RM_covars_1_e.png',
  dmatrix = 'MD',
  variables = covars,
  caliper = 0,
  match_ratio = 1,
  exact_variables = c('woman'),
  data = data_judges,
  export = paste0(output, 'match_10_dummies.csv')
)

# ----------- MATCH USED IN PROJECT -----------
# Match 10.2 Exact matching on woman, using dummies
perform_matching(
  match_id = 'm10_RM_covars_dummies_1_e.png',
  dmatrix = 'MD',
  variables = covars_and_dummies,
  caliper = 0,
  match_ratio = 1,
  exact_variables = c('woman'),
  data = data_judges,
  export = paste0(output, 'match10_dummy.csv')
)

# Match 10.3 Exact matching on woman, using dummies + fixed ratio 2:1
perform_matching(
  match_id = 'm10_RM_covars_dummies_2.png',
  dmatrix = 'MD',
  variables = covars_and_dummies,
  caliper = 0,
  match_ratio = 2,
  exact_variables = c('woman'),
  data = data_judges
)


# Match 11. Exact matching on woman, almost exact child
perform_matching(
  match_id = 'm11_RM_covars_1_e_ae.png',
  dmatrix = 'MD',
  variables = covars,
  caliper = 0,
  match_ratio = 1,
  almost_exact_variables = c('child'),
  exact_variables = c('woman'),
  data = data_judges
)

# Match 12. Exact matching on woman
perform_matching(
  match_id = 'm12_RM_covars_1_e.png',
  dmatrix = 'MD',
  variables = covars_and_dummies,
  caliper = 0,
  match_ratio = 1,
  exact_variables = c('woman'),
  data = data_judges
)

# Match 13. RM distance 1:2 and exact match on woman
perform_matching(
  match_id = 'm13_RM_covars_2.png',
  dmatrix = 'MD',
  variables = covars,
  caliper = 0,
  match_ratio = 2,
  exact_variables = c('woman'),
  data = data_judges
)


perform_matching(
  match_id = 'm13_RM_dummies.png',
  dmatrix = 'MD',
  variables = covars_and_dummies,
  caliper = 0.3,
  match_ratio = 2,
  exact_variables = c('woman'),
  data = data_judges,
  export = paste0(output, 'match13_dummy.csv')
)

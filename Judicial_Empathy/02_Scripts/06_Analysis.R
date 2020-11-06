# INFO -------------------------------------------
# Script: 06_Analysis.R
# Inputs:
#   * Modified judge data (including matches)
#     File: Judicial_empathy/03_Output/03_Matching/
#   * Cleaned judge data
#     File: Judicial_empathy/01_data/02_Cleaned_data/judges_cleaned.csv
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
library(bbplot) # Plots

# Output directory
output <- 'Judicial_empathy/03_Output/04_Analysis/'

# Helper functions
source('Judicial_empathy/02_Scripts/00_utility.R')

# Parameters
selected_match <- 'Judicial_empathy/03_Output/03_Matching/match10.csv'

# 1. Load data ------------------------------------
# Matched judge data
data_judges_matched <- fread(selected_match,
                             colClasses = c('character', rep('numeric', 44), 'factor'))

# Reincorporate outcomes
data_judges <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/judges_cleaned.csv')
data_judges_matched <- merge(data_judges_matched,
                             data_judges[, .(name, progressive.vote)], by = 'name')
rm(data_judges)

# Cleaned case data
data_cases <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/cases_cleaned.csv')

# 2. Judge data -----------------------------------
# * 2.1 FRT ---------------------------------------
matches_transformed <- cast.senm(data_judges_matched, data_judges_matched$matches)

FRT <- senm(y = matches_transformed$y,
            z = matches_transformed$z, 
            mset = matches_transformed$mset, 
            gamma = 1,
            inner = 0,
            tau = 0,
            trim=Inf,
            alternative = 'greater')

print('Gamma = 1 pvalue: ', FRT$pval)
# hopefully above pval will be smaller than 0.05

# * 2.2 Sensitivity analysis ----------------------
## test out various values of gamma until we find one that gives us an insignificant result?
## something similar to this?

gammas <- seq(from = 1, to = 5, by = 0.05)
max_gamma = 1
for (i in gammas) {
    sen <- senm(y = matches_transformed$y,
               z = matches_transformed$z, 
               mset = matches_transformed$mset, 
               gamma = i,
               inner = 0,
               tau = 0,
               trim=Inf,
               alternative = 'greater')
    if (sen$pval > 0.05) {
        max_gamma <- i
        break
    }
}
print('Sensitivity gamma: ', max_gamma)

# * 2.3 Stability analysis ----------------------

# We have missing values for: Age, religion, race
# (Removed birth year as we did not include is as a covarariate)

# Obtain mean outcome per match and treatment group, and an indicator
# of whether any unit has a missing value for age, religion, race
grouped_data_judges <- 
  data_judges_matched[, .(avg_outcome = mean(progressive.vote),
                          agemiss = ifelse(sum(agemiss) > 0, 1, 0),
                          religmiss = ifelse(sum(religmiss) > 0, 1, 0),
                          racemiss = ifelse(sum(racemiss) > 0, 1, 0)),
                      by = .(matches, z)]

# Reshape so as to calculate differences between matched groups
grouped_data_judges <- dcast(grouped_data_judges,
                             formula = matches + agemiss + religmiss + racemiss ~ z,
                             value.var = 'avg_outcome')
grouped_data_judges <- grouped_data_judges[, outcome_diff := `1` - `0`]

# Drop units that were not matched
grouped_data_judges <- grouped_data_judges[!is.na(outcome_diff)]

# Create box plots of differences in outcomes before and after removing the N/A
# observations.

missing_var_differences <- data.table()
for (mis_variable in c('agemiss', 'religmiss', 'racemiss')) {
  
  # Create a subset of the data
  grouped_sub <- copy(grouped_data_judges[, .SD, .SDcols = c(mis_variable, 'outcome_diff')])
  
  # Tag and duplicate the data for ggplot
  grouped_sub <- grouped_sub[, type := 'All observations']
  eval(parse(text = paste0('missing_sub <- copy(grouped_sub[' ,mis_variable, " == 0])")))
  missing_sub <- missing_sub[, type := 'Non-missing observations']
  grouped_sub <- rbind(grouped_sub, missing_sub)
  
  # Tag with missing variable type
  grouped_sub <- grouped_sub[, variable := mis_variable]
  missing_var_differences <- rbind(missing_var_differences, 
                                   grouped_sub[, .(variable, type, outcome_diff)])
}

# Refactor missing variable for visualization purposes
missing_var_differences <- 
  missing_var_differences[, variable := factor(variable, 
                                               levels = c('agemiss', 'religmiss', 'racemiss'),
                                               labels = c('Age', 'Religion', 'Race'))]

# Plot
g <- ggplot(missing_var_differences, aes(y = outcome_diff, x = type)) + 
  geom_boxplot() +
  bbc_style() + 
  labs(x = '', y = 'Difference in outcomes') + 
  theme(axis.text = element_text(size=8),
        strip.text = element_text(size = 8)) + 
  facet_wrap(variable ~.)
ggsave(filename = paste0(output, '01_stability_analysis.png'), scale = 1.4,
       units = 'in', width = 6, height = 3)

rm(missing_var_differences, missing_sub, grouped_data_judges, grouped_sub)

# * 2.4 Heterogeneous effects ---------------------
# NOTE: THE BELOW CODE WILL ONLY WORK WITH 1:1 MATCHING
grouped_data_judges <- 
  data_judges_matched[, .(avg_outcome = mean(progressive.vote),
                          woman = sum(woman),
                          republican = sum(republican)),
                      by = .(matches, z)]

# Drop pairs that are not exactly matched 
# HERE -------------------------------------------------------
grouped_woman <- copy(grouped_data_judges[, .(woman = sum(woman)), by = matches])

# Reshape so as to calculate differences between matched groups
grouped_data_judges <- dcast(grouped_data_judges,
                             formula = matches + woman + republican ~ z,
                             value.var = 'avg_outcome')
grouped_data_judges <- grouped_data_judges[, outcome_diff := `1` - `0`]

# x: covariate of interest (e.g woman, republican, etc.)
# we should only consider those pairs that match exactly on the covariate
# wolcox.test(differences[differences$x == 0]$average_outcome,
#             differences[differences$x ==1]$average_outcome, mu=0, paired=False)

# 3. Case data ------------------------------------
# * 3.1 Obtain residuals --------------------------

# * 3.2 FRT ---------------------------------------

# * 3.3 Sensitivity analysis ----------------------

# * 3.4 Sensibility analysis ----------------------

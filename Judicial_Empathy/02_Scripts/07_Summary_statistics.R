# INFO -------------------------------------------
# Script: 07_Summary_statistics.R
# Inputs:
#   * Cleaned judge data
#     File: Judicial_empathy/01_data/02_Cleaned_data/judges_cleaned.csv
#   * Cleaned case data
#     File: Judicial_empathy/01_data/02_Cleaned_data/cases_cleaned.csv
# Outputs:
#   * Summary statistics tables

# 0. Working set up ------------------------------
# Libraries
library(data.table)
library(stargazer)
library(xtable)

# Output directory
output <- 'Judicial_empathy/03_Output/05_Summary_statistics/'

# 1. Load data ------------------------------------
# Judge data
data_judges <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/judges_cleaned.csv')

# Case data
data_cases <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/cases_cleaned.csv')

# 2. Judge data ------------------------------------
# Add missing indicator to age so as to compute statistics
data_judges <- data_judges[, age:= ifelse(age ==0, NA, age)]

# Get summary
summary_judges_horiz <- 
  data_judges[, .(n = .N, 
                  child = mean(child), 
                  girls = mean(girls), 
                  p_girls = mean(girls / child), 
                  age = mean(age, na.rm = TRUE),
                  woman = mean(woman), 
                  republican = mean(republican), 
                  no_cases = mean(no_cases), 
                  agemiss = mean(agemiss), 
                  religmiss = mean(religmiss), 
                  racemiss = mean(racemiss)), by = 'z']

# Transpose and add row names
summary_judges <- data.table::transpose(summary_judges_horiz,  keep.names = 'rn')
summary_judges <- 
  summary_judges[, rn:= factor(rn, 
                               levels = 
                                 c('z', 'n', 'child', 'girls', 'p_girls', 'age', 'woman',
                                   'republican', 'no_cases', 'agemiss', 'religmiss', 'racemiss'), 
                               labels =
                                 c('Assignment', 
                                   'Number of judges (N)', 
                                   'Mean number of children', 
                                   'Mean number of girls',
                                   'Percentage of girls', 
                                   'Mean age',
                                   'Mean percentage of female judges', 
                                   'Mean percentage of republican judges', 
                                   'Mean number of cases heard',
                                   'Missing age indicator (mean)',
                                   'Missing religion indicator (mean)',
                                   'Missing race indicator (mean)'))]
# Export
xtable(x = summary_judges, 
       label = 'judge_summary', 
       caption = 'Summary statistics of the 244 judges by treatment assignment',
       digits = 2
       )

# 3. Case data -----------------------------
summary_cases <- 
  data_cases[, .(n = .N, year = median(year, na.rm = TRUE)), by = .(area,z)] 

summary_cases <- dcast(summary_cases, formula = area ~ z,
                       value.var = c('n', 'year'))

summary_cases <- 
  summary_cases[, area := factor(area, 
                                 levels = c('Title IX', 'abortion', 'employment', 'pregnancy',
                                            'reproductive rights'),
                                 labels = c('Title IX', 'Abortion', 'Employment', 'Pregnancy',
                                            'Reproductive Rights'))]

xtable(x = summary_cases[, .(area, n_0, year_0, n_1, year_1)], 
       label = 'case_summary', 
       caption = 'Summary statistics of the 244 judges by treatment assignment',
       digits = 1
)

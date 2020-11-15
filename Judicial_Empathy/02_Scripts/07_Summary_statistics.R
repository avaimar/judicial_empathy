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
# Treatment VS Control
summary_judges_horiz_T <- 
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

# Full sample
summary_judges_horiz_A <- 
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
                  racemiss = mean(racemiss))]
summary_judges_horiz_A <- summary_judges_horiz_A[, z:= '2']
setcolorder(summary_judges_horiz_A, 'z')

# Bind
summary_judges_horiz <- rbind(summary_judges_horiz_T, summary_judges_horiz_A)

# Transpose
summary_judges <- data.table::transpose(summary_judges_horiz,  keep.names = 'rn')
columns <- c('V1', 'V2', 'V3')
summary_judges <- summary_judges[, (columns) := lapply(.SD, as.numeric), .SDcols = columns]


# Handle rounding
summary_judges <- summary_judges[rn == 'n', (columns) := lapply(.SD, as.integer), .SDcols = columns]
summary_judges <- summary_judges[rn == 'child', (columns) := round( .SD, 2), .SDcols = columns]
summary_judges <- summary_judges[rn == 'girls', (columns) := round( .SD, 2), .SDcols = columns]

summary_judges <- summary_judges[rn == 'p_girls', (columns) := lapply( .SD, function(x) x* 100), .SDcols = columns]
summary_judges <- summary_judges[rn == 'p_girls', (columns) := round( .SD, 2), .SDcols = columns]

summary_judges <- summary_judges[rn == 'age', (columns) := round( .SD, 2), .SDcols = columns]

summary_judges <- summary_judges[rn == 'woman', (columns) := lapply( .SD, function(x) x* 100), .SDcols = columns]
summary_judges <- summary_judges[rn == 'woman', (columns) := round( .SD, 2), .SDcols = columns]

summary_judges <- summary_judges[rn == 'republican', (columns) := lapply( .SD, function(x) x* 100), .SDcols = columns]
summary_judges <- summary_judges[rn == 'republican', (columns) := round( .SD, 2), .SDcols = columns]

summary_judges <- summary_judges[rn == 'no_cases', (columns) := round( .SD, 2), .SDcols = columns]

summary_judges <- summary_judges[rn == 'agemiss' | rn == 'religmiss' | rn == 'racemiss' , 
                                 (columns) := lapply( .SD, function(x) x* 100), .SDcols = columns]
summary_judges <- summary_judges[rn == 'agemiss' | rn == 'religmiss' | rn == 'racemiss' ,
                                 (columns) := round( .SD, 2), .SDcols = columns]

# Modify row names
summary_judges <- 
  summary_judges[, rn:= factor(rn, 
                               levels = 
                                 c('z', 'n', 'child', 'girls', 'p_girls', 'age', 'woman',
                                   'republican', 'no_cases', 'agemiss', 'religmiss', 'racemiss'), 
                               labels =
                                 c('Assignment', 
                                   'Number of judges (N)', 
                                   'Number of children (mean)', 
                                   'Number of girls (mean)',
                                   'Percentage of girls (mean)', 
                                   'Age (mean)',
                                   'Percentage of female judges (mean)', 
                                   'Percentage of republican judges (mean)', 
                                   'Number of cases heard (mean)',
                                   'Percentage of missing age values (mean)',
                                   'Percentage of missing religion values(mean)',
                                   'Percentage of missing race values (mean)'))]



# Export
judges_table <- xtable(x = summary_judges, 
       label = 'judge_summary', 
       caption = 'Summary statistics of the 199 judges with children by treatment assignment',
       digits = c(2, 2, 2, 2, 2),
       align = c('l', 'l', 'c', 'c' ,'c'))

print(judges_table, include.rownames=FALSE)

# 3. Case data -----------------------------
# Get table by treatment and for the full sample
summary_cases_T <- data_cases[, .(n = .N, year = median(year, na.rm = TRUE)), by = .(area,z)] 
summary_cases_T <- dcast(summary_cases_T, formula = area ~ z,
                       value.var = c('n', 'year'))

summary_cases_A <- 
  data_cases[, .(n = .N, year = median(year, na.rm = TRUE)), by = .(area)] 

# Bind tables
summary_cases_T <- summary_cases_T[order(area)]
summary_cases_A <- summary_cases_A[order(area)]
summary_cases <- cbind(summary_cases_T, summary_cases_A[, .(n, year)])

# Factor area
summary_cases <- 
  summary_cases[, area := factor(area, 
                                 levels = c('Title IX', 'abortion', 'employment', 'pregnancy',
                                            'reproductive rights'),
                                 labels = c('Title IX', 'Abortion', 'Employment', 'Pregnancy',
                                            'Reproductive Rights'))]

# Export
case_table <- xtable(x = summary_cases[, .(area, n_0, year_0, n_1, year_1, n, year)], 
       label = 'case_summary', 
       caption = 'Summary statistics of the 2,173 cases by case area and judge assignment',
       digits = 1
)

print(case_table, include.rownames=FALSE)

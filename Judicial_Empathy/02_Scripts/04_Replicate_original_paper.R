# INFO -------------------------------------------
# Script: 04_Replicate_original_paper.R
# Inputs:
#   * Cleaned case data (as per original paper)
#     File: Judicial_empathy/01_Data/01_Cleaned_data/judges_cleaned.csv
# Outputs:
#   * Regression tables at 03_Outputs/02_Original_Paper/

# 0. Working set up ------------------------------
# Libraries
library(data.table) # Table manipulation
library(stargazer) # Regression output formatting

# Output directory
output <- 'Judicial_empathy/03_Output/02_Original_Paper/'

# 1. Load data ------------------------------------
data_judges <- fread('Judicial_empathy/01_Data/02_Cleaned_data/judges_replication.csv')

# 2. Auxiliary variables -------------------------
total_cases <- sum(data_judges$no_cases)

# 3. Recreate models -----------------------------
# * 3.1 All judges --------------------------------------------------
model1 <- lm(formula = progressive.vote ~ factor(girls) + factor(child),
             data = data_judges, 
             weights = no_cases / total_cases)

model2 <- lm(formula = progressive.vote ~ as.numeric(girls > 0) + factor(child),
             data = data_judges, 
             weights = no_cases / total_cases)

model3 <- lm(formula = progressive.vote ~ as.numeric(girls > 0) + factor(child) + 
               factor(republican) + age + as.numeric(religion == 4) + woman + 
               as.numeric(race == 2) + as.numeric(race == 3),
             data = data_judges, 
             weights = no_cases / total_cases)

model4 <- lm(formula = progressive.vote ~ as.numeric(girls > 0) + factor(child) + 
               factor(republican) + age + as.numeric(religion == 4) + woman + 
               as.numeric(race == 2) + as.numeric(race == 3) + factor(circuit.1),
             data = data_judges, 
             weights = no_cases / total_cases)

# * 3.2 Judges with 1-4 children -------------------------------------
model5 <- lm(formula = progressive.vote ~ factor(girls) + factor(child),
             data = data_judges[child %in% c(1:4)], 
             weights = no_cases / total_cases)

model6 <- lm(formula = progressive.vote ~ as.numeric(girls > 0) + factor(child),
             data = data_judges[child %in% c(1:4)], 
             weights = no_cases / total_cases)

model7 <- lm(formula = progressive.vote ~ as.numeric(girls > 0) + factor(child) + 
               factor(republican) + age + as.numeric(religion == 4) + woman + 
               as.numeric(race == 2) + as.numeric(race == 3),
             data = data_judges[child %in% c(1:4)], 
             weights = no_cases / total_cases)

model8 <- lm(formula = progressive.vote ~ as.numeric(girls > 0) + factor(child) + 
               factor(republican) + age + as.numeric(religion == 4) + woman + 
               as.numeric(race == 2) + as.numeric(race == 3) + factor(circuit.1),
             data = data_judges[child %in% c(1:4)], 
             weights = no_cases / total_cases)

# 4. Format tables and save --------------------------------
covariate_labels <- c('Girls (1)', 'Girls (2)', 'Girls (3)', 'Girls (4)', 'Girls (5)',
                      'Girls (1+)', 'Republican', 'Age', 'Catholic', 'Woman', 
                      'African American', 'Hispanic')

out_model <- stargazer(model1, model2, model3, model4, model5, model6, model7, model8,
          type = 'latex', 
          dep.var.labels = 'Progressive vote (percentage)',
          column.labels = c('All judges', 'Judges with 1-4 children'),
          column.separate = c(4,4),
          dep.var.caption = '',
          covariate.labels = covariate_labels,
          model.names = TRUE,
          omit.stat = c('F', 'ser'),
          table.placement = 'h',
          omit = c('child', 'circuit.1'),
          omit.labels = c('Child fixed effects', 'Circuit fixed effects'),
          digits = 2, digits.extra = 2
          )

# Adjust size
out_model <- gsub('\\begin{tabular}', 
                  '\\begin{adjustbox}{width=1\\textwidth}\\begin{tabular}', 
                  out_model,
                  fixed = TRUE)

out_model <- gsub('\\end{tabular} ', 
                  '\\end{tabular}\\end{adjustbox}', 
                  out_model,
                  fixed = TRUE)

# Correct problem with fixed effects
out_model <- gsub('Circuit fixed effects & No & No & No & No', 
                  'Circuit fixed effects & No & No & No & Yes ', 
                  out_model,
                  fixed = TRUE)

# Add label and caption
out_model <- gsub('\\caption{}', 
                  paste0('\\caption{Main models from "Identifying Judicial Empathy: ',
                         'Does Having Daughters Cause Judges to Rule for Women\'s Issues?"}'), 
                  out_model,
                  fixed = TRUE)

out_model <- gsub('\\label{}', 
                  '\\label{replication_table}', 
                  out_model,
                  fixed = TRUE)

# Write
write(out_model, 
      file =  paste0(output, 'table4_replica.tex'))




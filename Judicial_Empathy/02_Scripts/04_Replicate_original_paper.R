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
library(MASS)
library(sandwich)

# Output directory
output <- 'Judicial_empathy/03_Output/02_Original_Paper/'

# 1. Load data ------------------------------------
data_judges <- fread('Judicial_empathy/01_Data/02_Cleaned_data/judges_replication.csv')
data_cases <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/cases_replication.csv')

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


# 5. Case data ----------------------------------------------
model1 <- glm(progressive.vote ~ as.factor(girls) + as.factor(child),
              family = 'binomial', data = data_cases[child < 5])

model2 <- glm(progressive.vote ~ as.numeric(girls > 0) + as.factor(child),
              family = 'binomial', data = data_cases[child < 5])

model3 <- glm(progressive.vote ~ as.numeric(girls > 0) + as.factor(child) + republican + 
                age + as.numeric(religion == 4) + woman + as.numeric(race == 2) +
                as.numeric(race ==3) + as.factor(circuit) + as.factor(year),
              family = 'binomial', data = data_cases[child < 5])

model4 <- glm(progressive.vote ~ as.numeric(girls > 0) + as.factor(child) + republican + 
                age + as.numeric(religion == 4) + woman + as.numeric(race == 2) +
                as.numeric(race ==3) + as.factor(circuit) + as.factor(year) + as.factor(area),
              family = 'binomial', data = data_cases[child < 5])

# Need to add clustered SEs here at the case level?
model5 <- glm(progressive.vote ~ as.numeric(girls > 0) + as.factor(child) + republican + 
                age + as.numeric(religion == 4) + woman + as.numeric(race == 2) +
                as.numeric(race ==3) + as.factor(circuit) + as.factor(year) + as.factor(area),
              family = 'binomial', data = data_cases[child < 5])

model5_CSEs <- vcovBS(x= model5, cluster = data_cases[child < 5]$V1)

# Ordered logit model
model6 <- polr(factor(vote) ~ as.numeric(girls > 0) + as.factor(child) + republican + 
                age + as.numeric(religion == 4) + woman + as.numeric(race == 2) +
                as.numeric(race ==3) + as.factor(circuit) + as.factor(year) + as.factor(area),
              data = data_cases[child < 5])


## Now: Calculating standard errors bootstrapped at the case level:
## Note: This takes about 10 minutes to run. 

## Grab the data for these two models
mod12 <- as.formula("~ progressive.vote + vote + girls +  child + republican +race + age + religion + woman + circuit + year + area + casename")
dat12 <- model.frame(mod12, data = subset(women.cases, child < 5 & child > 0))
dat12$casename <- as.character(dat12$casename)

## Create list of data.frames for each case for easy reference
casenames <- sort(unique(dat12$casename))
caselist <- lapply(casenames, function(x) dat12[which(dat12$casename == x),])
names(caselist) <- casenames

## Running the bootstraps
## Note that not all of the coefficients will be sampled in any given of the 
## bootstraps - this might result in some warnings form polr
## (the ordered logit), but is not a problem for the results

## Running the bootstraps
## Note that not all of the coefficients will be sampled in any given of the 
## bootstraps - this might result in some warnings form polr
## (the ordered logit), but is not a problem for the results

boots <- 1000
b.star11 <- matrix(NA, ncol =length(my.out11$result$coef), nrow = boots)
colnames(b.star11) <- names(my.out11$result$coef)
b.star12 <- matrix(NA, ncol =length(my.out12$result$coef), nrow = boots)
colnames(b.star12) <- names(my.out12$result$coef)
set.seed(1234)
for (b in 1:boots) {
  if ((b%%10) == 0) cat("boot: ", b, "\n")
  clust.sample <- sample(casenames, replace = TRUE)
  c.boot <- do.call(rbind,lapply(clust.sample,function(x) caselist[[x]]))
  out11.star <- glm(progressive.vote ~ I(girls>0) + as.factor(child) + republican 
                    + age + I(religion == 4) + woman + I(race == 2) + I(race == 3) + as.factor(circuit) + as.factor(year) + as.factor(area), data = c.boot, family = binomial("logit"))
  out12.star <- polr(as.factor(vote) ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year) + as.factor(area), data = c.boot)	
  b.star11[b,names(out11.star$coef)] <- out11.star$coef
  b.star12[b,names(out12.star$coef)] <- out12.star$coef
}
bcse11 <- apply(b.star11, 2, sd, na.rm=TRUE)		
bcse12 <- apply(b.star12, 2, sd, na.rm=TRUE)		
#save(list=c("bcse11", "bcse12"), file = "bootstraps.RData")
## End bootstrap code here



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
library(coin) # wilcoxon signed rank test
library(ri2)
library(survey)
library(tidyverse)

# Output directory
output <- 'Judicial_empathy/03_Output/04_Analysis/'

# Helper functions
source('Judicial_empathy/02_Scripts/00_utility.R')

# Parameters
#selected_match <- 'Judicial_empathy/03_Output/03_Matching/match10_dummy.csv'
selected_match <- 'Judicial_empathy/03_Output/03_Matching/match13_dummy.csv'

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
matches_transformed <- cast.senm(dat = data_judges_matched, 
                                 ms.arg = data_judges_matched$matches,
                                 two.outcomes=FALSE,
                                 y_col = 'progressive.vote')

FRT <- senm(y = matches_transformed$y,
            #z = matches_transformed$z, 
            z = (1-matches_transformed$z),
            mset = matches_transformed$mset, 
            gamma = 1,
            inner = 0,
            tau = 0,
            trim=Inf,
            #alternative = 'greater')
            alternative = 'less')

cat('Gamma = 1 pvalue: ', FRT$pval)

# * 2.2 Sensitivity analysis ----------------------

gammas <- seq(from = 1, to = 2, by = 0.001)
max_gamma = 1
pvals = c()
over_pval = FALSE
for (i in gammas) {
    sen <- senm(y = matches_transformed$y,
               #z = matches_transformed$z,
               z = (1-matches_transformed$z),
               mset = matches_transformed$mset, 
               gamma = i,
               inner = 0,
               tau = 0,
               trim=Inf,
               #alternative = 'greater')
               alternative = 'less')
    pvals = append(pvals, sen$pval)
    if (sen$pval > 0.05 & over_pval == FALSE) {
        max_gamma <- i
        over_pval = TRUE
        #break
    }
}
cat('Sensitivity gamma: ', max_gamma)
plot(gammas, pvals, type='l')

rm(matches_transformed)

# amplify(max_gamma, )

# * 2.3 Stability analysis ----------------------

# We have missing values for: Age, religion, race
# (Removed birth year as we did not include it as a covarariate)

# Obtain mean outcome per match and treatment group, and an indicator
# of whether any unit has a missing value for age, religion, race

## NOTE: for the 2:1 match, no units who have one of race and religion missing
## but not the other are matched!! This means that the boxplots for the 
## race and relig stability analyses are the same!!

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

# Obtain observations related to missings for scatter part of plot
missings_grouped <- 
  copy(grouped_data_judges[, .(matches, agemiss, religmiss, racemiss, outcome_diff)])
missings_grouped <- melt(missings_grouped, 
                         id.vars = c('matches', 'outcome_diff'), 
                         variable.name = 'mis_variable')
missings_grouped <- missings_grouped[value == 1]

# Assign type "non-missing observations" so it'll appear in that part of the plot
missings_grouped <- missings_grouped[, type := 'Non-missing observations']

# Plot
g <- ggplot(missing_var_differences, aes(y = outcome_diff, x = type)) + 
  geom_boxplot() +
  geom_point(data = missings_grouped, aes(x = type, y = outcome_diff),
             shape = 4) +
  #bbc_style() + 
  labs(x = '', y = 'Difference in outcomes') + 
  theme(axis.text = element_text(size=8),
        strip.text = element_text(size = 8)) + 
  facet_wrap(variable ~.) 
ggsave(filename = paste0(output, '01_stability_analysis.png'), scale = 1.4,
       units = 'in', width = 6, height = 3)

rm(missing_var_differences, missing_sub, grouped_data_judges, grouped_sub)

no_missing_age = data_judges_matched[data_judges_matched$agemiss == 0]
no_missing_age = no_missing_age %>% group_by(matches) %>% filter(n()>2) %>% ungroup()
no_missing_age <- cast.senm(dat = no_missing_age, 
                                 ms.arg = no_missing_age$matches,
                                 two.outcomes=FALSE,
                                 y_col = 'progressive.vote')
FRT <- senm(y = no_missing_age$y,
            #z = matches_transformed$z, 
            z = (1-no_missing_age$z),
            mset = no_missing_age$mset, 
            gamma = 1,
            inner = 0,
            tau = 0,
            trim=Inf,
            #alternative = 'greater')
            alternative = 'less')
FRT$pval

no_missing_relig = data_judges_matched[data_judges_matched$religmiss == 0]
no_missing_relig = no_missing_relig %>% group_by(matches) %>% filter(n()>2) %>% ungroup()
no_missing_relig <- cast.senm(dat = no_missing_relig, 
                            ms.arg = no_missing_relig$matches,
                            two.outcomes=FALSE,
                            y_col = 'progressive.vote')
FRT <- senm(y = no_missing_relig$y,
            #z = matches_transformed$z, 
            z = (1-no_missing_relig$z),
            mset = no_missing_relig$mset, 
            gamma = 1,
            inner = 0,
            tau = 0,
            trim=Inf,
            #alternative = 'greater')
            alternative = 'less')
FRT$pval

no_missing_race = data_judges_matched[data_judges_matched$racemiss == 0]
no_missing_race = no_missing_race %>% group_by(matches) %>% filter(n()>2) %>% ungroup()
no_missing_race <- cast.senm(dat = no_missing_race, 
                              ms.arg = no_missing_race$matches,
                              two.outcomes=FALSE,
                              y_col = 'progressive.vote')
FRT <- senm(y = no_missing_race$y,
            #z = matches_transformed$z, 
            z = (1-no_missing_race$z),
            mset = no_missing_race$mset, 
            gamma = 1,
            inner = 0,
            tau = 0,
            trim=Inf,
            #alternative = 'greater')
            alternative = 'less')
FRT$pval


# * 2.4 Heterogeneous effects ---------------------
# NOTE: THE BELOW CODE WILL ONLY WORK WITH 1:1 MATCHING

## NOTE: not sure if this should become avg(no_cases) for 2:1 matching
grouped_data_judges <- 
  data_judges_matched[, .(avg_outcome = mean(progressive.vote),
                          woman = sum(woman),
                          republican = sum(republican),
                          no_cases = sum(no_cases)),
                      by = .(matches, z)]
### ADDED THIS! remove unmatched group ###
grouped_data_judges = grouped_data_judges[grouped_data_judges$matches != '']

#exact_matches <-
#  grouped_data_judges[, .(woman_match = ifelse(sum(woman) == 0 | sum(woman) == 2, 1, 0),
#                          republican_match = 
#                           ifelse(sum(republican) == 0 | sum(republican) == 2, 1, 0)), 
#                      by = matches]
exact_matches <-
  grouped_data_judges[, .(woman_match = ifelse(sum(woman) == 0 | sum(woman) == 3, 1, 0),
                          republican_match = 
                            ifelse(sum(republican) == 0 | sum(republican) == 3, 1, 0)), 
                      by = matches]

# Reshape so as to calculate differences between matched groups
grouped_data_judges <- dcast(grouped_data_judges,
                             formula = matches ~ z,
                             value.var = c('woman', 'republican', 'avg_outcome', 'no_cases'))
grouped_data_judges <- grouped_data_judges[, outcome_diff := `avg_outcome_1` - `avg_outcome_0`]

# Merge exact matches
grouped_data_judges <- merge(grouped_data_judges, exact_matches, by = 'matches', all.x = TRUE)

# Sum number of cases for the pair
grouped_data_judges <- grouped_data_judges[, no_cases := no_cases_0 + no_cases_1]

# * Test for women
women_data <- 
  grouped_data_judges[woman_match == 1, .(matches, woman_0, woman_1, outcome_diff, no_cases)]
wilcox_test(
  formula = outcome_diff ~ factor(woman_0),
  data = women_data,
  conf.int = TRUE,
  distribution = 'exact',
  ties.method = 'mid-ranks' # test is insensitive to the tie method
)

# Using number of cases as weights for each pair
# wilcox_test(
#   formula = outcome_diff ~ factor(woman_0),
#   data = women_data,
#   weights = ~ no_cases,
#   distribution = 'exact',
#   ties.method = 'mid-ranks' # test is insensitive to the tie method
# )

# Using survey package
design <- svydesign(ids = ~0, data = women_data, weights = ~ no_cases)
svyranktest(
  formula = outcome_diff ~ factor(woman_0),
  test = 'wilcoxon',
  design = design
)

# * Test for republicans
republican_data <- 
  grouped_data_judges[republican_match == 1, 
                      .(matches, republican_0, republican_1, outcome_diff, no_cases)]
wilcox_test(
  formula = outcome_diff ~ factor(republican_0),
  data = republican_data,
  conf.int = TRUE,
  distribution = 'exact',
  ties.method = 'average-scores' # test is insensitive to the tie method
)

# Using number of cases as weights for each pair
wilcox_test(
  formula = outcome_diff ~ factor(republican_0),
  data = republican_data,
  weights = ~ no_cases,
  distribution = 'exact',
  ties.method = 'average-scores' # test is insensitive to the tie method
)

# Using survey package
design <- svydesign(ids = ~0, data = republican_data, weights = ~ no_cases)
svyranktest(
  formula = outcome_diff ~ factor(republican_0),
  test = 'wilcoxon',
  design = design
)

rm(women_data, republican_data, exact_matches)

# 3. Case data ------------------------------------
selected_match <- 'Judicial_empathy/03_Output/03_Matching/match10_dummy.csv'

# load judge data with new match
data_judges_matched <- fread(selected_match,
                             colClasses = c('character', rep('numeric', 44), 'factor'))

# Reincorporate outcomes
data_judges <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/judges_cleaned.csv')
data_judges_matched <- merge(data_judges_matched,
                             data_judges[, .(name, progressive.vote)], by = 'name')
rm(data_judges)

# Cleaned case data
data_cases <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/cases_cleaned.csv')
cases_matched <- merge(data_cases, data_judges_matched[, .(songerID, matches)], by = 'songerID')
cases_matched <- cases_matched[cases_matched$matches != '']

# * 3.1 Obtain residuals --------------------------

#model <- lm(progressive.vote ~ factor(year) + factor(area), 
#           data=cases_matched)
model <- glm(progressive.vote ~ factor(year) + factor(area), 
            data=cases_matched, family='binomial')

residuals <- resid(model)

# * 3.2 FRT ---------------------------------------
ranks <- rank(residuals)
cases_matched$resid_ranks <- ranks

### OUTLINE OF TEST STATISTIC ###
test_statistic <- function(grouped_data) {
    # Reshape so as to calculate differences between matched groups
    grouped_data <- dcast(grouped_data,
                           formula = matches ~ z,
                           value.var = c('weights', 'avg_resid_rank'))
    
    grouped_data <- grouped_data[, outcome_diff := `avg_resid_rank_1` - `avg_resid_rank_0`]
    
    ## WEIGHTING PROCEDURE:
    ## can set weights equal to 1 (so that each cluster is equally weighted)
    ## or weight by the number of cases in each cluster divided by the total
    #grouped_data <- grouped_data[, weighted_outcome_diff := (outcome_diff)*(weights_0+weights_1)]
    
    return(sum(grouped_data$outcome_diff))
    # return(sum(grouped_data$outcome_diff)/2) # <- this is the test stat used by senm function!
    # gives the same results
    #return(sum(grouped_data$weighted_outcome_diff))
}

test_statistic_weighted <- function(grouped_data) {
  # Reshape so as to calculate differences between matched groups
  grouped_data <- dcast(grouped_data,
                        formula = matches ~ z,
                        value.var = c('weights', 'avg_resid_rank'))
  
  grouped_data <- grouped_data[, outcome_diff := `avg_resid_rank_1` - `avg_resid_rank_0`]
  
  ## WEIGHTING PROCEDURE:
  ## Weight by the number of cases in each cluster divided by the total
  grouped_data <- grouped_data[, weighted_outcome_diff := (outcome_diff)*(weights_0+weights_1)]
  # grouped_data <- grouped_data[, weighted_outcome_diff := (outcome_diff)*(weights_0*weights_1)]
  # ^ this choice of weights minimizes the variance
  
  return(sum(grouped_data$weighted_outcome_diff))
}

grouped_data_cases <- 
  copy(cases_matched[, .(avg_resid_rank = mean(resid_ranks), n_cases = .N),
       by = .(matches, z)])

grouped_data_cases <- 
  grouped_data_cases[, weights := n_cases / sum(n_cases)]

## this specifies our randomization procedure
## within each block, one unit assigned treatment and one unit assigned control
## per "Clustered Treatment Assignments ... Observational Studies:
## "Weights ws ∝ ns1 + ns2 are particularly relevant when one suspects that the treatment
## effect may be larger in some cluster pairs than in others."
declaration = declare_ra(blocks = as.vector(grouped_data_cases$matches), 
                         block_m_each = cbind(rep(1, 37), rep(1, 37)))

# Weights of 1
conduct_ri(test_function = test_statistic, declaration = declaration, 
           sharp_hypothesis = 0, data = grouped_data_cases, 
           assignment = "z", p="upper", sims = 5000)

# Identical to above test statistic but with weights = 1/2 because lambda = 1/2 by default
cases_transformed = cast.senm(dat = grouped_data_cases, 
                              ms.arg = grouped_data_cases$matches,
                              two.outcomes=FALSE,
                              y_col = 'avg_resid_rank')

### NOTE: senm does not actually compute stratified DIM! it weights all groups equally and 
### does not divide by the total number of clusters.
## setting TonT = TRUE computes DIM but p value stays exactly the same
FRT <- senm(y = cases_transformed$y,
            z = cases_transformed$z, 
            mset = cases_transformed$mset, 
            gamma = 1,
            inner = 0,
            tau = 0,
            trim=Inf,
            alternative = 'greater')
FRT$pval

# Weighted by number of cases
conduct_ri(test_function = test_statistic_weighted, declaration = declaration, 
           sharp_hypothesis = 0, data = grouped_data_cases, 
           assignment = "z", p='upper', sims = 10000)

# * 3.3 Sensitivity analysis -------------------
gammas <- seq(from = 1, to = 2, by = 0.001)
pvals = c()
max_gamma <- 1
over_pval = FALSE
for (i in gammas) {
  sen <- senm(y = cases_transformed$y,
              z = cases_transformed$z, 
              mset = cases_transformed$mset, 
              gamma = i,
              inner = 0,
              tau = 0,
              trim=Inf,
              alternative = 'greater')
  pvals = append(pvals, sen$pval)
  if (sen$pval > 0.05 & over_pval==FALSE) {
    max_gamma <- i
    over_pval=TRUE
    #break
  }
}
plot(gammas, pvals, type='l')

cat('Sensitivity gamma: ', max_gamma)
rm(cases_transformed)
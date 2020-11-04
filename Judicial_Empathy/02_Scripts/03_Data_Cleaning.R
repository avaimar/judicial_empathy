
# 0. Working set up -----------------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(varhandle)
library(glmnet) # lasso

# 1. Load data ---------------------------------
data_cases <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_case_1.csv", check.names = TRUE)
data_judges <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_judge.csv", check.names = TRUE)

## 2. Subset cases to match method from paper -------
cases_small = subset(data_cases, area == "employment" | area == "Title IX" | area == "pregnancy" | area == "abortion" | area == "reproductive rights")
cases_small = subset(cases_small, femplaintiff == 1)
cases_small$area <- factor(cases_small$area, levels = c("abortion","employment","pregnancy","reproductive rights","Title IX"))

# 3. Judge data -------------------------------
## get number of cases for each judge
cases_per_judge <- cases_small[,.(no_cases = .N), by=name]
data_judges <- merge(data_judges, cases_per_judge, by='name')

### total number of relevant cases heard by all judges in dataset
sum(data_judges$no_cases)

## get liberal vote share (outcome)
liberal_votes = cases_small %>% 
  group_by(name) %>% 
  summarise(lib_vote_share = sum(vote == 2 | vote == 3)/n())
## this is the same as 'progressive.vote' in the dataset

### subset the data to include only those judges for whom we
### fertility data
judges_small = subset(data_judges, !is.na(girls))
nrow(judges_small)

## write initial dataset to file for replication purposes
write.csv(judges_small, 'Judicial_empathy/01_data/02_Cleaned_data/judges_replication.csv')

## number of cases heard by judges who we have fertility data about
cases_fertility = subset(cases_small, name %in% judges_small$name)
nrow(cases_fertility)

colSums(is.na(data_judges[, .(child)]))
colSums(is.na(data_judges[, .(girls)]))
colSums(is.na(data_judges[, .(sons)]))

mean_na = colMeans(is.na(judges_small))
mean_na[mean_na != 0]
## highest proportion of NAs is age (.24), followed by religion, then birth year and race

summary(judges_small)
## ranges of attributes look reasonable, doesn't look like there are any
## obvious erroneous entries

p <- ggplot(judges_small, aes(x=as.factor(circuit.1))) + 
  geom_histogram(fill = 'lightblue', stat='count')
p

## very few judges who have had more than 5 children
p2 <- ggplot(judges_small, aes(x=child)) + 
  geom_histogram(fill = 'lightblue', bins = 9)
p2

p3 <- ggplot(judges_small, aes(x=as.factor(woman))) + 
  geom_histogram(fill = 'lightblue', stat='count')
p3

## only one person in the race 4 category, 6 in race 3
## I still think this will not be an issue in analyses.
## I think it just means match might not be as good for these individuals
judges_small %>% 
  group_by(race) %>%
  summarise(no_rows = length(race))
p4 <- ggplot(judges_small, aes(x=as.factor(race))) + 
  geom_histogram(fill = 'lightblue', stat='count')
p4

p5 <- ggplot(judges_small, aes(x=age)) + 
  geom_histogram(fill = 'lightblue', bins=30)
p5

## only one instance of religions 9, 11, 16, 17, 21, 24
judges_small %>% 
  group_by(religion) %>%
  summarise(no_rows = length(religion))
p6 <- ggplot(judges_small, aes(x=as.factor(religion))) + 
  geom_histogram(fill = 'lightblue', stat='count')
p6

## if these categories were consistent, we would expect age + yearb to be around
## the same value for all judges. Perhaps this is because the data was taken from
## different sources and at different times so the age relative to different years
levels(as.factor(judges_small$age + judges_small$yearb))

# drop "yearb" column
judges_small = judges_small[, c("yearb"):=NULL]

## add indicator variables for missing values
judges_small$agemiss = as.numeric(is.na(judges_small$age))
judges_small$religmiss = as.numeric(is.na(judges_small$religion))
judges_small$racemiss = as.numeric(is.na(judges_small$race))

## convert missing values to 0s
judges_small[is.na(judges_small)] <- 0

## convert categorical attributes to binary indicators
race_indicator <- to.dummy(judges_small$race, "race")
judges_small = cbind(judges_small, race_indicator)

judges_small[judges_small$religion %in% c(9, 11, 16, 17, 21, 24)]$religion <- 9
relig_indicator <- to.dummy(judges_small$religion, "religion")
judges_small = cbind(judges_small, relig_indicator)

# For circuit, correct name first so as to not create duplicate column
setnames(judges_small, 'circuit.1', 'circuit')
circuit_indicator <- to.dummy(judges_small$circuit, "circuit")
judges_small = cbind(judges_small, circuit_indicator)

## drop non-dummy columns
#judges_small = judges_small[, c("race"):=NULL]
#judges_small = judges_small[, c("religion"):=NULL]
#judges_small = judges_small[, c("circuit.1"):=NULL]
judges_small <- judges_small[, V1 := NULL]

# Define treatment
# 1. Remove judges with no children
judges_small <- judges_small[child > 0]

# 2. Treatment: Judge has at least 1 girl
judges_small <- judges_small[, z := as.numeric(girls > 0)]

# 3. Move treatment to front
setcolorder(judges_small, c('name', 'z'))

# Write cleaned dataset
write.csv(judges_small, 'Judicial_empathy/01_data/02_Cleaned_data/judges_cleaned.csv',
          row.names = FALSE)

# 4. Case data -----------------------------------
# Get unique columns
cases_small <- cases_small[, .SD, .SDcols = unique(colnames(cases_small))]

# * 4.1 Add treatment column ------------------------
cases_small <- merge(cases_small, 
                     judges_small[, .(songerID, z)], 
                     by = 'songerID', all.x = TRUE)

# Check units with missing treatments
sum(is.na(cases_small$z))
missing_judges <- unique(cases_small[is.na(z)]$name)
length(missing_judges)

# Why are these missing treatment?
# Only 45 judges were in the judge data
sum(missing_judges %in% data_judges$name)
missing_judges_in_data <- missing_judges[missing_judges %in% data_judges$name]

# These 45 judges either have no children or no info on girls/boys
table(data_judges[name %in% missing_judges_in_data,]$child, 
      data_judges[name %in% missing_judges_in_data,]$girls)

# Therefore, we drop these units with missing treatments
cases_small <- cases_small[!is.na(z)]

# * 4.2 Check missings ---------------------------
missing_cols <- data.table(
  column = names(colMeans(is.na(cases_small))),
  missing_perc = colMeans(is.na(cases_small)))
hist(missing_cols$missing_perc)

# Drop columns with over 85% missing values
sum(missing_cols$missing_perc >= 0.85)
cases_small <- 
  cases_small[, .SD, .SDcols = missing_cols[missing_perc < 0.85,]$column]

# Notice that by dropping to columns with less than 30% missing values
# we are simply removing 245 - 223 = 22 columns
cases_small <- 
  cases_small[, .SD, .SDcols = missing_cols[missing_perc < 0.3,]$column]

# * 4.3 Drop meaningless covariates --------------------------
covariates <- colnames(cases_small)

# Drop treatment and outcome
covariates <- covariates[!(covariates %in% c('vote', 'progressive.vote', 'z'))]

# We drop the following covariates which are unique to each case
#   * casename
#   * name, name.1, capsnames
#   * pname
#   * cite
#   * docket
covariates <- covariates[!(covariates %in% c('casename', 'name', 'name.1', 
                                             'capsnames', 'pname', 'cite', 'docket'))]

# We also drop the covariates at the judge level as these will be used
# only for matching.
#   * girls, sons, republican, race, 
covariates <- covariates[!(covariates %in% c('girls', 'sons', 'woman', 'child', 'race',
                                             'republican', 'circuit', 'circuit.1', 'age',
                                             'religion', 'yearb'))]
# Note: We leave 'songerID' as an identifier to map to judge matches in analysis

# * 4.3 Use lasso to identify important covariates --------
#m1_lasso <- 
#  glmnet(x = as.matrix(cases_small[, .SD, .SDcols = covariates]),
#         y = cases_small$z,
#         family = 'binomial',
#         alpha = 1)

# * 4. Write cleaned dataset --------------------
cases_small <- cases_small[, .SD, .SDcols = c('z', 'vote', covariates)]
write.csv(cases_small, 'Judicial_empathy/01_data/02_Cleaned_data/judges_cleaned.csv',
          row.names = FALSE)

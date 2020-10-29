library(data.table)
library(ggplot2)
library(dplyr)
library(varhandle)

data_cases <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_case_1.csv", check.names = TRUE)
data_judges <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_judge.csv", check.names = TRUE)

## subset cases to match method from paper
cases_small = subset(data_cases, area == "employment" | area == "Title IX" | area == "pregnancy" | area == "abortion" | area == "reproductive rights")
cases_small = subset(cases_small, femplaintiff == 1)
cases_small$area <- factor(cases_small$area, levels = c("abortion","employment","pregnancy","reproductive rights","Title IX"))

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
